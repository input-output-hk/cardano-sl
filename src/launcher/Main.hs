{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

import           Control.Concurrent.Async hiding (wait)
import           Data.IORef
import           Options.Applicative      (Parser, ParserInfo, auto, execParser, fullDesc,
                                           help, helper, info, long, metavar, option,
                                           progDesc, strOption)
import qualified System.IO                as IO
import qualified System.Process           as Process
import           System.Process.Internals (translate)
import           Turtle                   hiding (option, toText)
import           Universum                hiding (FilePath)

import           Control.Exception        (handle, mask_, throwIO)
import           Foreign.C.Error          (Errno (..), ePIPE)
import           GHC.IO.Exception         (IOErrorType (..), IOException (..))

data LauncherOptions = LO
    { loNode           :: Text
    , loWallet         :: Maybe Text
    , loUpdater        :: Text
    , loUpdateArchive  :: FilePath
    , loNodeTimeoutSec :: Int
    }

optsParser :: Parser LauncherOptions
optsParser = LO <$> nodeOpt
                <*> walletOpt
                <*> updaterOpt
                <*> updateArchiveOpt
                <*> nodeTimeoutOpt
  where
    nodeOpt = toText <$>
      strOption (long "node" <>
                 metavar "CMD" <>
                 help "Command that starts the node")
    walletOpt = optional $ toText <$>
      strOption (long "wallet" <>
                 metavar "CMD" <>
                 help "Command that starts the wallet")
    updaterOpt = toText <$>
      strOption (long "updater" <>
                 metavar "CMD" <>
                 help "Command that starts the updater")
    updateArchiveOpt = fromString <$>
      strOption (long "update-archive" <>
                 metavar "PATH" <>
                 help "Contents of the update")
    nodeTimeoutOpt =
      option auto (long "node-timeout" <>
                   metavar "SEC" <>
                   help "How much to wait for the node to exit \
                        \before killing it")

optsInfo :: ParserInfo LauncherOptions
optsInfo = info (helper <*> optsParser) $
    fullDesc `mappend` progDesc "Tool to launch Cardano SL"

main :: IO ()
main = do
    LO {..} <- execParser optsInfo
    sh $ case loWallet of
        Nothing -> serverScenario loNode (loUpdater, loUpdateArchive)
        Just wp -> clientScenario loNode wp (loUpdater, loUpdateArchive)
                                  loNodeTimeoutSec

-- | If we are on server, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node.
-- * If it exits with code 20, then update and restart, else quit.
serverScenario :: Text -> (Text, FilePath) -> Shell ()
serverScenario nodeCmd upd = do
    echo "Running the updater"
    runUpdater upd
    -- TODO: somehow signal updater failure if it fails? would be nice to
    -- write it into the log, at least
    echo "Starting the node"
    exitCode <- shell nodeCmd mempty
    printf ("The node has exited with "%s%"\n") (show exitCode)
    case exitCode of
        ExitFailure 20 -> serverScenario nodeCmd upd
        _              -> return ()

-- | If we are on desktop, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node and the wallet.
-- * If the wallet exits with code 20, then update and restart, else quit.
clientScenario :: Text -> Text -> (Text, FilePath) -> Int -> Shell ()
clientScenario nodeCmd walletCmd upd nodeTimeout = do
    echo "Running the updater"
    runUpdater upd
    echo "Starting the node"
    nodeHandleRef <- liftIO $
        newIORef (panic "nodeHandleRef: node wasn't started")
    -- I don't know why but a process started with turtle just can't be
    -- killed, so let's use modified shell' and terminateProcess
    nodeAsync <- fork (shell' nodeHandleRef nodeCmd mempty)
    echo "Starting the wallet"
    walletAsync <- fork (shell walletCmd mempty)
    (someAsync, exitCode) <- liftIO $ waitAny [nodeAsync, walletAsync]
    if | someAsync == nodeAsync -> do
             printf ("The node has exited with "%s%"\n") (show exitCode)
             echo "Waiting for the wallet to die"
             void $ wait walletAsync
       | exitCode == ExitFailure 20 -> do
             echo "The wallet has exited with code 20"
             printf ("Killing the node in "%d%" seconds") nodeTimeout
             sleep (fromIntegral nodeTimeout)
             echo "Killing the node now"
             liftIO $ do
                 Process.terminateProcess =<< readIORef nodeHandleRef
                 cancel nodeAsync
             clientScenario nodeCmd walletCmd upd nodeTimeout
       | otherwise -> do
             printf ("The wallet has exited with "%s%"\n") (show exitCode)
             echo "Killing the node"
             liftIO $ do
                 Process.terminateProcess =<< readIORef nodeHandleRef
                 cancel nodeAsync

-- | We run the updater and delete the update file if the update was
-- successful.
runUpdater :: (Text, FilePath) -> Shell ()
runUpdater (updaterCmd, updateArchive) = do
    let cmd = updaterCmd <> " " <>
              toText (translate (toString (format fp updateArchive)))
    exitCode <- shell cmd mempty
    printf ("The updater has exited with "%s%"\n") (show exitCode)
    when (exitCode == ExitSuccess) $ do
        -- this will throw an exception if the file doesn't exist but
        -- hopefully if the updater has succeeded it *does* exist
        rm updateArchive

----------------------------------------------------------------------------
-- Turtle internals, modified to give access to process handles
----------------------------------------------------------------------------

shell'
    :: MonadIO io
    => IORef Process.ProcessHandle
    -- ^ Where to put process handle
    -> Text
    -- ^ Command line
    -> Shell Line
    -- ^ Lines of standard input
    -> io ExitCode
    -- ^ Exit code
shell' ref cmdLine =
    system' ref
        ( (Process.shell (toString cmdLine))
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            } )

system'
    :: MonadIO io
    => IORef Process.ProcessHandle
    -- ^ Where to put process handle
    -> Process.CreateProcess
    -- ^ Command
    -> Shell Line
    -- ^ Lines of standard input
    -> io ExitCode
    -- ^ Exit code
system' ref p s = liftIO (do
    let open = do
            (m, Nothing, Nothing, ph) <- Process.createProcess p
            writeIORef ref ph
            case m of
                Just hIn -> IO.hSetBuffering hIn IO.LineBuffering
                _        -> return ()
            return (m, ph)

    -- Prevent double close
    mvar <- newMVar False
    let close handle = do
            modifyMVar_ mvar (\finalized -> do
                unless finalized (ignoreSIGPIPE (IO.hClose handle))
                return True )
    let close' (Just hIn, ph) = do
            close hIn
            Process.terminateProcess ph
        close' (Nothing , ph) = do
            Process.terminateProcess ph

    let handle (Just hIn, ph) = do
            let feedIn :: (forall a. IO a -> IO a) -> IO ()
                feedIn restore =
                    restore (ignoreSIGPIPE (outhandle hIn s)) `finally` close hIn
            mask_ (withAsyncWithUnmask feedIn (\a -> Process.waitForProcess ph <* halt a) )
        handle (Nothing , ph) = do
            Process.waitForProcess ph

    bracket open close' handle )

halt :: Async a -> IO ()
halt a = do
    m <- poll a
    case m of
        Nothing        -> cancel a
        Just (Left  e) -> throwIO e
        Just (Right _) -> return ()

ignoreSIGPIPE :: IO () -> IO ()
ignoreSIGPIPE = handle (\e -> case e of
    IOError
        { ioe_type = ResourceVanished
        , ioe_errno = Just ioe }
        | Errno ioe == ePIPE -> return ()
    _ -> throwIO e )
