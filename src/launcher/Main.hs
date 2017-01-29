{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Concurrent.Async hiding (wait)
import           Options.Applicative      (Parser, ParserInfo, auto, execParser, fullDesc,
                                           help, helper, info, long, metavar, option,
                                           progDesc, strOption)
import           System.Directory         (getAppUserDataDirectory)
import qualified System.IO                as IO
import           System.Process           (ProcessHandle)
import qualified System.Process           as Process
import           System.Process.Internals (translate)
import           Turtle                   hiding (option, toText)
import           Universum                hiding (FilePath)

import           Control.Exception        (handle, mask_, throwIO)
import           Foreign.C.Error          (Errno (..), ePIPE)
import           GHC.IO.Exception         (IOErrorType (..), IOException (..))

data LauncherOptions = LO
    { loNode           :: Maybe Text
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
    nodeOpt = optional $ toText <$>
      strOption (long "node" <>
                 metavar "PATH" <>
                 help "Path to the node executable")
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
serverScenario :: Maybe Text -> (Text, FilePath) -> Shell ()
serverScenario nodePath upd = do
    echo "Running the updater"
    runUpdater upd
    -- TODO: somehow signal updater failure if it fails? would be nice to
    -- write it into the log, at least
    echo "Starting the node"
    (_, nodeAsync) <- spawnNode nodePath
    exitCode <- wait nodeAsync
    printf ("The node has exited with "%s%"\n") (show exitCode)
    case exitCode of
        ExitFailure 20 -> serverScenario nodePath upd
        _              -> return ()

-- | If we are on desktop, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node and the wallet.
-- * If the wallet exits with code 20, then update and restart, else quit.
clientScenario :: Maybe Text -> Text -> (Text, FilePath) -> Int -> Shell ()
clientScenario nodePath walletCmd upd nodeTimeout = do
    echo "Running the updater"
    runUpdater upd
    echo "Starting the node"
    -- I don't know why but a process started with turtle just can't be
    -- killed, so let's use 'terminateProcess' and modified 'system'
    (nodeHandle, nodeAsync) <- spawnNode nodePath
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
                 Process.terminateProcess nodeHandle
                 cancel nodeAsync
             clientScenario nodePath walletCmd upd nodeTimeout
       | otherwise -> do
             printf ("The wallet has exited with "%s%"\n") (show exitCode)
             echo "Killing the node"
             liftIO $ do
                 Process.terminateProcess nodeHandle
                 cancel nodeAsync

-- | We run the updater and delete the update file if the update was
-- successful.
runUpdater :: (Text, FilePath) -> Shell ()
runUpdater (updaterCmd, updateArchive) = do
    let cmd = updaterCmd <> " " <>
              toText (translate (toString updateArchive))
    exitCode <- shell cmd mempty
    printf ("The updater has exited with "%s%"\n") (show exitCode)
    when (exitCode == ExitSuccess) $ do
        -- this will throw an exception if the file doesn't exist but
        -- hopefully if the updater has succeeded it *does* exist
        rm updateArchive

----------------------------------------------------------------------------
-- Command generation
----------------------------------------------------------------------------

spawnNode :: Maybe Text -> Shell (ProcessHandle, Async ExitCode)
spawnNode mbNodePath = do
    appDir <- fromString <$> liftIO (getAppUserDataDirectory "Daedalus")
    -- TODO: some of code below only on Windows. The original code in Daedalus
    -- looked like this:
    --
    --     const DATA = process.env.APPDATA ||
    --                  (process.platform == 'darwin'
    --                      ? process.env.HOME + 'Library/Preferences'
    --                      : '~/.config')
    let flags = [
            "--listen", "0.0.0.0:12100",
            "--peer", "35.156.182.24:3000/MHdrsP-oPf7UWl0007QuXnLK5RD=",
            "--peer", "54.183.103.204:3000/MHdrsP-oPf7UWl0077QuXnLK5RD=",
            "--peer", "52.53.231.169:3000/MHdrsP-oPf7UWl0127QuXnLK5RD=",
            "--peer", "35.157.41.94:3000/MHdrsP-oPf7UWl0057QuXnLK5RD=",
            "--log-config", "log-config-prod.yaml",
            "--keyfile", toString (appDir </> "Secrets" </> "secret.key"),
            "--logs-prefix", toString (appDir </> "Logs"),
            "--db-path", toString (appDir </> "DB"),
            "--wallet-db-path", toString (appDir </> "Wallet"),
            "--wallet"
            ]

    let logPath = appDir </> "Logs" </> "cardano-node.log"
    logHandle <- openFile (toString logPath) AppendMode

    let nodePath = fromMaybe "cardano-node.exe" mbNodePath
    let cr = (Process.proc (toString nodePath) flags)
                 { Process.std_in  = Process.CreatePipe
                 , Process.std_out = Process.UseHandle logHandle
                 , Process.std_err = Process.UseHandle logHandle
                 }
    phvar <- liftIO newEmptyMVar
    asc <- fork (system' phvar cr mempty)
    ph <- liftIO $ takeMVar phvar
    return (ph, asc)

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

instance ToString FilePath where
    toString = toString . format fp

----------------------------------------------------------------------------
-- Turtle internals, modified to give access to process handles
----------------------------------------------------------------------------

{-
shell'
    :: MonadIO io
    => MVar ProcessHandle
    -- ^ Where to put process handle
    -> Text
    -- ^ Command line
    -> Shell Line
    -- ^ Lines of standard input
    -> io ExitCode
    -- ^ Exit code
shell' phvar cmdLine =
    system' phvar
        ( (Process.shell (toString cmdLine))
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            } )
-}

system'
    :: MonadIO io
    => MVar ProcessHandle
    -- ^ Where to put process handle
    -> Process.CreateProcess
    -- ^ Command
    -> Shell Line
    -- ^ Lines of standard input
    -> io ExitCode
    -- ^ Exit code
system' phvar p sl = liftIO (do
    let open = do
            (m, Nothing, Nothing, ph) <- Process.createProcess p
            putMVar phvar ph
            case m of
                Just hIn -> IO.hSetBuffering hIn IO.LineBuffering
                _        -> return ()
            return (m, ph)

    -- Prevent double close
    mvar <- newMVar False
    let close hdl = do
            modifyMVar_ mvar (\finalized -> do
                unless finalized (ignoreSIGPIPE (IO.hClose hdl))
                return True )
    let close' (Just hIn, ph) = do
            close hIn
            Process.terminateProcess ph
        close' (Nothing , ph) = do
            Process.terminateProcess ph

    let handle_ (Just hIn, ph) = do
            let feedIn :: (forall a. IO a -> IO a) -> IO ()
                feedIn restore =
                    restore (ignoreSIGPIPE (outhandle hIn sl)) `finally` close hIn
            mask_ (withAsyncWithUnmask feedIn (\a -> Process.waitForProcess ph <* halt a) )
        handle_ (Nothing , ph) = do
            Process.waitForProcess ph

    bracket open close' handle_ )

halt :: Async a -> IO ()
halt a = do
    m <- poll a
    case m of
        Nothing          -> cancel a
        Just (Left  msg) -> throwIO msg
        Just (Right _)   -> return ()

ignoreSIGPIPE :: IO () -> IO ()
ignoreSIGPIPE = handle (\ex -> case ex of
    IOError
        { ioe_type = ResourceVanished
        , ioe_errno = Just ioe }
        | Errno ioe == ePIPE -> return ()
    _ -> throwIO ex )
