{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Control.Concurrent.Async hiding (wait)
import           Options.Applicative      (Mod, OptionFields, Parser, ParserInfo, auto,
                                           execParser, fullDesc, help, helper, info, long,
                                           metavar, option, progDesc, short, strOption)
import           System.Directory         (getTemporaryDirectory)
import qualified System.IO                as IO
import           System.Process           (ProcessHandle)
import qualified System.Process           as Process
import           System.Timeout           (timeout)
import           System.Wlog              (lcTree)
import           Turtle                   hiding (option, toText)
import           Universum                hiding (FilePath)

-- Modules needed for the “Turtle internals” session
import           Control.Exception        (handle, mask_, throwIO)
import           Foreign.C.Error          (Errno (..), ePIPE)
import           GHC.IO.Exception         (IOErrorType (..), IOException (..))

import           Paths_cardano_sl         (version)
import           Pos.CLI                  (readLoggerConfig)
import           Pos.Reporting.Methods    (chooseLogFiles, retrieveLogFiles, sendReport)
import           Pos.ReportServer.Report  (ReportType (..))

data LauncherOptions = LO
    { loNodePath       :: !FilePath
    , loNodeArgs       :: ![Text]
    , loNodeLogConfig  :: !(Maybe FilePath)
    , loWalletPath     :: !(Maybe FilePath)
    , loWalletArgs     :: ![Text]
    , loUpdaterPath    :: !FilePath
    , loUpdaterArgs    :: ![Text]
    , loUpdateArchive  :: !(Maybe FilePath)
    , loNodeTimeoutSec :: !Int
    , loReportServer   :: !(Maybe String)
    }

optsParser :: Parser LauncherOptions
optsParser = do
    let textOption :: IsString a => Mod OptionFields String -> Parser a
        textOption = fmap fromString . strOption

    -- Node-related args
    loNodePath <- textOption $
        long    "node" <>
        help    "Path to the node executable" <>
        metavar "PATH"
    loNodeArgs <- many $ textOption $
        short   'n' <>
        help    "An argument to be passed to the node" <>
        metavar "ARG"
    loNodeLogConfig <- optional $ textOption $
        long    "node-log-config" <>
        help    "Path to log config that will be used by the node" <>
        metavar "PATH"

    -- Wallet-related args
    loWalletPath <- optional $ textOption $
        long    "wallet" <>
        help    "Path to the wallet executable" <>
        metavar "PATH"
    loWalletArgs <- many $ textOption $
        short   'w' <>
        help    "An argument to be passed to the wallet" <>
        metavar "ARG"

    -- Update-related args
    loUpdaterPath <- textOption $
        long    "updater" <>
        help    "Path to the updater executable" <>
        metavar "PATH"
    loUpdaterArgs <- many $ textOption $
        short   'u' <>
        help    "An argument to be passed to the updater" <>
        metavar "ARG"
    loUpdateArchive <- optional $ textOption $
        long    "update-archive" <>
        help    "Path to the update archive (will be passed to the updater)" <>
        metavar "PATH"

    -- Other args
    loNodeTimeoutSec <- option auto $
        long    "node-timeout" <>
        help    "How much to wait for the node to exit before killing it" <>
        metavar "SEC"
    loReportServer <- optional $ strOption $
        long    "report-server" <>
        help    "Where to send logs in case of failure" <>
        metavar "URL"

    pure LO{..}

optsInfo :: ParserInfo LauncherOptions
optsInfo = info (helper <*> optsParser) $
    fullDesc <> progDesc "Tool to launch Cardano SL"

main :: IO ()
main = do
    LO {..} <- execParser optsInfo
    let realNodeArgs = case loNodeLogConfig of
            Nothing -> loNodeArgs
            Just lc -> loNodeArgs ++ ["--log-config", toText lc]
    sh $ case loWalletPath of
             Nothing -> do
                 echo "Running in the server scenario"
                 serverScenario
                     loNodeLogConfig
                     (loNodePath, realNodeArgs)
                     (loUpdaterPath, loUpdaterArgs, loUpdateArchive)
                     loReportServer
             Just wpath -> do
                 echo "Running in the client scenario"
                 clientScenario
                     loNodeLogConfig
                     (loNodePath, realNodeArgs)
                     (wpath, loWalletArgs)
                     (loUpdaterPath, loUpdaterArgs, loUpdateArchive)
                     loNodeTimeoutSec
                     loReportServer

-- | If we are on server, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node.
-- * If it exits with code 20, then update and restart, else quit.
serverScenario
    :: Maybe FilePath                      -- ^ Logger config
    -> (FilePath, [Text])                  -- ^ Node, its args
    -> (FilePath, [Text], Maybe FilePath)  -- ^ Updater, args, the update .tar
    -> Maybe String                        -- ^ Report server
    -> Shell ()
serverScenario logConf node updater report = do
    runUpdater updater
    -- TODO: the updater, too, should create a log if it fails
    (_, nodeAsync, nodeLog) <- spawnNode node
    exitCode <- wait nodeAsync
    printf ("The node has exited with "%s%"\n") (show exitCode)
    if exitCode == ExitFailure 20
        then serverScenario logConf node updater report
        else whenJust report $ \repServ -> do
                 printf ("Sending logs to "%s%"\n") (toText repServ)
                 reportNodeCrash exitCode logConf repServ nodeLog

-- | If we are on desktop, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node and the wallet.
-- * If the wallet exits with code 20, then update and restart, else quit.
--
clientScenario
    :: Maybe FilePath                      -- ^ Logger config
    -> (FilePath, [Text])                  -- ^ Node, its args
    -> (FilePath, [Text])                  -- ^ Wallet, args
    -> (FilePath, [Text], Maybe FilePath)  -- ^ Updater, args, the update .tar
    -> Int                                 -- ^ Node timeout, in seconds
    -> Maybe String                        -- ^ Report server
    -> Shell ()
clientScenario logConf node wallet updater nodeTimeout report = do
    runUpdater updater
    -- I don't know why but a process started with turtle just can't be
    -- killed, so let's use 'terminateProcess' and modified 'system'
    (nodeHandle, nodeAsync, nodeLog) <- spawnNode node
    walletAsync <- fork (runWallet wallet)
    (someAsync, exitCode) <- liftIO $ waitAny [nodeAsync, walletAsync]
    if | someAsync == nodeAsync -> do
             printf ("The node has exited with "%s%"\n") (show exitCode)
             whenJust report $ \repServ -> do
                 printf ("Sending logs to "%s%"\n") (toText repServ)
                 reportNodeCrash exitCode logConf repServ nodeLog
             echo "Waiting for the wallet to die"
             void $ wait walletAsync
       | exitCode == ExitFailure 20 -> do
             echo "The wallet has exited with code 20"
             printf ("Killing the node in "%d%" seconds\n") nodeTimeout
             sleep (fromIntegral nodeTimeout)
             echo "Killing the node now"
             liftIO $ do
                 Process.terminateProcess nodeHandle
                 cancel nodeAsync
             clientScenario logConf node wallet updater nodeTimeout report
       | otherwise -> do
             printf ("The wallet has exited with "%s%"\n") (show exitCode)
             -- TODO: does the wallet have some kind of log?
             echo "Killing the node"
             liftIO $ do
                 Process.terminateProcess nodeHandle
                 cancel nodeAsync

-- | We run the updater and delete the update file if the update was
-- successful.
runUpdater :: (FilePath, [Text], Maybe FilePath) -> Shell ()
runUpdater (path, args, updateArchive) = do
    exists <- testfile path
    if not exists then
        printf ("The updater at "%fp%" doesn't exist, skipping the update\n")
               path
    else do
        echo "Running the updater"
        let args' = args ++ maybe [] (one . toText) updateArchive
        exitCode <- proc (toText path) args' mempty
        printf ("The updater has exited with "%s%"\n") (show exitCode)
        when (exitCode == ExitSuccess) $ do
            -- this will throw an exception if the file doesn't exist but
            -- hopefully if the updater has succeeded it *does* exist
            whenJust updateArchive rm

----------------------------------------------------------------------------
-- Running stuff
----------------------------------------------------------------------------

spawnNode
    :: (FilePath, [Text])
    -> Shell (ProcessHandle, Async ExitCode, FilePath)
spawnNode (path, args) = do
    echo "Starting the node"
    (logPath, logHandle) <- do
        tempdir <- liftIO (fromString <$> getTemporaryDirectory)
        mktemp tempdir "cardano-node-output.log"
    liftIO $ IO.hSetBuffering logHandle IO.LineBuffering
    let cr = (Process.proc (toString path) (map toString args))
                 { Process.std_in  = Process.CreatePipe
                 , Process.std_out = Process.UseHandle logHandle
                 , Process.std_err = Process.UseHandle logHandle
                 }
    phvar <- liftIO newEmptyMVar
    asc <- fork (system' phvar cr mempty)
    mbPh <- liftIO $ timeout 5000000 (takeMVar phvar)
    case mbPh of
        Nothing -> panic "couldn't run the node (it didn't start after 5s)"
        Just ph -> return (ph, asc, logPath)

runWallet :: (FilePath, [Text]) -> IO ExitCode
runWallet (path, args) = do
    echo "Starting the wallet"
    proc (toText path) args mempty

----------------------------------------------------------------------------
-- Working with the report server
----------------------------------------------------------------------------

-- | Send logs (and block until they're sent).
--
-- TODO: logs should be sent asynchronously. This would probably require some
-- changes in the logging mechanism itself (e.g. to ensure that the logs
-- aren't deleted before we have sent them).
--
-- ...Or maybe we don't care because we don't restart anything after sending
-- logs (and so the user never actually sees the process or waits for it).
reportNodeCrash
    :: MonadIO m
    => ExitCode        -- ^ Exit code of the node
    -> Maybe FilePath  -- ^ Path to the logger config
    -> String          -- ^ URL of the server
    -> FilePath        -- ^ Path to the stdout log
    -> m ()
reportNodeCrash exitCode logConfPath reportServ logPath = liftIO $ do
    logConfig <- readLoggerConfig (toString <$> logConfPath)
    let logFileNames = map snd $ retrieveLogFiles $ logConfig ^. lcTree
    -- TODO: we don't want to send all logs;
    -- see Pos.Reporting.Methods.sendReportNode
    logFiles <-
        (toString logPath :) <$>
        concatMapM chooseLogFiles (filter (const True) logFileNames)
    let ec = case exitCode of
            ExitSuccess   -> 0
            ExitFailure n -> n
    sendReport logFiles (RCrash ec) "cardano-node" version reportServ

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

instance ToString FilePath where
    toString = toString . format fp

instance ToText FilePath where
    toText = format fp

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
            (m, _, _, ph) <- Process.createProcess p
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
