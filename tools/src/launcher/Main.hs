{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

import           Universum

import           Control.Concurrent (modifyMVar_)
import           Control.Concurrent.Async.Lifted.Safe (Async, async, cancel, poll, wait, waitAny,
                                                       withAsync, withAsyncWithUnmask)
import           Control.Lens (makeLensesWith)
import qualified Data.ByteString.Lazy as BS.L
import           Data.List (isSuffixOf)
import           Data.Maybe (isNothing)
import qualified Data.Text.IO as T
import           Data.Time.Units (Second, convertUnit)
import           Data.Version (showVersion)
import           Formatting (int, sformat, shown, stext, (%))
import qualified NeatInterpolation as Q (text)
import           Options.Applicative (Mod, OptionFields, Parser, auto, execParser, footerDoc,
                                      fullDesc, header, help, helper, info, infoOption, long,
                                      metavar, option, progDesc, short, strOption, switch)
import           System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import           System.Environment (getExecutablePath)
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))
import qualified System.IO as IO
import           System.Process (ProcessHandle, waitForProcess)
import qualified System.Process as Process
import           System.Timeout (timeout)
import           System.Wlog (logError, logInfo, logNotice, logWarning)
import qualified System.Wlog as Log
import           Text.PrettyPrint.ANSI.Leijen (Doc)

#ifdef mingw32_HOST_OS
import qualified System.IO.Silently as Silently
#endif

#ifndef mingw32_HOST_OS
import           System.Posix.Signals (sigKILL, signalProcess)
import qualified System.Process.Internals as Process
#endif

-- Modules needed for system'
import           Control.Exception.Safe (handle, mask_, tryAny)
import           Foreign.C.Error (Errno (..), ePIPE)
import           GHC.IO.Exception (IOErrorType (..), IOException (..))

import           Paths_cardano_sl (version)
import           Pos.Client.CLI (configurationOptionsParser, readLoggerConfig)
import           Pos.Core (HasConfiguration, Timestamp (..))
import           Pos.DB.Block (dbGetSerBlockRealDefault, dbGetSerUndoRealDefault,
                               dbPutSerBlundRealDefault)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..))
import           Pos.DB.Rocks (NodeDBs, closeNodeDBs, dbDeleteDefault, dbGetDefault,
                               dbIterSourceDefault, dbPutDefault, dbWriteBatchDefault, openNodeDBs)
import           Pos.Launcher (HasConfigurations, withConfigurations)
import           Pos.Launcher.Configuration (ConfigurationOptions (..))
import           Pos.Reporting.Methods (compressLogs, retrieveLogFiles, sendReport)
import           Pos.ReportServer.Report (ReportType (..))
import           Pos.Update (installerHash)
import           Pos.Update.DB.Misc (affirmUpdateInstalled)
import           Pos.Util (HasLens (..), directory, postfixLFields, sleep)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)

data LauncherOptions = LO
    { loNodePath            :: !FilePath
    , loNodeArgs            :: ![Text]
    , loNodeDbPath          :: !FilePath
    , loNodeLogConfig       :: !(Maybe FilePath)
    , loNodeLogPath         :: !(Maybe FilePath)
    , loWalletPath          :: !(Maybe FilePath)
    , loWalletArgs          :: ![Text]
    , loWalletLogging       :: !Bool
    , loWalletLogPath       :: !(Maybe FilePath)
    , loUpdaterPath         :: !FilePath
    , loUpdaterArgs         :: ![Text]
    , loUpdateArchive       :: !(Maybe FilePath)
    , loUpdateWindowsRunner :: !(Maybe FilePath)
    , loNodeTimeoutSec      :: !Int
    , loReportServer        :: !(Maybe String)
    , loConfiguration       :: !ConfigurationOptions
    -- | Launcher logs will be written into this directory (as well as to
    -- console, except on Windows where we don't output anything to console
    -- because it crashes).
    , loLauncherLogsPrefix  :: !(Maybe FilePath)
    }

-- | The concrete monad where everything happens
type M a = (HasConfigurations, HasCompileInfo) => Log.LoggerNameBox IO a

-- | Executable can be node, wallet or updater
data Executable = EWallet | ENode | EUpdater

-- | This datatype holds values for either node or wallet
--   Node/wallet path, args, log path
data NodeData = NodeData
    { ndPath    :: !FilePath
    , ndArgs    :: ![Text]
    , ndLogPath :: Maybe FilePath }

-- | Updater path, args, windows runner path, archive path
data UpdaterData = UpdaterData
    { udPath        :: !FilePath
    , udArgs        :: ![Text]
    , udWindowsPath :: Maybe FilePath
    , udArchivePath :: Maybe FilePath
    }

optionsParser :: Parser LauncherOptions
optionsParser = do
    let textOption :: IsString a => Mod OptionFields String -> Parser a
        textOption = fmap fromString . strOption

    -- Node-related args
    loNodePath <- textOption $
        long    "node" <>
        help    "Path to the node executable." <>
        metavar "PATH"
    loNodeArgs <- many $ textOption $
        short   'n' <>
        help    "An argument to be passed to the node." <>
        metavar "ARG"
    loNodeDbPath <- strOption $
        long    "db-path" <>
        metavar "FILEPATH" <>
        help    "Path to directory with all DBs used by the node."
    loNodeLogConfig <- optional $ textOption $
        long    "node-log-config" <>
        help    "Path to log config that will be used by the node." <>
        metavar "PATH"
    loNodeLogPath <- optional $ textOption $
        long    "node-log-path" <>
        help    "File where node stdout/err will be redirected " <>
        metavar "PATH"

    -- Wallet-related args
    loWalletPath <- optional $ textOption $
        long    "wallet" <>
        help    "Path to the wallet frontend executable (e. g. Daedalus)." <>
        metavar "PATH"
    loWalletArgs <- many $ textOption $
        short   'w' <>
        help    "An argument to be passed to the wallet frontend executable." <>
        metavar "ARG"
    loWalletLogging <- switch $
        long    "wlogging" <>
        help    "Bool that determines if wallet should log to stdout"

    loWalletLogPath <- optional $ textOption $
        long    "wallet-log-path" <>
        help    "File where wallet stdout/err will be redirected " <>
        metavar "PATH"
    -- Update-related args
    loUpdaterPath <- textOption $
        long    "updater" <>
        help    "Path to the updater executable." <>
        metavar "PATH"
    loUpdaterArgs <- many $ textOption $
        short   'u' <>
        help    "An argument to be passed to the updater." <>
        metavar "ARG"
    loUpdateArchive <- optional $ textOption $
        long    "update-archive" <>
        help    "Path to the update archive, it will be passed to the updater." <>
        metavar "PATH"
    loUpdateWindowsRunner <- optional $ textOption $
        long    "updater-windows-runner" <>
        help    "Path to write the Windows batch file executing updater" <>
        metavar "PATH"

    -- Other args
    loNodeTimeoutSec <- option auto $
        long    "node-timeout" <>
        help    ("How much to wait for the node to exit before killing it " <>
                 "(and then how much to wait after that).") <>
        metavar "SEC"
    loReportServer <- optional $ strOption $
        long    "report-server" <>
        help    "Where to send logs in case of failure." <>
        metavar "URL"
    loLauncherLogsPrefix <- optional $ strOption $
        long    "launcher-logs-prefix" <>
        help    "Where to put launcher logs (def: console only)." <>
        metavar "DIR"

    loConfiguration <- configurationOptionsParser

    pure LO{..}

getLauncherOptions :: IO LauncherOptions
getLauncherOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> optionsParser) $
        fullDesc <> progDesc ""
                 <> header "Tool to launch Cardano SL."
                 <> footerDoc usageExample

    versionOption = infoOption
        ("cardano-launcher-" <> showVersion version)
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = (Just . fromString @Doc . toString @Text) [Q.text|
Command example:

  stack exec -- cardano-launcher                            \
    --node binaries_v000/cardano-node                       \
    --node-log-config scripts/log-templates/log-config.yaml \
    -n "--update-server"                                    \
    -n "http://localhost:3001"                              \
    -n "--update-latest-path"                               \
    -n "updateDownloaded.tar"                               \
    -n "--listen"                                           \
    -n "127.0.0.1:3004"                                     \
    -n "--kademlia-id"                                      \
    -n "a_P8zb6fNP7I2H54FtGuhqxaMDAwMDAwMDAwMDAwMDA="       \
    -n "--rebuild-db"                                       \
    -n "--wallet"                                           \
    -n "--web-port"                                         \
    -n 8080                                                 \
    -n "--wallet-port"                                      \
    -n 8090                                                 \
    -n "--wallet-rebuild-db"                                \
    --updater cardano-updater                               \
    -u "dir"                                                \
    -u "binaries_v000"                                      \
    --node-timeout 5                                        \
    --update-archive updateDownloaded.tar|]

data LauncherModeContext = LauncherModeContext { lmcNodeDBs :: NodeDBs }

makeLensesWith postfixLFields ''LauncherModeContext

type LauncherMode = ReaderT LauncherModeContext IO

instance HasLens NodeDBs LauncherModeContext NodeDBs where
    lensOf = lmcNodeDBs_L

instance HasConfiguration => MonadDBRead LauncherMode where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault

instance HasConfiguration => MonadDB LauncherMode where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
    dbPutSerBlund = dbPutSerBlundRealDefault

newtype NodeDbPath = NodeDbPath FilePath

bracketNodeDBs :: NodeDbPath -> (NodeDBs -> IO a) -> IO a
bracketNodeDBs (NodeDbPath dbPath) = bracket (openNodeDBs False dbPath) closeNodeDBs

main :: IO ()
main =
  withCompileInfo $(retrieveCompileTimeInfo) $
#ifdef mingw32_HOST_OS
  -- We don't output anything to console on Windows because on Windows the
  -- launcher is considered a “GUI application” and so stdout and stderr
  -- don't even exist.
  Silently.hSilence [stdout, stderr] $
#endif
  do
    LO {..} <- getLauncherOptions
    let realNodeArgs = addConfigurationOptions loConfiguration $
            case loNodeLogConfig of
                Nothing -> loNodeArgs
                Just lc -> loNodeArgs ++ ["--log-config", toText lc]
    Log.setupLogging Nothing $
        Log.productionB
            & Log.lcTermSeverityOut .~ Just Log.debugPlus
            & Log.lcLogsDirectory .~ loLauncherLogsPrefix
            & Log.lcTree %~ case loLauncherLogsPrefix of
                  Nothing ->
                      identity
                  Just _  ->
                      set Log.ltFiles [Log.HandlerWrap "launcher" Nothing] .
                      set Log.ltSeverity (Just Log.debugPlus)
    Log.usingLoggerName "launcher" $
        withConfigurations loConfiguration $
        case loWalletPath of
            Nothing -> do
                logNotice "LAUNCHER STARTED"
                logInfo "Running in the server scenario"
                serverScenario
                    (NodeDbPath loNodeDbPath)
                    loNodeLogConfig
                    (NodeData loNodePath realNodeArgs loNodeLogPath)
                    (UpdaterData
                        loUpdaterPath loUpdaterArgs loUpdateWindowsRunner loUpdateArchive)
                    loReportServer
            Just wpath -> do
                logNotice "LAUNCHER STARTED"
                logInfo "Running in the client scenario"
                clientScenario
                    (NodeDbPath loNodeDbPath)
                    loNodeLogConfig
                    (NodeData loNodePath realNodeArgs loNodeLogPath)
                    (NodeData wpath loWalletArgs loWalletLogPath)
                    (UpdaterData
                        loUpdaterPath loUpdaterArgs loUpdateWindowsRunner loUpdateArchive)
                    loNodeTimeoutSec
                    loReportServer
                    loWalletLogging
  where
    -- We propagate configuration options to the node executable,
    -- because we almost certainly want to use the same configuration
    -- and don't want to pass the same options twice.  However, if
    -- user passes these options to the node explicitly, then we leave
    -- their choice. It doesn't cover all cases
    -- (e. g. `--system-start=10`), but it's better than nothing.
    addConfigurationOptions :: ConfigurationOptions -> [Text] -> [Text]
    addConfigurationOptions (ConfigurationOptions path key systemStart seed) =
        addConfFileOption path .
        addConfKeyOption key .
        addSystemStartOption systemStart .
        addSeedOption seed

    addConfFileOption filePath =
        maybeAddOption "--configuration-file" (toText filePath)
    addConfKeyOption key = maybeAddOption "--configuration-key" key
    addSystemStartOption =
        maybe identity (maybeAddOption "--system-start" . timestampToText)
    addSeedOption =
        maybe identity (maybeAddOption "--configuration-seed" . show)

    maybeAddOption :: Text -> Text -> [Text] -> [Text]
    maybeAddOption optionName optionValue options
        | optionName `elem` options = options
        | otherwise = optionName : optionValue : options

    timestampToText (Timestamp ts) =
        pretty @Integer $ fromIntegral $ convertUnit @_ @Second ts

-- | If we are on server, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node.
-- * If it exits with code 20, then update and restart, else quit.
serverScenario
    :: NodeDbPath
    -> Maybe FilePath     -- ^ Logger config
    -> NodeData           -- ^ Node, args, log path
    -> UpdaterData        -- ^ Updater, args, updater runner, archive path
    -> Maybe String       -- ^ Report server
    -> M ()
serverScenario ndbp logConf node updater report = do
    runUpdater ndbp updater
    -- TODO: the updater, too, should create a log if it fails
    (_, nodeAsync) <- spawnNode node False
    exitCode <- wait nodeAsync
    if exitCode == ExitFailure 20 then do
        logNotice $ sformat ("The node has exited with "%shown) exitCode
        serverScenario ndbp logConf node updater report
    else do
        logWarning $ sformat ("The node has exited with "%shown) exitCode
        whenJust report $ \repServ -> do
            logInfo $ sformat ("Sending logs to "%stext) (toText repServ)
            reportNodeCrash exitCode logConf repServ

-- | If we are on desktop, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node and the wallet.
-- * If the wallet exits with code 20, then update and restart, else quit.
clientScenario
    :: NodeDbPath
    -> Maybe FilePath    -- ^ Logger config
    -> NodeData          -- ^ Node, args, wallet log path
    -> NodeData          -- ^ Wallet, args, wallet log path
    -> UpdaterData       -- ^ Updater, args, updater runner, archive path
    -> Int               -- ^ Node timeout, in seconds
    -> Maybe String      -- ^ Report server
    -> Bool              -- ^ Wallet logging
    -> M ()
clientScenario ndbp logConf node wallet updater nodeTimeout report walletLog = do
    runUpdater ndbp updater
    let doesWalletLogToConsole = isNothing (ndLogPath wallet) && walletLog
    (nodeHandle, nodeAsync) <- spawnNode node doesWalletLogToConsole
    walletAsync <- async (runWallet walletLog wallet (ndLogPath node))
    (someAsync, exitCode) <- waitAny [nodeAsync, walletAsync]
    let restart = clientScenario ndbp logConf node wallet updater nodeTimeout report walletLog
    if | someAsync == nodeAsync -> do
             logWarning $ sformat ("The node has exited with "%shown) exitCode
             whenJust report $ \repServ -> do
                 logInfo $ sformat ("Sending logs to "%stext) (toText repServ)
                 reportNodeCrash exitCode logConf repServ
             logInfo "Waiting for the wallet to die"
             walletExitCode <- wait walletAsync
             logInfo $ sformat ("The wallet has exited with "%shown) walletExitCode
             when (walletExitCode == ExitFailure 20) $
                 case exitCode of
                     ExitSuccess{} -> restart
                     ExitFailure{} ->
                         -- -- Commented out because shutdown is broken and node
                         -- -- returns non-zero codes even for valid scenarios (CSL-1855)
                         -- TL.putStrLn $
                         --   "The wallet has exited with code 20, but\
                         --   \ we won't update due to node crash"
                         restart -- remove this after CSL-1855
       | exitCode == ExitFailure 20 -> do
             logNotice "The wallet has exited with code 20"
             logInfo $ sformat ("Killing the node in "%int%" seconds") nodeTimeout
             sleep (fromIntegral nodeTimeout)
             killNode nodeHandle nodeAsync
             restart
       | otherwise -> do
             logWarning $ sformat ("The wallet has exited with "%shown) exitCode
             -- TODO: does the wallet have some kind of log?
             killNode nodeHandle nodeAsync
  where
    killNode nodeHandle nodeAsync = do
        logInfo "Killing the node"
        liftIO (tryAny (Process.terminateProcess nodeHandle)) >>= \case
            Right _ -> pass
            Left ex -> logError $ "'terminateProcess' failed: " <> show ex
        cancel nodeAsync
        -- Give the node some time to die, then complain if it hasn't
        nodeExitCode <- liftIO $
            timeout (fromIntegral nodeTimeout) $
            Process.waitForProcess nodeHandle
        whenNothing_ nodeExitCode $ do
            logWarning "The node didn't die after 'terminateProcess'"
            maybeTrySIGKILL nodeHandle

-- | We run the updater and delete the update file if the update was
-- successful.
runUpdater :: NodeDbPath -> UpdaterData -> M ()
runUpdater ndbp ud = do
    let path = udPath ud
        args = udArgs ud
        runnerPath = udWindowsPath ud
        mUpdateArchivePath = udArchivePath ud
    whenM (liftIO (doesFileExist path)) $ do
        logNotice "Running the updater"
        let args' = args ++ maybe [] (one . toText) mUpdateArchivePath
        exitCode <- case runnerPath of
            Nothing -> runUpdaterProc path args'
            Just rp -> do
                -- Write the bat script and pass it the updater with all args
                writeWindowsUpdaterRunner rp
                -- The script will terminate this updater so this function shouldn't return
                runUpdaterProc rp ((toText path):args')
        case exitCode of
            ExitSuccess -> do
                logInfo "The updater has exited successfully"
                -- this will throw an exception if the file doesn't exist but
                -- hopefully if the updater has succeeded it *does* exist
                whenJust mUpdateArchivePath $ \updateArchivePath -> liftIO $ do
                    let affirmInstalled = do
                            updateArchive <- BS.L.readFile updateArchivePath
                            bracketNodeDBs ndbp $ \lmcNodeDBs ->
                                usingReaderT LauncherModeContext{..} $
                                affirmUpdateInstalled (installerHash updateArchive)
                        removeInstaller = removeFile updateArchivePath
                    -- Even if we fail to affirm that update was
                    -- installed, we still want to remove installer to
                    -- avoid infinite loop. If we don't remove it, we
                    -- will launch it again and again.
                    affirmInstalled `finally` removeInstaller
            ExitFailure code ->
                logWarning $ sformat ("The updater has failed (exit code "%int%")") code

runUpdaterProc :: HasConfigurations => FilePath -> [Text] -> M ExitCode
runUpdaterProc path args = liftIO $ do
    let cr = createProc Process.CreatePipe path args
    phvar <- newEmptyMVar
    system' phvar cr mempty EUpdater

writeWindowsUpdaterRunner :: FilePath -> M ()
writeWindowsUpdaterRunner runnerPath = liftIO $ do
    exePath <- getExecutablePath
    launcherArgs <- getArgs
    writeFile (toString runnerPath) $ unlines
        [ "TaskKill /IM cardano-launcher.exe /F"
        -- Run updater
        , "%*"
        -- Delete updater
        , "del %1"
        -- Run launcher again
        , "start \"cardano launcher\" /b " <> (quote $ toText exePath) <> " " <> (unwords $ map (quote . toText) launcherArgs)
        -- Delete the bat file
        , "(goto) 2>nul & del \"%~f0\""
        ]
  where
    quote str = "\"" <> str <> "\""

----------------------------------------------------------------------------
-- Running stuff
----------------------------------------------------------------------------

spawnNode
    :: NodeData
    -> Bool -- Wallet logging
    -> M (ProcessHandle, Async ExitCode)
spawnNode nd doesWalletLogToConsole = do
    let path = ndPath nd
        args = ndArgs nd
        mLogPath = ndLogPath nd
    logNotice "Starting the node"
    -- We don't explicitly close the `logHandle` here,
    -- but this will be done when we run the `CreateProcess` built
    -- by proc later in `system'`:
    -- http://hackage.haskell.org/package/process-1.6.1.0/docs/System-Process.html#v:createProcess
    cr <- liftIO $ case mLogPath of
        Just lp -> do
            createLogFileProc path args lp
            -- TODO (jmitchell): Find a safe, reliable way to print `logPath`. Cardano
            -- fails when it prints unicode characters. In the meantime, don't print it.
            -- See DAEF-12.
            -- printf ("Redirecting node's stdout and stderr to "%fp%"\n") logPath
        Nothing -> do
            let cr = if doesWalletLogToConsole then
                        Process.CreatePipe
                    else Process.Inherit
            return $ createProc cr path args
    phvar <- newEmptyMVar
    asc <- async (system' phvar cr mempty ENode)
    mbPh <- liftIO $ timeout 10000000 (takeMVar phvar)
    case mbPh of
        Nothing -> do
            logError "Couldn't run the node (it didn't start after 10s)"
            exitFailure
        Just ph -> do
            logInfo "Node has started"
            return (ph, asc)

runWallet
    :: Bool              -- ^ wallet logging
    -> NodeData          -- ^ Wallet, its args, wallet log file
    -> Maybe FilePath    -- ^ Node log file
    -> M ExitCode
runWallet shouldLog nd nLogPath = do
    let wpath = ndPath nd
        wargs = ndArgs nd
        mWLogPath = ndLogPath nd
    logNotice "Starting the wallet"
    phvar <- newEmptyMVar
    liftIO $ case mWLogPath of
        Just lp -> do
            cr <- createLogFileProc wpath wargs lp
            system' phvar cr mempty EWallet
        Nothing ->
           -- if nLog is Nothing and shouldLog is True
           -- we want to CreatePipe otherwise Inherit
           let cr = if shouldLog && isNothing nLogPath then
                        Process.CreatePipe
                    else Process.Inherit
           in system' phvar (createProc cr wpath wargs) mempty EWallet

createLogFileProc :: FilePath -> [Text] -> FilePath -> IO Process.CreateProcess
createLogFileProc path args lp = do
    _ <- createDirectoryIfMissing True (directory lp)
    (_, logHandle) <- (lp,) <$> openFile lp AppendMode
    IO.hSetBuffering logHandle IO.LineBuffering
    return $ createProc (Process.UseHandle logHandle) path args

createProc :: Process.StdStream -> FilePath -> [Text] -> Process.CreateProcess
createProc stdStream path args =
    (Process.proc path (map toString args))
            { Process.std_in = Process.CreatePipe
            , Process.std_out = stdStream
            , Process.std_err = stdStream
            }

asyncLog :: Handle -> Handle -> ProcessHandle -> Executable -> IO ExitCode
asyncLog stdO stdE pH nt = do
    withAsync (forever $ T.hGetLine stdO >>= customLogger stdout nt) $ \_ ->
        withAsync (forever $ T.hGetLine stdE >>= customLogger stderr nt) $ \_ -> do
        waitForProcess pH

customLogger :: Handle -> Executable -> Text -> IO ()
customLogger hndl loggerName logStr = do
    T.hPutStrLn hndl $ logNameStr <> logStr
    where
        logNameStr = case loggerName of
            ENode    -> "[node] "
            EWallet  -> "[wallet] "
            EUpdater -> "[updater] "

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
    :: ExitCode        -- ^ Exit code of the node
    -> Maybe FilePath  -- ^ Path to the logger config
    -> String          -- ^ URL of the server
    -> M ()
reportNodeCrash exitCode logConfPath reportServ = liftIO $ do
    logConfig <- readLoggerConfig (toString <$> logConfPath)
    let logFileNames =
            map ((fromMaybe "" (logConfig ^. Log.lcLogsDirectory) </>) . snd) $
            retrieveLogFiles logConfig
    let logFiles = filter (".pub" `isSuffixOf`) logFileNames
    let ec = case exitCode of
            ExitSuccess   -> 0
            ExitFailure n -> n
    bracket (compressLogs logFiles) removeFile $ \txz ->
        sendReport [txz] (RCrash ec) "cardano-node" reportServ

-- Taken from the 'turtle' library and modified
system'
    :: (HasConfigurations, MonadIO io)
    => MVar ProcessHandle
    -- ^ Where to put process handle
    -> Process.CreateProcess
    -- ^ Command
    -> [Text]
    -- ^ Lines of standard input
    -> Executable
    -- ^ node/wallet log output
    -> io ExitCode
    -- ^ Exit code
system' phvar p sl nt = liftIO (do
    let open = do
            (hIn, stdO, stdE, ph) <- Process.createProcess p
            putMVar phvar ph
            case (hIn, stdO, stdE) of
                (Just hndl, Just o, Just e) -> do
                    -- log certain kind of processes since
                    -- inherited ones might create infinite loop
                    case (Process.std_out p, Process.std_err p) of
                        (Process.CreatePipe, Process.CreatePipe) -> do
                            _ <- asyncLog o e ph nt
                            return ()
                        _ -> return ()
                    IO.hSetBuffering hndl IO.LineBuffering
                _        -> return ()
            return (hIn, ph)

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
    where
      outhandle :: Handle -> [Text] -> IO ()
      outhandle hdl txt = forM_ txt (T.hPutStrLn hdl)

halt :: Async a -> IO ()
halt a = do
    m <- poll a
    case m of
        Nothing          -> cancel a
        Just (Left  msg) -> throwM msg
        Just (Right _)   -> return ()

ignoreSIGPIPE :: IO () -> IO ()
ignoreSIGPIPE = handle (\ex -> case ex of
    IOError
        { ioe_type = ResourceVanished
        , ioe_errno = Just ioe }
        | Errno ioe == ePIPE -> return ()
    _ -> throwM ex )

----------------------------------------------------------------------------
-- SIGKILL
----------------------------------------------------------------------------

-- | If we're on Linux or macOS, send a SIGKILL to a process.
maybeTrySIGKILL :: ProcessHandle -> M ()
maybeTrySIGKILL _h = do
#ifdef mingw32_HOST_OS
    logInfo "Not trying to send a SIGKILL because we're on Windows"
#else
    logInfo "Sending SIGKILL"
    liftIO $ Process.withProcessHandle _h $ \case
        Process.OpenHandle pid -> signalProcess sigKILL pid
        _                      -> pass
#endif
