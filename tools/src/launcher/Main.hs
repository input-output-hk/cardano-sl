{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
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

import qualified Prelude (show)
import           Universum

import           Control.Concurrent (modifyMVar_)
import           Control.Concurrent.Async.Lifted.Safe (Async, async, cancel, poll, wait, waitAny,
                                                       withAsync, withAsyncWithUnmask)
import           Control.Exception.Safe (catchAny, handle, mask_, tryAny)
import           Control.Lens (makeLensesWith)
import           Data.Aeson (FromJSON, Value (Array, Bool, Object), fromJSON, genericParseJSON,
                             withObject)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.HashMap.Strict as HM
import           Data.List (isSuffixOf)
import           Data.Maybe (isNothing)
import qualified Data.Text.IO as T
import           Data.Time.Units (Second, convertUnit)
import           Data.Version (showVersion)
import qualified Data.Yaml as Y
import           Formatting (build, int, sformat, shown, stext, string, (%))
import qualified NeatInterpolation as Q (text)
import           Options.Applicative (Parser, ParserInfo, ParserResult (..), defaultPrefs,
                                      execParserPure, footerDoc, fullDesc, handleParseResult,
                                      header, help, helper, info, infoOption, long, metavar,
                                      progDesc, renderFailure, short, strOption)
import           Serokell.Aeson.Options (defaultOptions)
import           System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import qualified System.Directory as Sys
import           System.Environment (getExecutablePath, getProgName, setEnv)
import           System.Exit (ExitCode (..))
import           System.FilePath (takeDirectory, (</>))
import qualified System.Info as Sys
import qualified System.IO as IO
import qualified System.IO.Silently as Silently
import           System.Process (ProcessHandle, waitForProcess)
import qualified System.Process as Process
import           System.Timeout (timeout)
import           System.Wlog (logError, logInfo, logNotice, logWarning)
import qualified System.Wlog as Log
import           Text.PrettyPrint.ANSI.Leijen (Doc)

#ifndef mingw32_HOST_OS
import           System.Posix.Signals (sigKILL, signalProcess)
import qualified System.Process.Internals as Process
#endif

-- Modules needed for system'
import           Foreign.C.Error (Errno (..), ePIPE)
import           GHC.IO.Exception (IOErrorType (..), IOException (..))

import           Paths_cardano_sl (version)
import           Pos.Client.CLI (readLoggerConfig)
import           Pos.Core (HasConfiguration, Timestamp (..), protocolMagic)
import           Pos.DB.Block (dbGetSerBlockRealDefault, dbGetSerUndoRealDefault,
                               dbPutSerBlundsRealDefault)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..))
import           Pos.DB.Rocks (NodeDBs, closeNodeDBs, dbDeleteDefault, dbGetDefault,
                               dbIterSourceDefault, dbPutDefault, dbWriteBatchDefault, openNodeDBs)
import           Pos.Launcher (HasConfigurations, withConfigurations)
import           Pos.Launcher.Configuration (ConfigurationOptions (..))
import           Pos.Reporting.Wlog (compressLogs, retrieveLogFiles)
import           Pos.Reporting.Http (sendReport)
import           Pos.ReportServer.Report (ReportType (..))
import           Pos.Update (installerHash)
import           Pos.Update.DB.Misc (affirmUpdateInstalled)
import           Pos.Util (HasLens (..), directory, logException, postfixLFields)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo, compileInfo)

import           Launcher.Environment (substituteEnvVarsValue)
import           Launcher.Logging (reportErrorDefault)

data LauncherOptions = LO
    { loNodePath            :: !FilePath
    , loNodeArgs            :: ![Text]
    , loNodeDbPath          :: !FilePath
    , loNodeLogConfig       :: !(Maybe FilePath)
    , loNodeLogPath         :: !(Maybe FilePath)
    , loWalletPath          :: !(Maybe FilePath)
    , loFrontendOnlyMode    :: !Bool
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
    -- | This prefix will be passed as logs-prefix to the node. Launcher logs
    -- will be written into "pub" subdirectory of the prefix (as well as to
    -- console, except on Windows where we don't output anything to console
    -- because it crashes).
    , loLogsPrefix          :: !(Maybe FilePath)
    } deriving (Generic)

instance FromJSON LauncherOptions where
    parseJSON = withObject "LauncherOptions" $ \o ->
        genericParseJSON defaultOptions $ Object $
            -- This provides default values for some keys and
            -- allows not specifying them in the configuration
            -- file at all. @<>@ for hashmaps is left-biased,
            -- so it only adds new keys if there are aren't
            -- any yet.
            o <> HM.fromList
                [ ("walletLogging", Bool False)
                , ("nodeArgs",      Array mempty)
                , ("walletArgs",    Array mempty)
                , ("updaterArgs",   Array mempty)
                , ("frontendOnlyMode", Bool False)
                ]

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

data LauncherArgs = LauncherArgs
    { maybeConfigPath  :: !(Maybe FilePath)
    }

data LauncherError =
      ConfigParseError !FilePath !(Y.ParseException)

instance Show LauncherError where
    show (ConfigParseError configPath yamlException) = toString $
        sformat ("Failed to parse config at "%string%": "%shown) configPath yamlException

instance Exception LauncherError

launcherArgsParser :: Parser LauncherArgs
launcherArgsParser = do
    maybeConfigPath <- optional $ strOption $
        short   'c' <>
        long    "config" <>
        help    "Path to the launcher configuration file." <>
        metavar "PATH"
    pure $ LauncherArgs {..}

getLauncherOptions :: IO LauncherOptions
getLauncherOptions = do
    LauncherArgs {..} <- either parseErrorHandler pure =<< execParserEither programInfo
    daedalusDir <- takeDirectory <$> getExecutablePath
    case Sys.os of
      "mingw32" -> do
        -- This is used by 'substituteEnvVars', later
        setEnv "DAEDALUS_DIR" daedalusDir
      _ -> pure ()
    -- linux and windows use DAEDALUS_DIR for different things, but having a single var with the same meaning will help with some issues
    setEnv "DAEDALUS_INSTALL_DIRECTORY" daedalusDir
    configPath <- maybe defaultConfigPath pure maybeConfigPath

    -- [CSL-2503] remove once cardano-node is capable of finding the file on its own and daedalus no longer needs it
    setEnv "LAUNCHER_CONFIG" configPath

    decoded <- Y.decodeFileEither configPath
    case decoded of
        Left err -> do
            reportErrorDefault "config-parse-error.log" $ show err
            throwM $ ConfigParseError configPath err
        Right value -> do
          let contextDesc = "Substituting environment vars in '"<>toText configPath<>"'"
          substituted <- substituteEnvVarsValue contextDesc value
          case fromJSON $ substituted of
            AE.Error err -> do
              reportErrorDefault "config-parse-error.log" $ show err
              error $ toText err
            AE.Success op -> pure op

  where
    execParserEither :: ParserInfo a -> IO (Either (Text, ExitCode) a)
    execParserEither pinfo = do
        args <- getArgs
        case execParserPure defaultPrefs pinfo args of
            Success a -> pure $ Right a
            Failure failure -> do
                progn <- getProgName
                let (msg, exitCode) = renderFailure failure progn
                pure $ Left (toText msg, exitCode)
            CompletionInvoked compl -> handleParseResult $ CompletionInvoked compl

    parseErrorHandler :: (Text, ExitCode) -> IO a
    parseErrorHandler (msg, exitCode) = do
        reportErrorDefault "cli-parse-error.log" msg
        exitWith exitCode

    programInfo = info (helper <*> versionOption <*> launcherArgsParser) $
        fullDesc <> progDesc ""
                 <> header "Tool to launch Cardano SL."
                 <> footerDoc usageExample

    versionOption = infoOption
        ("cardano-launcher-" <> showVersion version)
        (long "version" <> help "Show version.")

    defaultConfigPath :: IO FilePath
    defaultConfigPath = do
        launcherDir <- takeDirectory <$> getExecutablePath
        pure $ launcherDir </> "launcher-config.yaml"

usageExample :: Maybe Doc
usageExample = (Just . fromString @Doc . toString @Text) [Q.text|
Command example:

  stack exec -- cardano-launcher --config launcher-config.yaml

See tools/src/launcher/launcher-config.yaml for
an example of the config file.|]

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
    dbPutSerBlunds = dbPutSerBlundsRealDefault

newtype NodeDbPath = NodeDbPath FilePath

bracketNodeDBs :: NodeDbPath -> (NodeDBs -> IO a) -> IO a
bracketNodeDBs (NodeDbPath dbPath) = bracket (openNodeDBs False dbPath) closeNodeDBs

main :: IO ()
main =
  withCompileInfo $(retrieveCompileTimeInfo) $
  case Sys.os of
    "mingw32" ->
      -- We don't output anything to console on Windows because on Windows the
      -- launcher is considered a “GUI application” and so stdout and stderr
      -- don't even exist.
      Silently.hSilence [stdout, stderr]
    _ -> identity
  $ do
    Sys.getXdgDirectory Sys.XdgData "" >>= setEnv "XDG_DATA_HOME"
    setEnv "LC_ALL" "en_GB.UTF-8"
    setEnv "LANG"   "en_GB.UTF-8"

    LO {..} <- getLauncherOptions
    -- Launcher logs should be in public directory
    let launcherLogsPrefix = (</> "pub") <$> loLogsPrefix
    -- Add options specified in loConfiguration but not in loNodeArgs to loNodeArgs.
    let realNodeArgs = propagateOptions loReportServer loNodeDbPath loLogsPrefix loConfiguration $
            case loNodeLogConfig of
                Nothing -> loNodeArgs
                Just lc -> loNodeArgs ++ ["--log-config", toText lc]
    Log.setupLogging Nothing $
        Log.productionB
            & Log.lcTermSeverityOut .~ Just Log.debugPlus
            & Log.lcLogsDirectory .~ launcherLogsPrefix
            & Log.lcTree %~ case launcherLogsPrefix of
                  Nothing ->
                      identity
                  Just _  ->
                      set Log.ltFiles [Log.HandlerWrap "launcher" Nothing] .
                      set Log.ltSeverity (Just Log.debugPlus)
    logException loggerName . Log.usingLoggerName loggerName $
        withConfigurations loConfiguration $ \_ ->
        case (loWalletPath, loFrontendOnlyMode) of
            (Nothing, _) -> do
                logNotice "LAUNCHER STARTED"
                logInfo "Running in the server scenario"
                serverScenario
                    (NodeDbPath loNodeDbPath)
                    loLogsPrefix
                    loNodeLogConfig
                    (NodeData loNodePath realNodeArgs loNodeLogPath)
                    (UpdaterData
                        loUpdaterPath loUpdaterArgs loUpdateWindowsRunner loUpdateArchive)
                    loReportServer
                logNotice "Finished serverScenario"
            (Just wpath, True) -> do
                frontendOnlyScenario
                    (NodeDbPath loNodeDbPath)
                    (NodeData loNodePath realNodeArgs loNodeLogPath)
                    (NodeData wpath loWalletArgs loWalletLogPath)
                    (UpdaterData loUpdaterPath loUpdaterArgs loUpdateWindowsRunner loUpdateArchive)
                    loWalletLogging
            (Just wpath, False) -> do
                logNotice "LAUNCHER STARTED"
                logInfo "Running in the client scenario"
                clientScenario
                    (NodeDbPath loNodeDbPath)
                    loLogsPrefix
                    loNodeLogConfig
                    (NodeData loNodePath realNodeArgs loNodeLogPath)
                    (NodeData wpath loWalletArgs loWalletLogPath)
                    (UpdaterData
                        loUpdaterPath loUpdaterArgs loUpdateWindowsRunner loUpdateArchive)
                    loNodeTimeoutSec
                    loReportServer
                    loWalletLogging
                logNotice "Finished clientScenario"
  where
    -- We propagate some options to the node executable, because
    -- we almost certainly want to use the same configuration and
    -- don't want to pass the same options twice.  However, if the
    -- user passes these options to the node explicitly, then we
    -- leave their choice. It doesn't cover all cases
    -- (e. g. `--system-start=10`), but it's better than nothing.
    loggerName = "launcher"
    propagateOptions
        :: Maybe String -> FilePath -> Maybe FilePath -> ConfigurationOptions -> [Text] -> [Text]
    propagateOptions maybeReportServer nodeDbPath logPrefix (ConfigurationOptions path key systemStart seed) =
        addReportServerOption maybeReportServer .
        addNodeDbPath nodeDbPath .
        addConfFileOption path .
        addConfKeyOption key .
        addSystemStartOption systemStart .
        addSeedOption seed .
        addLogPrefix logPrefix

    addReportServerOption =
        maybe identity (maybeAddOption "--report-server" . toText)
    addNodeDbPath nodeDbPath =
        maybeAddOption "--db-path" (toText nodeDbPath)
    addConfFileOption filePath =
        maybeAddOption "--configuration-file" (toText filePath)
    addConfKeyOption key = maybeAddOption "--configuration-key" key
    addSystemStartOption =
        maybe identity (maybeAddOption "--system-start" . timestampToText)
    addSeedOption =
        maybe identity (maybeAddOption "--configuration-seed" . show)
    addLogPrefix = maybe identity (maybeAddOption "--logs-prefix" . toText)

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
    -> Maybe FilePath     -- ^ Log prefix
    -> Maybe FilePath     -- ^ Logger config
    -> NodeData           -- ^ Node, args, log path
    -> UpdaterData        -- ^ Updater, args, updater runner, archive path
    -> Maybe String       -- ^ Report server
    -> M ()
serverScenario ndbp logPrefix logConf node updater report = do
    runUpdater ndbp updater
    -- TODO: the updater, too, should create a log if it fails
    (_, nodeAsync) <- spawnNode node False
    exitCode <- wait nodeAsync
    if exitCode == ExitFailure 20 then do
        logNotice $ sformat ("The node has exited with "%shown) exitCode
        serverScenario ndbp logPrefix logConf node updater report
    else do
        logWarning $ sformat ("The node has exited with "%shown) exitCode
        whenJust report $ \repServ -> do
            logInfo $ sformat ("Sending logs to "%stext) (toText repServ)
            reportNodeCrash exitCode logPrefix logConf repServ

-- | If we are on desktop, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node and the wallet.
-- * If the wallet exits with code 20, then update and restart, else quit.
clientScenario
    :: NodeDbPath
    -> Maybe FilePath    -- ^ Log prefix
    -> Maybe FilePath    -- ^ Logger config
    -> NodeData          -- ^ Node, args, node log path
    -> NodeData          -- ^ Wallet, args, wallet log path
    -> UpdaterData       -- ^ Updater, args, updater runner, archive path
    -> Int               -- ^ Node timeout, in seconds
    -> Maybe String      -- ^ Report server
    -> Bool              -- ^ Wallet logging
    -> M ()
clientScenario ndbp logPrefix logConf node wallet updater nodeTimeout report walletLog = do
    runUpdater ndbp updater
    let doesWalletLogToConsole = isNothing (ndLogPath wallet) && walletLog
    (nodeHandle, nodeAsync) <- spawnNode node doesWalletLogToConsole
    walletAsync <- async (runWallet walletLog wallet (ndLogPath node))
    logInfo "Waiting for wallet or node to finish..."
    (someAsync, exitCode) <- waitAny [nodeAsync, walletAsync]
    logInfo "Wallet or node has finished!"
    let restart
            = clientScenario ndbp logPrefix logConf node wallet updater nodeTimeout report walletLog
    (walletExitCode, nodeExitCode) <- if
       | someAsync == nodeAsync -> do
             unless (exitCode == ExitFailure 20) $ do
                 logWarning $ sformat ("The node has exited with "%shown) exitCode
                 whenJust report $ \repServ -> do
                     logInfo $ sformat ("Sending logs to "%stext) (toText repServ)
                     reportNodeCrash exitCode logPrefix logConf repServ
             logInfo "Waiting for the wallet to die"
             walletExitCode <- wait walletAsync
             logInfo $ sformat ("The wallet has exited with "%shown) walletExitCode
             return (walletExitCode, Just exitCode)
       | exitCode == ExitFailure 20 -> do
             logNotice "The wallet has exited with code 20"
             logInfo $ sformat
                 ("Waiting for the node to shut down or killing it in "%int%" seconds otherwise")
                 nodeTimeout
             nodeExitCode <- liftIO $ timeout (fromIntegral nodeTimeout * 1000000) (wait nodeAsync)
             return (exitCode, nodeExitCode)
       | otherwise -> do
             logWarning $ sformat ("The wallet has exited with "%shown) exitCode
             -- TODO: does the wallet have some kind of log?
             return (exitCode, Nothing)
    when (isNothing nodeExitCode) $ do
        logWarning "The wallet has exited, but the node is still up."
        killNode nodeHandle nodeAsync
    when (walletExitCode == ExitFailure 20) $
        case nodeExitCode of
            Just (ExitFailure 20) -> restart
            _ -> logWarning $
                "The wallet has exited with code 20, but we won't update due to node crash"
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

frontendOnlyScenario :: NodeDbPath -> NodeData -> NodeData -> UpdaterData -> Bool -> M ()
frontendOnlyScenario ndbp node wallet updater walletLog = do
    runUpdater ndbp updater
    logInfo "Waiting for wallet to finish..."
    exitCode <- runWallet walletLog wallet (ndLogPath node)
    logInfo "Wallet has finished!"
    let restart = frontendOnlyScenario ndbp node wallet updater walletLog
    if exitCode == ExitFailure 20
        then do
            logNotice "The wallet has exited with code 20"
            restart
        else do
            logWarning $ sformat ("The wallet has exited with "%shown) exitCode

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
runUpdaterProc path args = do
    logNotice $ sformat ("    "%string%" "%stext) path (unwords $ map quote args)
    liftIO $ do
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
    logNotice $ sformat ("    "%string%" "%stext) path (unwords $ map quote args)
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
           let cr = if | not shouldLog -> Process.NoStream
                       -- If node's output is not redirected, we want
                       -- to receive node's output and modify it.
                       | isNothing nLogPath -> Process.CreatePipe
                       -- If node's output is redirected, it's ok to
                       -- let wallet log to launcher's standard streams.
                       | otherwise -> Process.Inherit
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
    -> Maybe FilePath  -- ^ Log prefix
    -> Maybe FilePath  -- ^ Path to the logger config
    -> String          -- ^ URL of the server
    -> M ()
reportNodeCrash exitCode _ logConfPath reportServ = do
    logConfig <- readLoggerConfig (toString <$> logConfPath)
    let logFileNames =
            map ((fromMaybe "" (logConfig ^. Log.lcLogsDirectory) </>) . snd) $
            retrieveLogFiles logConfig
        -- The log files are computed purely: they're only hypothetical. They
        -- are the file names that the logger config *would* create, but they
        -- don't necessarily exist on disk. 'compressLogs' assumes that all
        -- of the paths given indeed exist, and it will throw an exception if
        -- any of them do not (or if they're not tar-appropriate paths).
        hyptheticalLogFiles = filter (".pub" `isSuffixOf`) logFileNames
        ec = case exitCode of
            ExitSuccess   -> 0
            ExitFailure n -> n
        handler = logError . sformat ("Failed to report node crash: "%build)
        sendIt logFiles = bracket (compressLogs logFiles) (liftIO . removeFile) $ \txz ->
            liftIO $ sendReport protocolMagic compileInfo (Just txz) (RCrash ec) "cardano-node" reportServ
    logFiles <- liftIO $ filterM doesFileExist hyptheticalLogFiles
    -- We catch synchronous exceptions and do not re-throw! This is a crash
    -- handler. It runs if some other part of the system crashed. We wouldn't
    -- want a crash in the crash handler to shadow an existing crash.
    liftIO (sendIt logFiles) `catchAny` handler

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
    -- ^ executable to run
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

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

quote :: Text -> Text
quote str = "\"" <> str <> "\""
