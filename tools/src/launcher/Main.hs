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
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

import qualified Prelude (show)
import           Universum

import           Control.Concurrent (modifyMVar_)
import           Control.Concurrent.Async.Lifted.Safe (Async, async, cancel,
                     poll, wait, waitAny, withAsync, withAsyncWithUnmask)
import           Control.Exception.Safe (handle, mask_, tryAny)
import           Control.Lens (makeLensesWith)
import           Data.Aeson (FromJSON, ToJSON,
                     Value (Array, Bool, Object, String), fromJSON,
                     genericParseJSON, genericToJSON, toJSON, withObject)
import qualified Data.Aeson as AE
import           Data.Aeson.Options (defaultOptions)
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (isNothing)
import qualified Data.Text.IO as T
import           Data.Time.Units (Second, convertUnit)
import           Data.Version (showVersion)
import qualified Data.Yaml as Y
import           Formatting (int, sformat, shown, stext, string, (%))
import qualified NeatInterpolation as Q (text)
import           Options.Applicative (Parser, ParserInfo, ParserResult (..),
                     defaultPrefs, execParserPure, footerDoc, fullDesc,
                     handleParseResult, header, help, helper, info, infoOption,
                     long, metavar, progDesc, renderFailure, short, strOption)
import           System.Directory (createDirectoryIfMissing, doesFileExist,
                     removeFile)
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
import           Text.PrettyPrint.ANSI.Leijen (Doc)

#ifndef mingw32_HOST_OS
import           System.Posix.Signals (sigKILL, signalProcess)
import qualified System.Process.Internals as Process
#endif

-- Modules needed for system'
import           Foreign.C.Error (Errno (..), ePIPE)
import           GHC.IO.Exception (IOErrorType (..), IOException (..))

import           Paths_cardano_sl (version)
import           Pos.Chain.Genesis (Config (..))
import           Pos.Core (Timestamp (..))
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB.Block (dbGetSerBlockRealDefault,
                     dbGetSerBlundRealDefault, dbGetSerUndoRealDefault,
                     dbPutSerBlundsRealDefault)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..))
import           Pos.DB.Rocks (NodeDBs, closeNodeDBs, dbDeleteDefault,
                     dbGetDefault, dbIterSourceDefault, dbPutDefault,
                     dbWriteBatchDefault, openNodeDBs)
import           Pos.DB.Update (affirmUpdateInstalled)
import           Pos.Launcher (HasConfigurations, withConfigurations)
import           Pos.Launcher.Configuration (ConfigurationOptions (..))
import           Pos.Network.Update.Download (installerHash)
import           Pos.Util (HasLens (..), directory, logException,
                     postfixLFields)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.Log.LoggerConfig (BackendKind (..), LogHandler (..),
                     LogSecurityLevel (..), defaultInteractiveConfiguration,
                     lcBasePath, lcLoggerTree, ltHandlers, ltMinSeverity)
import           Pos.Util.Wlog (LoggerNameBox (..), Severity (Info), logError,
                     logInfo, logNotice, logWarning, removeAllHandlers,
                     setupLogging', usingLoggerName)

import           Pos.Tools.Launcher.Environment (substituteEnvVarsValue)
import           Pos.Tools.Launcher.Logging (reportErrorDefault)

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
    , loWorkingDir          :: !FilePath
    , loX509ToolPath        :: !FilePath
    , loUpdaterPath         :: !FilePath
    , loUpdaterArgs         :: ![Text]
    , loUpdateArchive       :: !(Maybe FilePath)
    , loUpdateWindowsRunner :: !(Maybe FilePath)
    , loNodeTimeoutSec      :: !Int
    , loReportServer        :: !(Maybe String)
    , loStatePath           :: !FilePath
    , loConfiguration       :: !ConfigurationOptions
    , loTlsPath             :: !FilePath
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
                , ("workingDir",    String ".")
                , ("nodeArgs",      Array mempty)
                , ("walletArgs",    Array mempty)
                , ("updaterArgs",   Array mempty)
                , ("frontendOnlyMode", Bool False)
                ]

-- | The concrete monad where everything happens
type M a = LoggerNameBox IO a

data Executable = EWallet | ENode | EUpdater | ECertGen

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

instance MonadDBRead LauncherMode where
    dbGet = dbGetDefault
    dbIterSource = dbIterSourceDefault
    dbGetSerBlock = dbGetSerBlockRealDefault
    dbGetSerUndo = dbGetSerUndoRealDefault
    dbGetSerBlund = dbGetSerBlundRealDefault

instance MonadDB LauncherMode where
    dbPut = dbPutDefault
    dbWriteBatch = dbWriteBatchDefault
    dbDelete = dbDeleteDefault
    dbPutSerBlunds = dbPutSerBlundsRealDefault

newtype NodeDbPath = NodeDbPath FilePath

bracketNodeDBs :: NodeDbPath -> (NodeDBs -> IO a) -> IO a
bracketNodeDBs (NodeDbPath dbPath) = bracket (openNodeDBs False dbPath) closeNodeDBs

main :: IO ()
main =
  withCompileInfo $
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

    lo@LO {..} <- getLauncherOptions

    Sys.setCurrentDirectory loWorkingDir

    -- Launcher logs should be in public directory
    let launcherLogsPrefix = loLogsPrefix
    -- Add options specified in loConfiguration but not in loNodeArgs to loNodeArgs.
    let realNodeArgs = propagateOptions lo loConfiguration $
            case loNodeLogConfig of
                Nothing -> loNodeArgs
                Just lc -> loNodeArgs ++ ["--log-config", toText lc]
    lh <- setupLogging' (cfoKey loConfiguration) $
        defaultInteractiveConfiguration Info
            & lcBasePath .~ launcherLogsPrefix
            & lcLoggerTree %~ case launcherLogsPrefix of
                  Nothing ->
                      identity
                  Just _  ->
                    (ltHandlers %~ (\xs -> LogHandler { _lhName="launcher", _lhFpath=Just "pub/launcher"
                                                      , _lhBackend=FileTextBE
                                                      , _lhMinSeverity=Just Info
                                                      , _lhSecurityLevel=Just PublicLogLevel} : xs)) .
                    set ltMinSeverity Info
    logException loggerName . usingLoggerName loggerName $
        withConfigurations Nothing Nothing False loConfiguration $ \genesisConfig _ _ _ -> do

        -- Generate TLS certificates as needed
        generateTlsCertificates loConfiguration loX509ToolPath loTlsPath

        case (loWalletPath, loFrontendOnlyMode) of
            (Nothing, _) -> do
                logNotice "LAUNCHER STARTED"
                logInfo "Running in the server scenario"
                serverScenario
                    (configProtocolMagic genesisConfig)
                    (NodeDbPath loNodeDbPath)
                    (NodeData loNodePath realNodeArgs loNodeLogPath)
                    (UpdaterData loUpdaterPath loUpdaterArgs loUpdateWindowsRunner loUpdateArchive)
                    lo
                logNotice "Finished serverScenario"
            (Just wpath, True) -> do
                frontendOnlyScenario
                    (NodeDbPath loNodeDbPath)
                    (NodeData loNodePath realNodeArgs loNodeLogPath)
                    (NodeData wpath loWalletArgs loWalletLogPath)
                    (UpdaterData loUpdaterPath loUpdaterArgs loUpdateWindowsRunner loUpdateArchive)
                    lo
            (Just wpath, False) -> do
                logNotice "LAUNCHER STARTED"
                logInfo "Running in the client scenario"
                clientScenario
                    (configProtocolMagic genesisConfig)
                    (NodeDbPath loNodeDbPath)
                    (NodeData loNodePath realNodeArgs loNodeLogPath)
                    (NodeData wpath loWalletArgs loWalletLogPath)
                    (UpdaterData loUpdaterPath loUpdaterArgs loUpdateWindowsRunner loUpdateArchive)
                    lo
                logNotice "Finished clientScenario"
    removeAllHandlers lh
  where
    -- We propagate some options to the node executable, because
    -- we almost certainly want to use the same configuration and
    -- don't want to pass the same options twice.  However, if the
    -- user passes these options to the node explicitly, then we
    -- leave their choice. It doesn't cover all cases
    -- (e. g. `--system-start=10`), but it's better than nothing.
    loggerName = "launcher"
    propagateOptions
        :: LauncherOptions -> ConfigurationOptions -> [Text] -> [Text]
    propagateOptions lo (ConfigurationOptions path key systemStart seed) =
        addReportServerOption (loReportServer lo) .
        addNodeDbPath (loNodeDbPath lo) .
        addConfFileOption path .
        addConfKeyOption key .
        addSystemStartOption systemStart .
        addSeedOption seed .
        addLogPrefix (loLogsPrefix lo)

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


generateTlsCertificates :: ConfigurationOptions -> FilePath -> FilePath -> M ()
generateTlsCertificates ConfigurationOptions{..} executable tlsPath = do
    alreadyExists <-
        and <$> mapM (liftIO . doesFileExist) [tlsPath]

    let tlsServer = tlsPath </> "server"
    let tlsClient = tlsPath </> "client"

    unless alreadyExists $ do
        logInfo $ "Generating new TLS certificates in " <> toText tlsPath

        let process = createProc  Process.Inherit executable
                [ "--server-out-dir"     , toText tlsServer
                , "--clients-out-dir"    , toText tlsClient
                , "--configuration-file" , toText cfoFilePath
                , "--configuration-key"  , cfoKey
                ]

        exitCode <- liftIO $ do
            createDirectoryIfMissing True tlsServer
            createDirectoryIfMissing True tlsClient
            phvar <- newEmptyMVar
            system' phvar process mempty ECertGen

        when (exitCode /= ExitSuccess) $ do
            logError "Couldn't generate TLS certificates for Wallet"
            liftIO . fail $ "Couldn't generate TLS certificates; Daedalus wallet won't work without TLS. Please check your configuration and make sure you aren't already running an instance of Daedalus wallet."


-- | If we are on server, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node.
-- * If it exits with code 20, then update and restart, else quit.
serverScenario
    :: (HasCompileInfo, HasConfigurations)
    => ProtocolMagic
    -> NodeDbPath
    -> NodeData           -- ^ Node, args, log path
    -> UpdaterData        -- ^ Updater, args, updater runner, archive path
    -> LauncherOptions
    -> M ()
serverScenario pm ndbp node updater lo = do
    runUpdater ndbp updater
    -- TODO: the updater, too, should create a log if it fails
    (_, nodeAsync) <- spawnNode node False
    exitCode <- wait nodeAsync
    if exitCode == ExitFailure 20 then do
        logNotice $ sformat ("The node has exited with "%shown) exitCode
        serverScenario pm ndbp node updater lo
    else do
        logWarning $ sformat ("The node has exited with "%shown) exitCode

-- | If we are on desktop, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node and the wallet.
-- * If the wallet exits with code 20, then update and restart, else quit.
clientScenario
    :: (HasCompileInfo, HasConfigurations)
    => ProtocolMagic
    -> NodeDbPath
    -> NodeData          -- ^ Node, args, node log path
    -> NodeData          -- ^ Wallet, args, wallet log path
    -> UpdaterData       -- ^ Updater, args, updater runner, archive path
    -> LauncherOptions   -- ^ Launcher Options
    -> M ()
clientScenario pm ndbp node wallet updater lo = do
    runUpdater ndbp updater
    let doesWalletLogToConsole = isNothing (ndLogPath wallet) && (loWalletLogging lo)
    (nodeHandle, nodeAsync) <- spawnNode node doesWalletLogToConsole
    walletAsync <- async (runWallet wallet (ndLogPath node) lo)
    logInfo "Waiting for wallet or node to finish..."
    (someAsync, exitCode) <- waitAny [nodeAsync, walletAsync]
    logInfo "Wallet or node has finished!"
    let restart
          = clientScenario pm ndbp node wallet updater lo
    (walletExitCode, nodeExitCode) <- if
       | someAsync == nodeAsync -> do
             unless (exitCode == ExitFailure 20) $ do
                 logWarning $ sformat ("The node has exited with "%shown) exitCode
             logInfo "Waiting for the wallet to die"
             walletExitCode <- wait walletAsync
             logInfo $ sformat ("The wallet has exited with "%shown) walletExitCode
             return (walletExitCode, Just exitCode)
       | exitCode == ExitFailure 20 -> do
             logNotice "The wallet has exited with code 20"
             logInfo $ sformat
                 ("Waiting for the node to shut down or killing it in "%int%" seconds otherwise")
                 (loNodeTimeoutSec lo)
             nodeExitCode <- liftIO $ timeout (fromIntegral (loNodeTimeoutSec lo) * 1000000) (wait nodeAsync)
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
            timeout (fromIntegral $ loNodeTimeoutSec lo) $
            Process.waitForProcess nodeHandle
        whenNothing_ nodeExitCode $ do
            logWarning "The node didn't die after 'terminateProcess'"
            maybeTrySIGKILL nodeHandle

frontendOnlyScenario :: HasConfigurations => NodeDbPath -> NodeData -> NodeData -> UpdaterData -> LauncherOptions -> M ()
frontendOnlyScenario ndbp node wallet updater lo = do
    runUpdater ndbp updater
    logInfo "Waiting for wallet to finish..."
    exitCode <- runWallet wallet (ndLogPath node) lo
    logInfo "Wallet has finished!"
    let restart = frontendOnlyScenario ndbp node wallet updater lo
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

runUpdaterProc :: FilePath -> [Text] -> M ExitCode
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
data SafeModeConfig = SafeModeConfig { smcSafeMode :: ! Bool } deriving Generic
instance FromJSON SafeModeConfig where
    parseJSON = genericParseJSON defaultOptions
instance ToJSON SafeModeConfig where
    toJSON = genericToJSON defaultOptions
readSafeMode :: LauncherOptions -> M Bool
readSafeMode lo = do
    let safeModeConfigFile = getSafeModeConfigPath lo
    decoded <- liftIO $ Y.decodeFileEither safeModeConfigFile
    case decoded of
        Right value -> pure $ smcSafeMode value
        Left _      -> pure False

saveSafeMode :: LauncherOptions -> Bool -> M ()
saveSafeMode lo status = do
    logInfo $ sformat ("Saving wallet safe mode setting: "%shown) status
    let safeModeConfigFile = getSafeModeConfigPath lo
    liftIO $ Y.encodeFile safeModeConfigFile $ SafeModeConfig status
    pure ()

getSafeModeConfigPath :: LauncherOptions -> FilePath
getSafeModeConfigPath LO{..} = loStatePath </> "safemode.yaml"

runWallet
    :: NodeData          -- ^ Wallet, its args, wallet log file
    -> Maybe FilePath    -- ^ Node log file
    -> LauncherOptions   -- ^ Launcher Options
    -> M ExitCode
runWallet nd nLogPath lo = do
    safeMode <- readSafeMode lo
    let wpath = ndPath nd
        wargs = (ndArgs nd) <> (if safeMode then [ "--safe-mode", "--disable-gpu", "--disable-d3d11" ] else [])
        mWLogPath = ndLogPath nd
    logNotice "Starting the wallet"
    phvar <- newEmptyMVar
    let go = liftIO $ case mWLogPath of
            Just lp -> do
                cr <- createLogFileProc wpath wargs lp
                system' phvar cr mempty EWallet
            Nothing ->
               let cr = if | not (loWalletLogging lo) -> Process.NoStream
                           -- If node's output is not redirected, we want
                           -- to receive node's output and modify it.
                           | isNothing nLogPath -> Process.CreatePipe
                           -- If node's output is redirected, it's ok to
                           -- let wallet log to launcher's standard streams.
                           | otherwise -> Process.Inherit
               in system' phvar (createProc cr wpath wargs) mempty EWallet
    walletExitStatus <- go
    case walletExitStatus of
      ExitFailure 21 -> do
             logNotice "The wallet has exited with code 21"
             logInfo "Switching Configuration to safe mode"
             saveSafeMode lo True
             runWallet nd nLogPath lo

      ExitFailure 22 -> do
             logNotice "The wallet has exited with code 22"
             logInfo "Switching Configuration to normal mode"
             saveSafeMode lo False
             runWallet nd nLogPath lo

      _ -> pure walletExitStatus


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
            ECertGen -> "[X509-certificates] "

----------------------------------------------------------------------------
-- Working with the report server
----------------------------------------------------------------------------

-- Taken from the 'turtle' library and modified
system'
    :: MonadIO io
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
