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
                                                       withAsyncWithUnmask)
import           Control.Exception.Safe (tryAny)
import           Control.Lens (makeLensesWith)
import qualified Data.ByteString.Lazy as BS.L
import           Data.List (isSuffixOf)
import qualified Data.Text.IO as T
import           Data.Time.Units (Second, convertUnit)
import           Data.Version (showVersion)
import           Formatting (int, sformat, shown, stext, (%))
import qualified NeatInterpolation as Q (text)
import           Options.Applicative (Mod, OptionFields, Parser, auto, execParser, footerDoc,
                                      fullDesc, header, help, helper, info, infoOption, long,
                                      metavar, option, progDesc, short, strOption)
import           System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory,
                                   removeFile)
import           System.Environment (getExecutablePath)
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))
import qualified System.IO as IO
import           System.Process (ProcessHandle, readProcessWithExitCode)
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
import           Control.Exception (handle, mask_, throwIO)
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
        help    ("File where node stdout/err will be redirected " <>
                 "(def: temp file).") <>
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

  stack exec -- cardano-launcher                                   \
    --node binaries_v000/cardano-node                              \
    --node-log-config scripts/log-templates/update-log-config.yaml \
    -n "--update-server"                                           \
    -n "http://localhost:3001"                                     \
    -n "--update-latest-path"                                      \
    -n "updateDownloaded.tar"                                      \
    -n "--listen"                                                  \
    -n "127.0.0.1:3004"                                            \
    -n "--kademlia-id"                                             \
    -n "a_P8zb6fNP7I2H54FtGuhqxaMDAwMDAwMDAwMDAwMDA="              \
    -n "--flat-distr"                                              \
    -n "(3,100000)"                                                \
    -n "--rebuild-db"                                              \
    -n "--wallet"                                                  \
    -n "--web-port"                                                \
    -n 8080                                                        \
    -n "--wallet-port"                                             \
    -n 8090                                                        \
    -n "--wallet-rebuild-db"                                       \
    --updater cardano-updater                                      \
    -u "dir"                                                       \
    -u "binaries_v000"                                             \
    --node-timeout 5                                               \
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
            & Log.lcTermSeverity .~ Just Log.Debug
            & Log.lcFilePrefix .~ loLauncherLogsPrefix
            & Log.lcTree %~ case loLauncherLogsPrefix of
                  Nothing ->
                      identity
                  Just _  ->
                      set Log.ltFiles [Log.HandlerWrap "launcher" Nothing] .
                      set Log.ltSeverity (Just Log.Debug)
    Log.usingLoggerName "launcher" $
        withConfigurations loConfiguration $
        case loWalletPath of
            Nothing -> do
                logNotice "LAUNCHER STARTED"
                logInfo "Running in the server scenario"
                serverScenario
                    (NodeDbPath loNodeDbPath)
                    loNodeLogConfig
                    (loNodePath, realNodeArgs, loNodeLogPath)
                    ( loUpdaterPath
                    , loUpdaterArgs
                    , loUpdateWindowsRunner
                    , loUpdateArchive)
                    loReportServer
            Just wpath -> do
                logNotice "LAUNCHER STARTED"
                logInfo "Running in the client scenario"
                clientScenario
                    (NodeDbPath loNodeDbPath)
                    loNodeLogConfig
                    (loNodePath, realNodeArgs, loNodeLogPath)
                    (wpath, loWalletArgs)
                    ( loUpdaterPath
                    , loUpdaterArgs
                    , loUpdateWindowsRunner
                    , loUpdateArchive)
                    loNodeTimeoutSec
                    loReportServer
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
    -> Maybe FilePath                      -- ^ Logger config
    -> (FilePath, [Text], Maybe FilePath)  -- ^ Node, its args, node log
    -> (FilePath, [Text], Maybe FilePath, Maybe FilePath)
    -- ^ Updater, args, updater runner, the update .tar
    -> Maybe String                        -- ^ Report server
    -> M ()
serverScenario ndbp logConf node updater report = do
    runUpdater ndbp updater
    -- TODO: the updater, too, should create a log if it fails
    (_, nodeAsync) <- spawnNode node
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
    -> Maybe FilePath                      -- ^ Logger config
    -> (FilePath, [Text], Maybe FilePath)  -- ^ Node, its args, node log
    -> (FilePath, [Text])                  -- ^ Wallet, args
    -> (FilePath, [Text], Maybe FilePath, Maybe FilePath)
    -- ^ Updater, args, updater runner, the update .tar
    -> Int                                 -- ^ Node timeout, in seconds
    -> Maybe String                        -- ^ Report server
    -> M ()
clientScenario ndbp logConf node wallet updater nodeTimeout report = do
    runUpdater ndbp updater
    (nodeHandle, nodeAsync) <- spawnNode node
    walletAsync <- async (runWallet wallet)
    (someAsync, exitCode) <- waitAny [nodeAsync, walletAsync]
    let restart = clientScenario ndbp logConf node wallet updater nodeTimeout report
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
runUpdater :: NodeDbPath -> (FilePath, [Text], Maybe FilePath, Maybe FilePath) -> M ()
runUpdater ndbp (path, args, runnerPath, mUpdateArchivePath) = do
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
                    updateArchive <- BS.L.readFile updateArchivePath
                    bracketNodeDBs ndbp $ \lmcNodeDBs ->
                        usingReaderT LauncherModeContext{..} $
                        affirmUpdateInstalled (installerHash updateArchive)
                    removeFile updateArchivePath
            ExitFailure code ->
                logWarning $ sformat ("The updater has failed (exit code "%int%")") code

runUpdaterProc :: HasConfigurations => FilePath -> [Text] -> M ExitCode
runUpdaterProc path args = liftIO $ do
    let cr = (Process.proc (toString path) (map toString args))
                 { Process.std_in  = Process.CreatePipe
                 , Process.std_out = Process.CreatePipe
                 , Process.std_err = Process.CreatePipe
                 }
    phvar <- newEmptyMVar
    system' phvar cr mempty

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
    :: (FilePath, [Text], Maybe FilePath)
    -> M (ProcessHandle, Async ExitCode)
spawnNode (path, args, mbLogPath) = do
    logNotice "Starting the node"
    -- We don't explicitly close the `logHandle` here,
    -- but this will be done when we run the `CreateProcess` built
    -- by proc later is `system'`:
    -- http://hackage.haskell.org/package/process-1.6.1.0/docs/System-Process.html#v:createProcess
    (_, logHandle) <- liftIO $ case mbLogPath of
        Just lp -> do
            createDirectoryIfMissing True (directory lp)
            (lp,) <$> openFile lp AppendMode
        Nothing -> do
            tempdir <- fromString <$> getTemporaryDirectory
            -- FIXME (adinapoli): `Shell` from `turtle` was giving us no-resource-leak guarantees
            -- via the `Managed` monad, which is something we have lost here, and we are back to manual
            -- resource control. In this case, however, shall we really want to nuke the file? It seems
            -- something useful to have lying around in the filesystem. We should probably close the
            -- `Handle`, though, but if this program is short lived it won't matter anyway.
            IO.openTempFile tempdir "cardano-node-output.log"
    -- TODO (jmitchell): Find a safe, reliable way to print `logPath`. Cardano
    -- fails when it prints unicode characters. In the meantime, don't print it.
    -- See DAEF-12.

    -- printf ("Redirecting node's stdout and stderr to "%fp%"\n") logPath
    liftIO $ IO.hSetBuffering logHandle IO.LineBuffering
    let cr = (Process.proc (toString path) (map toString args))
                 { Process.std_in  = Process.CreatePipe
                 , Process.std_out = Process.UseHandle logHandle
                 , Process.std_err = Process.UseHandle logHandle
                 }
    phvar <- newEmptyMVar
    asc <- async (system' phvar cr mempty)
    mbPh <- liftIO $ timeout 10000000 (takeMVar phvar)
    case mbPh of
        Nothing -> do
            logError "Couldn't run the node (it didn't start after 10s)"
            exitFailure
        Just ph -> do
            logInfo "Node has started"
            return (ph, asc)

runWallet :: (FilePath, [Text]) -> M ExitCode
runWallet (path, args) = do
    logNotice "Starting the wallet"
    view _1 <$>
        liftIO (readProcessWithExitCode path (map toString args) mempty)

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
            map ((fromMaybe "" (logConfig ^. Log.lcFilePrefix) </>) . snd) $
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
    where
      outhandle :: Handle -> [Text] -> IO ()
      outhandle hdl txt = forM_ txt (T.hPutStrLn hdl)

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
