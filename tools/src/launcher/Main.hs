{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Universum

import           Control.Concurrent           (modifyMVar_)
import           Control.Concurrent.Async     (Async, async, cancel, poll, wait, waitAny,
                                               withAsyncWithUnmask)
import           Data.List                    (isSuffixOf)
import qualified Data.Text.IO                 as T
import qualified Data.Text.Lazy.IO            as TL
import           Data.Time.Units              (Second, convertUnit)
import           Data.Version                 (showVersion)
import           Formatting                   (format, int, shown, stext, text, (%))
import qualified NeatInterpolation            as Q (text)
import           Options.Applicative          (Mod, OptionFields, Parser, auto,
                                               execParser, footerDoc, fullDesc, header,
                                               help, helper, info, infoOption, long,
                                               metavar, option, progDesc, short,
                                               strOption)
import           System.Directory             (createDirectoryIfMissing, doesFileExist,
                                               getTemporaryDirectory, removeFile)
import           System.Environment           (getExecutablePath)
import           System.Exit                  (ExitCode (..))
import           System.FilePath              (normalise, (</>))
import qualified System.IO                    as IO
import           System.Process               (ProcessHandle, readProcessWithExitCode)
import qualified System.Process               as Process
import           System.Timeout               (timeout)
import           System.Wlog                  (lcFilePrefix, usingLoggerName)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

-- Modules needed for system'
import           Control.Exception            (handle, mask_, throwIO)
import           Foreign.C.Error              (Errno (..), ePIPE)
import           GHC.IO.Exception             (IOErrorType (..), IOException (..))

import           Paths_cardano_sl             (version)
import           Pos.Client.CLI               (configurationOptionsParser,
                                               readLoggerConfig)
import           Pos.Core                     (Timestamp (..))
import           Pos.Launcher                 (HasConfigurations, withConfigurations)
import           Pos.Launcher.Configuration   (ConfigurationOptions (..))
import           Pos.Reporting.Methods        (retrieveLogFiles, sendReport)
import           Pos.ReportServer.Report      (ReportType (..))
import           Pos.Util                     (directory, sleep)
import           Pos.Util.CompileInfo         (HasCompileInfo, retrieveCompileTimeInfo,
                                               withCompileInfo)

data LauncherOptions = LO
    { loNodePath            :: !FilePath
    , loNodeArgs            :: ![Text]
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
    loNodeLogConfig <- optional $ textOption $
        long    "node-log-config" <>
        help    "Path to log config that will be used by the node." <>
        metavar "PATH"
    loNodeLogPath <- optional $ textOption $
        long    "node-log-path" <>
        help    "File where node stdout/err will be redirected\
                \ (def: temp file)." <>
        metavar "PATH"

    -- Wallet-related args
    loWalletPath <- optional $ textOption $
        long    "wallet" <>
        help    "Path to the wallet executable." <>
        metavar "PATH"
    loWalletArgs <- many $ textOption $
        short   'w' <>
        help    "An argument to be passed to the wallet." <>
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
        help    "How much to wait for the node to exit before killing it." <>
        metavar "SEC"
    loReportServer <- optional $ strOption $
        long    "report-server" <>
        help    "Where to send logs in case of failure." <>
        metavar "URL"

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

main :: IO ()
main = do
    LO {..} <- getLauncherOptions
    let realNodeArgs = addConfigurationOptions loConfiguration $
            case loNodeLogConfig of
                Nothing -> loNodeArgs
                Just lc -> loNodeArgs ++ ["--log-config", toText lc]
    usingLoggerName "launcher" $
        withConfigurations loConfiguration $
        withCompileInfo $(retrieveCompileTimeInfo) $
        liftIO $
        case loWalletPath of
            Nothing -> do
                putText "Running in the server scenario"
                serverScenario
                    loNodeLogConfig
                    (loNodePath, realNodeArgs, loNodeLogPath)
                    ( loUpdaterPath
                    , loUpdaterArgs
                    , loUpdateWindowsRunner
                    , loUpdateArchive)
                    loReportServer
            Just wpath -> do
                putText "Running in the client scenario"
                clientScenario
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
    :: (HasConfigurations, HasCompileInfo)
    => Maybe FilePath                      -- ^ Logger config
    -> (FilePath, [Text], Maybe FilePath)  -- ^ Node, its args, node log
    -> (FilePath, [Text], Maybe FilePath, Maybe FilePath)
    -- ^ Updater, args, updater runner, the update .tar
    -> Maybe String                        -- ^ Report server
    -> IO ()
serverScenario logConf node updater report = do
    runUpdater updater
    -- TODO: the updater, too, should create a log if it fails
    (_, nodeAsync, nodeLog) <- spawnNode node
    exitCode <- wait nodeAsync
    putStrLn $ format ("The node has exited with "%shown) exitCode
    if exitCode == ExitFailure 20
        then serverScenario logConf node updater report
        else whenJust report $ \repServ -> do
                 TL.putStrLn $ format ("Sending logs to "%stext) (toText repServ)
                 reportNodeCrash exitCode logConf repServ nodeLog

-- | If we are on desktop, we want the following algorithm:
--
-- * Update (if we are already up-to-date, nothing will happen).
-- * Launch the node and the wallet.
-- * If the wallet exits with code 20, then update and restart, else quit.
clientScenario
    :: (HasConfigurations, HasCompileInfo)
    => Maybe FilePath                      -- ^ Logger config
    -> (FilePath, [Text], Maybe FilePath)  -- ^ Node, its args, node log
    -> (FilePath, [Text])                  -- ^ Wallet, args
    -> (FilePath, [Text], Maybe FilePath, Maybe FilePath)
    -- ^ Updater, args, updater runner, the update .tar
    -> Int                                 -- ^ Node timeout, in seconds
    -> Maybe String                        -- ^ Report server
    -> IO ()
clientScenario logConf node wallet updater nodeTimeout report = do
    runUpdater updater
    (nodeHandle, nodeAsync, nodeLog) <- spawnNode node
    walletAsync <- async (runWallet wallet)
    (someAsync, exitCode) <- liftIO $ waitAny [nodeAsync, walletAsync]
    if | someAsync == nodeAsync -> do
             TL.putStrLn $ format ("The node has exited with "%shown) exitCode
             whenJust report $ \repServ -> do
                 TL.putStrLn $ format ("Sending logs to "%stext) (toText repServ)
                 reportNodeCrash exitCode logConf repServ nodeLog
             putText "Waiting for the wallet to die"
             void $ wait walletAsync
       | exitCode == ExitFailure 20 -> do
             putText "The wallet has exited with code 20"
             TL.putStrLn $ format ("Killing the node in "%int%" seconds") nodeTimeout
             sleep (fromIntegral nodeTimeout)
             putText "Killing the node now"
             liftIO $ do
                 Process.terminateProcess nodeHandle
                 cancel nodeAsync
             clientScenario logConf node wallet updater nodeTimeout report
       | otherwise -> do
             TL.putStrLn $ format ("The wallet has exited with "%shown) exitCode
             -- TODO: does the wallet have some kind of log?
             putText "Killing the node"
             liftIO $ do
                 Process.terminateProcess nodeHandle
                 cancel nodeAsync

-- | We run the updater and delete the update file if the update was
-- successful.
runUpdater :: HasConfigurations => (FilePath, [Text], Maybe FilePath, Maybe FilePath) -> IO ()
runUpdater (path, args, runnerPath, updateArchive) = do
    whenM (doesFileExist path) $ do
        putText "Running the updater"
        let args' = args ++ maybe [] (one . toText) updateArchive
        exitCode <- case runnerPath of
            Nothing -> runUpdaterProc path args'
            Just rp -> do
                -- Write the bat script and pass it the updater with all args
                liftIO $ writeWindowsUpdaterRunner $ rp
                -- The script will terminate this updater so this function shouldn't return
                runUpdaterProc rp ((toText path):args')
        TL.putStr $ format ("The updater has exited with "%text%"\n") (show exitCode)
        when (exitCode == ExitSuccess) $ do
            -- this will throw an exception if the file doesn't exist but
            -- hopefully if the updater has succeeded it *does* exist
            whenJust updateArchive removeFile

runUpdaterProc :: HasConfigurations => FilePath -> [Text] -> IO ExitCode
runUpdaterProc path args = do
    let cr = (Process.proc (toString path) (map toString args))
                 { Process.std_in  = Process.CreatePipe
                 , Process.std_out = Process.CreatePipe
                 , Process.std_err = Process.CreatePipe
                 }
    phvar <- newEmptyMVar
    system' phvar cr mempty

writeWindowsUpdaterRunner :: FilePath -> IO ()
writeWindowsUpdaterRunner runnerPath = do
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
    :: HasConfigurations
    => (FilePath, [Text], Maybe FilePath)
    -> IO (ProcessHandle, Async ExitCode, FilePath)
spawnNode (path, args, mbLogPath) = do
    putText "Starting the node"
    -- We don't explicitly close the `logHandle` here,
    -- but this will be done when we run the `CreateProcess` built
    -- by proc later is `system'`:
    -- http://hackage.haskell.org/package/process-1.6.1.0/docs/System-Process.html#v:createProcess
    (logPath, logHandle) <- case mbLogPath of
        Just lp -> do
            createDirectoryIfMissing True (directory lp)
            (lp,) <$> openFile lp AppendMode
        Nothing -> do
            tempdir <- liftIO (fromString <$> getTemporaryDirectory)
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
        Nothing -> error "couldn't run the node (it didn't start after 10s)"
        Just ph -> do
            putText "Node started"
            return (ph, asc, logPath)

runWallet :: (FilePath, [Text]) -> IO ExitCode
runWallet (path, args) = do
    putText "Starting the wallet"
    view _1 <$> readProcessWithExitCode path (map toString args) mempty

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
    :: (HasConfigurations, HasCompileInfo, MonadIO m)
    => ExitCode        -- ^ Exit code of the node
    -> Maybe FilePath  -- ^ Path to the logger config
    -> String          -- ^ URL of the server
    -> FilePath        -- ^ Path to the stdout log
    -> m ()
reportNodeCrash exitCode logConfPath reportServ logPath = liftIO $ do
    logConfig <- readLoggerConfig (toString <$> logConfPath)
    let logFileNames =
            map ((fromMaybe "" (logConfig ^. lcFilePrefix) </>) . snd) $
            retrieveLogFiles logConfig
    let logFiles = filter (".pub" `isSuffixOf`) logFileNames
    let ec = case exitCode of
            ExitSuccess   -> 0
            ExitFailure n -> n
    sendReport (normalise logPath:logFiles) [] (RCrash ec) "cardano-node" reportServ

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
