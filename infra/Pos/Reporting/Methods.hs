{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Methods of reporting different unhealthy behaviour to server.

module Pos.Reporting.Methods
       (
         MonadReporting

         -- * Report single event.
       , reportError
       , reportMisbehaviour
       , reportInfo

       -- * Exception handling
       , reportOrLog
       , reportOrLogE
       , reportOrLogW

       -- * Internals, exported for custom usages.
       -- E. g. to report crash from launcher.
       , sendReport
       , retrieveLogFiles
       , compressLogs
       ) where

import           Universum


import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import           Control.Exception (ErrorCall (..))
import           Control.Exception.Safe (Exception (..), try)
import           Control.Lens (each, to)
import           Data.Aeson (encode)
import           Data.Bits (Bits (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Conduit (runConduitRes, yield, (.|))
import           Data.Conduit.List (consume)
import qualified Data.Conduit.Lzma as Lzma
import qualified Data.HashMap.Strict as HM
import           Data.List (isSuffixOf)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as TIO
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Formatting (sformat, shown, stext, string, (%))
import           Network.HTTP.Client (httpLbs, newManager, parseUrlThrow)
import qualified Network.HTTP.Client.MultipartFormData as Form
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.Info (IPv4 (..), getNetworkInterfaces, ipv4)
import           Pos.ReportServer.Report (ReportInfo (..), ReportType (..))
import           Serokell.Util.Text (listBuilderJSON)
import           System.Directory (canonicalizePath, doesFileExist, getTemporaryDirectory,
                                   removeFile)
import           System.FilePath (takeFileName)
import           System.Info (arch, os)
import           System.IO (IOMode (WriteMode), hClose, hFlush, withFile)
import           System.Wlog (LoggerConfig (..), Severity (..), WithLogger, hwFilePath, lcTree,
                              logError, logInfo, logMessage, logWarning, ltFiles, ltSubloggers,
                              retrieveLogContent)


import           Paths_cardano_sl_infra (version)
import           Pos.Crypto (ProtocolMagic (..), HasProtocolMagic, protocolMagic)
import           Pos.DB.Error (DBError (..))
import           Pos.Exception (CardanoFatalError)
import           Pos.KnownPeers (MonadFormatPeers (..))
import           Pos.Network.Types (HasNodeType (..), NodeType (..))
import           Pos.Reporting.Exceptions (ReportingError (..))
import           Pos.Reporting.MemState (HasLoggerConfig (..), HasReportServers (..),
                                         HasReportingContext (..))
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo)
import           Pos.Util.Filesystem (withSystemTempFile)
import           Pos.Util.Util (maybeThrow, (<//>))

----------------------------------------------------------------------------
-- General purpose/low level
----------------------------------------------------------------------------

-- | Given logs files list and report type, sends reports to URI
-- asked. All files __must__ exist. Report server URI should be in form
-- like "http(s)://host:port/" without specified endpoint.
--
-- __Important notice__: if given paths are logs that we're currently
-- writing to using this executable (or forked thread), you'll get an
-- error, because there's no possibility to have two handles on the
-- same file, see 'System.IO' documentation on handles. See
-- 'withTempLogFile' to overcome this problem.
sendReport
    :: (HasProtocolMagic, HasCompileInfo, MonadIO m, MonadMask m)
    => [FilePath]                 -- ^ Log files to read from
    -> ReportType
    -> Text
    -> String
    -> m ()
sendReport logFiles reportType appName reportServerUri = do
    curTime <- liftIO getCurrentTime
    existingFiles <- filterM (liftIO . doesFileExist) logFiles
    when (null existingFiles && not (null logFiles)) $
        throwM $ CantRetrieveLogs logFiles

    rq0 <- parseUrlThrow $ reportServerUri <//> "report"
    let pathsPart = map partFile' existingFiles
    let payloadPart =
            Form.partLBS "payload"
            (encode $ mkReportInfo curTime)
    -- If performance will ever be a concern, moving to a global manager
    -- should help a lot.
    reportManager <- liftIO $ newManager tlsManagerSettings

    -- Assemble the `Request` out of the Form data.
    rq <- Form.formDataBody (payloadPart : pathsPart) rq0

    -- Actually perform the HTTP `Request`.
    e  <- try $ liftIO $ httpLbs rq reportManager
    whenLeft e $ \(e' :: SomeException) -> throwM $ SendingError e'
  where
    partFile' fp = Form.partFile (toFileName fp) fp
    toFileName = toText . takeFileName
    mkReportInfo curTime =
        ReportInfo
        { rApplication = appName
        -- We are using version of 'cardano-sl-infra' here. We agreed
        -- that the version of 'cardano-sl' and it subpackages should
        -- be same.
        , rVersion = version
        , rBuild = pretty compileInfo
        , rOS = toText (os <> "-" <> arch)
        , rMagic = getProtocolMagic protocolMagic
        , rDate = curTime
        , rReportType = reportType
        }


-- | Given logger config, retrieves all (logger name, filepath) for
-- every logger that has file handle. Filepath inside does __not__
-- contain the common logger config prefix.
retrieveLogFiles :: LoggerConfig -> [([Text], FilePath)]
retrieveLogFiles lconfig = fromLogTree $ lconfig ^. lcTree
  where
    fromLogTree lt =
        let curElems = map ([],) (lt ^.. ltFiles . each . hwFilePath)
            iterNext (part, node) = map (first (part :)) $ fromLogTree node
        in curElems ++ concatMap iterNext (lt ^. ltSubloggers . to HM.toList)

-- | Pass a list of absolute paths to log files. This function will
-- archive and compress these files and put resulting file into log
-- directory (returning filepath is absolute).
--
-- It will throw a PackingError in case:
--   - Any of the file paths given does not point to an existing file.
--   - Any of the file paths could not be converted to a tar path, for instance
--     because it is too long.
compressLogs :: (MonadIO m) => [FilePath] -> m FilePath
compressLogs files = liftIO $ do
    tar <- tarPackIndependently files
    tarxz <-
        BS.concat <$>
        runConduitRes (yield tar .| Lzma.compress (Just 0) .| consume)
    aName <- getArchiveName
    withFile aName WriteMode $ \handle -> do
        BS.hPut handle tarxz
        hFlush handle
    pure aName
  where
    tarPackIndependently :: [FilePath] -> IO ByteString
    tarPackIndependently paths = do
        entries <- forM paths $ \p -> do
            unlessM (doesFileExist p) $ throwM $
                PackingError $ "can't pack log file " <> fromString p <>
                               " because it doesn't exist or it's not a file"
            tPath <- either (throwM . PackingError . fromString)
                            pure
                            (Tar.toTarPath False $ takeFileName p)
            pabs <- canonicalizePath p
            Tar.packFileEntry pabs tPath
        pure $ BSL.toStrict $ Tar.write entries
    getArchiveName = liftIO $ do
        curTime <- formatTime defaultTimeLocale "%q" <$> getCurrentTime
        tempDir <- getTemporaryDirectory
        pure $ tempDir <//> ("report-" <> curTime <> ".tar.lzma")

-- | Creates a temp file from given text
withTempLogFile :: (MonadIO m, MonadMask m) => Text -> (FilePath -> m a) -> m a
withTempLogFile rawLogs action = do
    withSystemTempFile "main.log" $ \tempFp tempHandle -> do
        let getArchivePath = liftIO $ do
                TIO.hPutStrLn tempHandle rawLogs
                hClose tempHandle
                archivePath <- compressLogs [tempFp]
                canonicalizePath archivePath
            removeArchive = liftIO . removeFile
        bracket getArchivePath removeArchive action

----------------------------------------------------------------------------
-- Node-specific
----------------------------------------------------------------------------

type MonadReporting ctx m =
       ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , MonadFormatPeers m
       , HasReportingContext ctx
       , HasNodeType ctx
       , WithLogger m
       , HasProtocolMagic
       , HasCompileInfo
       )


-- Common code across node sending: tries to send logs to at least one
-- reporting server.
sendReportNodeImpl
    :: forall ctx m. (MonadReporting ctx m)
    => Maybe Text -> ReportType -> m ()
sendReportNodeImpl rawLogs reportType = do
    servers <- view (reportingContext . reportServers)
    if null servers
    then onNoServers
    else do
        let withPath :: ([FilePath] -> m ()) -> m ()
            withPath =
                maybe (\a -> a [])
                      (\t cont -> withTempLogFile t $ \file -> cont [file])
                      rawLogs
        -- logFile is either [a] or [].
        withPath $ \(logFile :: [FilePath]) -> do
            errors <-
                fmap lefts $ forM servers $
                try . sendReport logFile reportType "cardano-node" . toString
            whenNotNull errors $ throwSE . NE.head
  where
    onNoServers =
        logInfo $ "sendReportNodeImpl: not sending report " <>
                  "because no reporting servers are specified"
    throwSE (e :: SomeException) = throwM e

-- | Sends node's logs, taking 'LoggerConfig' from 'NodeContext',
-- retrieving all logger files from it. List of servers is also taken
-- from node's configuration.
sendReportNode
    :: (MonadReporting ctx m)
    => ReportType -> m ()
sendReportNode reportType = do
    noServers <- null <$> view (reportingContext . reportServers)
    if noServers
        then onNoServers
        else do
            logConfig <- view (reportingContext . loggerConfig)
            let allFiles = map snd $ retrieveLogFiles logConfig
            logFile <-
                maybeThrow
                    NoPubFiles
                    (head $ filter (".pub" `isSuffixOf`) allFiles)
            logContent <-
                takeGlobalSize charsConst <$>
                retrieveLogContent logFile (Just 5000)
            sendReportNodeImpl (Just $ unlines $ reverse logContent) reportType
  where
    -- 2 megabytes, assuming we use chars which are ASCII mostly
    charsConst :: Int
    charsConst = 1024 * 1024 * 2
    takeGlobalSize :: Int -> [Text] -> [Text]
    takeGlobalSize _ [] = []
    takeGlobalSize curLimit (t:xs) =
        let delta = curLimit - length t
        in bool [] (t : (takeGlobalSize delta xs)) (delta > 0)
    onNoServers =
        logInfo $
        "sendReportNode: not sending report " <>
        "because no reporting servers are specified"

-- | Same as 'sendReportNode', but doesn't attach any logs.
sendReportNodeNologs :: (MonadReporting ctx m) => ReportType -> m ()
sendReportNodeNologs = sendReportNodeImpl Nothing

-- Note that we are catching all synchronous exceptions, but don't
-- catch async ones. If reporting is broken, we don't want it to
-- affect anything else.
reportNode
    :: forall ctx m . (MonadReporting ctx m)
    => Bool -> Bool -> ReportType -> m ()
reportNode sendLogs extendWithNodeInfo reportType =
    reportNodeDo `catchAny` handler
  where
    send' = if sendLogs then sendReportNode else sendReportNodeNologs
    reportNodeDo = do
        logReportType reportType
        if extendWithNodeInfo
           then do
              nodeInfo <- getNodeInfo
              send' $ extendRTDesc (", nodeInfo: " <> nodeInfo) reportType
           else send' reportType
    handler :: SomeException -> m ()
    handler e =
        logError $
        sformat ("Didn't manage to report "%shown%
                 " because of exception '"%string%"' raised while sending")
        reportType (displayException e)

    extendRTDesc :: Text -> ReportType -> ReportType
    extendRTDesc text (RError reason)                  = RError $ reason <> text
    extendRTDesc text (RMisbehavior isCritical reason) = RMisbehavior isCritical $ reason <> text
    extendRTDesc text' (RInfo text)                    = RInfo $ text <> text'
    extendRTDesc _ x                                   = x

    logReportType :: WithLogger m => ReportType -> m ()
    logReportType (RCrash i) = logError $ "Reporting crash with code " <> show i
    logReportType (RError reason) =
        logError $ "Reporting error with reason \"" <> reason <> "\""
    logReportType (RMisbehavior True reason) =
        logError $ "Reporting critical misbehavior with reason \"" <> reason <> "\""
    logReportType (RMisbehavior False reason) =
        logWarning $ "Reporting non-critical misbehavior with reason \"" <> reason <> "\""
    logReportType (RInfo text) =
        logInfo $ "Reporting info with text \"" <> text <> "\""
    logReportType (RCustomReport{}) =
        logInfo $ "Reporting custom report"

    -- Retrieves node info that we would like to know when analyzing
    -- malicious behavior of node.
    getNodeInfo :: (MonadIO m, MonadFormatPeers m) => m Text
    getNodeInfo = do
        peersText <- fromMaybe "unknown" <$> formatKnownPeers sformat
        (ips :: [Text]) <-
            map show . filter ipExternal . map ipv4 <$>
            liftIO getNetworkInterfaces
        pure $ sformat outputF (pretty $ listBuilderJSON ips) peersText
      where
        ipExternal (IPv4 w) =
            not $ ipv4Local w || w == 0 || w == 16777343 -- the last is 127.0.0.1
        outputF = ("{ nodeIps: '"%stext%"', peers: '"%stext%"' }")

    -- checks if ipv4 is from local range
    ipv4Local :: Word32 -> Bool
    ipv4Local w =
        or [b1 == 10, b1 == 172 && b2 >= 16 && b2 <= 31, b1 == 192 && b2 == 168]
      where
        b1 = w .&. 0xff
        b2 = (w `shiftR` 8) .&. 0xff

-- | Report «misbehavior», i. e. a situation when something is globally
-- wrong, not only with our node. 'Bool' argument determines whether
-- misbehavior is critical and requires immediate response.
--
-- Misbehaviour is reported only by core nodes, because
-- a) core nodes usually have the most actual vision of the current state;
-- b) reporting misbehaviour from many nodes is redundant.
reportMisbehaviour
    :: forall ctx m . (MonadReporting ctx m)
    => Bool -> Text -> m ()
reportMisbehaviour isCritical message = when False $ do -- TODO: Add a flag wether to report
                                                        -- misbehaviors
    nodeType <- getNodeType <$> ask
    case nodeType of
        NodeCore -> reportNode True True (RMisbehavior isCritical message)
        _        -> pass

-- | Report some general information.
reportInfo
    :: forall ctx m . (MonadReporting ctx m)
    => Bool -> Text -> m ()
reportInfo sendLogs = reportNode sendLogs True . RInfo

-- | Report «error», i. e. a situation when something is wrong with our
-- node, e. g. an assertion failed.
reportError
    :: forall ctx m . (MonadReporting ctx m)
    => Text -> m ()
reportError = reportNode True True . RError

----------------------------------------------------------------------------
-- Exception handling
----------------------------------------------------------------------------

-- | Exception handler which reports (and logs) an exception or just
-- logs it. It reports only few types of exceptions which definitely
-- deserve attention. Other types are simply logged. It's suitable for
-- long-running workers which want to catch all exceptions and restart
-- after delay. If you are catching all exceptions somewhere, you most
-- likely want to use this handler (and maybe do something else).
--
-- NOTE: it doesn't rethrow an exception. If you are sure you need it,
-- you can rethrow it by yourself.
reportOrLog
    :: forall ctx m . (MonadReporting ctx m)
    => Severity -> Text -> SomeException -> m ()
reportOrLog severity prefix exc =
    case tryCast @CardanoFatalError <|> tryCast @ErrorCall <|> tryCast @DBError of
        Just msg -> reportError $ prefix <> msg
        Nothing  -> logMessage severity $ prefix <> pretty exc
  where
    tryCast ::
           forall e. Exception e
        => Maybe Text
    tryCast = toText . displayException <$> fromException @e exc

-- | A version of 'reportOrLog' which uses 'Error' severity.
reportOrLogE
    :: forall ctx m . (MonadReporting ctx m)
    => Text -> SomeException -> m ()
reportOrLogE = reportOrLog Error

-- | A version of 'reportOrLog' which uses 'Warning' severity.
reportOrLogW
    :: forall ctx m . (MonadReporting ctx m)
    => Text -> SomeException -> m ()
reportOrLogW = reportOrLog Warning
