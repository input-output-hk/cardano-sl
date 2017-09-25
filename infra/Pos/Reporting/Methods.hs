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
       ) where

import           Universum

import           Control.Exception                     (ErrorCall (..), Exception (..))
import           Control.Lens                          (each, to)
import           Control.Monad.Catch                   (try)
import           Data.Aeson                            (encode)
import           Data.Bits                             (Bits (..))
import qualified Data.HashMap.Strict                   as HM
import           Data.List                             (isSuffixOf)
import qualified Data.List.NonEmpty                    as NE
import qualified Data.Text.IO                          as TIO
import           Data.Time.Clock                       (getCurrentTime)
import           Formatting                            (sformat, shown, stext, string,
                                                        (%))
import           Network.HTTP.Client                   (httpLbs, newManager,
                                                        parseUrlThrow)
import qualified Network.HTTP.Client.MultipartFormData as Form
import           Network.HTTP.Client.TLS               (tlsManagerSettings)
import           Network.Info                          (IPv4 (..), getNetworkInterfaces,
                                                        ipv4)
import           Pos.ReportServer.Report               (ReportInfo (..), ReportType (..))
import           Serokell.Util.Text                    (listBuilderJSON)
import           System.Directory                      (doesFileExist)
import           System.FilePath                       (takeFileName)
import           System.Info                           (arch, os)
import           System.IO                             (hClose)
import           System.Wlog                           (LoggerConfig (..), Severity (..),
                                                        WithLogger, hwFilePath, lcTree,
                                                        logError, logInfo, logMessage,
                                                        logWarning, ltFiles, ltSubloggers,
                                                        retrieveLogContent)

import           Paths_cardano_sl_infra                (version)
import           Pos.Core.Configuration                (HasConfiguration, protocolMagic)
import           Pos.Core.Types                        (ProtocolMagic (..))
import           Pos.Exception                         (CardanoFatalError)
import           Pos.KnownPeers                        (MonadFormatPeers (..))
import           Pos.Reporting.Exceptions              (ReportingError (..))
import           Pos.Reporting.MemState                (HasLoggerConfig (..),
                                                        HasReportServers (..),
                                                        HasReportingContext (..))
import           Pos.Util.Util                         (maybeThrow, withSystemTempFile,
                                                        (<//>))

type MonadReporting ctx m =
       ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , MonadFormatPeers m
       , HasReportingContext ctx
       , WithLogger m
       , HasConfiguration
       )

----------------------------------------------------------------------------
-- Node-specific
----------------------------------------------------------------------------

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
            sendReportNodeImpl (reverse logContent) reportType
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
sendReportNodeNologs = sendReportNodeImpl []

sendReportNodeImpl
    :: (MonadReporting ctx m)
    => [Text] -> ReportType -> m ()
sendReportNodeImpl rawLogs reportType = do
    servers <- view (reportingContext . reportServers)
    when (null servers) onNoServers
    errors <- fmap lefts $ forM servers $ try .
        sendReport [] rawLogs reportType "cardano-node" . toString
    whenNotNull errors $ throwSE . NE.head
  where
    onNoServers =
        logInfo $ "sendReportNodeImpl: not sending report " <>
                  "because no reporting servers are specified"
    throwSE (e :: SomeException) = throwM e

-- checks if ipv4 is from local range
ipv4Local :: Word32 -> Bool
ipv4Local w =
    or [b1 == 10, b1 == 172 && b2 >= 16 && b2 <= 31, b1 == 192 && b2 == 168]
  where
    b1 = w .&. 0xff
    b2 = (w `shiftR` 8) .&. 0xff

-- | Retrieves node info that we would like to know when analyzing
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

extendRTDesc :: Text -> ReportType -> ReportType
extendRTDesc text (RError reason) = RError $ reason <> text
extendRTDesc text (RMisbehavior isCritical reason) = RMisbehavior isCritical $ reason <> text
extendRTDesc text' (RInfo text) = RInfo $ text <> text'
extendRTDesc _ x = x

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

-- | Report «misbehavior», i. e. a situation when something is globally
-- wrong, not only with our node. 'Bool' argument determines whether
-- misbehavior is critical and requires immediate response.
reportMisbehaviour
    :: forall ctx m . (MonadReporting ctx m)
    => Bool -> Text -> m ()
reportMisbehaviour isCritical = reportNode True True . RMisbehavior isCritical

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
-- General purpose
----------------------------------------------------------------------------

-- | Given logs files list and report type, sends reports to URI
-- asked. All files __must__ exist. Report server URI should be in form
-- like "http(s)://host:port/" without specified endpoint.
--
-- __Important notice__: if given paths are logs that we're currently
-- writing to using this executable (or forked thread), you'll get an
-- error, because there's no possibility to have two handles on the
-- same file, see 'System.IO' documentation on handles. Use second
-- parameter for that.
sendReport
    :: (HasConfiguration, MonadIO m, MonadMask m)
    => [FilePath]                 -- ^ Log files to read from
    -> [Text]                     -- ^ Raw log text (optional)
    -> ReportType
    -> Text
    -> String
    -> m ()
sendReport logFiles rawLogs reportType appName reportServerUri = do
    curTime <- liftIO getCurrentTime
    existingFiles <- filterM (liftIO . doesFileExist) logFiles
    when (null existingFiles && not (null logFiles)) $
        throwM $ CantRetrieveLogs logFiles
    putText $ "Rawlogs size is: " <> show (length rawLogs)
    withSystemTempFile "main.log" $ \tempFp tempHandle -> liftIO $ do
        for_ rawLogs $ TIO.hPutStrLn tempHandle
        hClose tempHandle
        rq0 <- parseUrlThrow $ reportServerUri <//> "report"
        let memlogFiles = bool [tempFp] [] (null rawLogs)
        let memlogPart = map partFile' memlogFiles
        let pathsPart = map partFile' existingFiles
        let payloadPart =
                Form.partLBS "payload"
                (encode $ mkReportInfo curTime $ existingFiles ++ memlogFiles)
        -- If performance will ever be a concern, moving to a global manager
        -- should help a lot.
        reportManager <- newManager tlsManagerSettings

        -- Assemble the `Request` out of the Form data.
        rq <- Form.formDataBody (payloadPart : (memlogPart ++ pathsPart)) rq0

        -- Actually perform the HTTP `Request`.
        e  <- try $ httpLbs rq reportManager
        whenLeft e $ \(e' :: SomeException) -> throwM $ SendingError e'
  where
    partFile' fp = Form.partFile (toFileName fp) fp
    toFileName = toText . takeFileName
    mkReportInfo curTime files =
        ReportInfo
        { rApplication = appName
        -- We are using version of 'cardano-sl-infra' here. We agreed
        -- that the version of 'cardano-sl' and it subpackages should
        -- be same.
        , rVersion = version
        , rBuild = 0 -- what should be put here?
        , rOS = toText (os <> "-" <> arch)
        , rLogs = map toFileName files
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
            addFoo (part, node) = map (first (part :)) $ fromLogTree node
        in curElems ++ concatMap addFoo (lt ^. ltSubloggers . to HM.toList)

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
    case tryCast @CardanoFatalError <|> tryCast @ErrorCall of
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
