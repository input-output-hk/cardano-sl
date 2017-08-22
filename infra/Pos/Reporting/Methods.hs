
-- | Methods of reporting different unhealthy behaviour to server.

module Pos.Reporting.Methods
       ( sendReportNode
       , sendReportNodeNologs
       , getNodeInfo
       , reportMisbehaviour
       , reportMisbehaviourSilent
       , reportingFatal
       , sendReport
       , retrieveLogFiles
       ) where

import           Universum

import           Control.Exception                     (ErrorCall (..), SomeException)
import           Control.Lens                          (each, to)
import           Control.Monad.Catch                   (try)
import           Data.Aeson                            (encode)
import           Data.Bits                             (Bits (..))
import qualified Data.HashMap.Strict                   as HM
import           Data.List                             (isSuffixOf)
import qualified Data.List.NonEmpty                    as NE
import qualified Data.Text.IO                          as TIO
import           Data.Time.Clock                       (getCurrentTime)
import           Formatting                            (sformat, shown, stext, (%))
import           Network.HTTP.Client                   (httpLbs, newManager,
                                                        parseUrlThrow)
import qualified Network.HTTP.Client.MultipartFormData as Form
import           Network.HTTP.Client.TLS               (tlsManagerSettings)
import           Network.Info                          (IPv4 (..), getNetworkInterfaces,
                                                        ipv4)
import           Pos.ReportServer.Report               (ReportInfo (..), ReportType (..))
import           Serokell.Util.Exceptions              (TextException (..))
import           Serokell.Util.Text                    (listBuilderJSON)
import           System.Directory                      (doesFileExist)
import           System.FilePath                       (takeFileName)
import           System.Info                           (arch, os)
import           System.IO                             (hClose)
import           System.Wlog                           (LoggerConfig (..), WithLogger,
                                                        hwFilePath, lcTree, logDebug,
                                                        logError, logInfo, ltFiles,
                                                        ltSubloggers, retrieveLogContent)

import           Paths_cardano_sl_infra                (version)
import           Pos.Core.Constants                    (protocolMagic)
import           Pos.Exception                         (CardanoFatalError)
import           Pos.KnownPeers                        (MonadFormatPeers (..))
import           Pos.Reporting.Exceptions              (ReportingError (..))
import           Pos.Reporting.MemState                (HasLoggerConfig (..),
                                                        HasReportServers (..),
                                                        HasReportingContext (..))
import           Pos.Util.Util                         (maybeThrow, withSystemTempFile)

-- TODO From Pos.Util, remove after refactoring.
-- | Concatenates two url part using regular slash '/'.
-- E.g. @"./dir/" <//> "/file" = "./dir/file"@.
(<//>) :: String -> String -> String
(<//>) lhs rhs = lhs' ++ "/" ++ rhs'
  where
    isSlash = (== '/')
    lhs' = reverse $ dropWhile isSlash $ reverse lhs
    rhs' = dropWhile isSlash rhs

type MonadReporting ctx m =
       ( MonadIO m
       , MonadMask m
       , MonadReader ctx m
       , MonadFormatPeers m
       , HasReportingContext ctx
       , WithLogger m
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
    if noServers then onNoServers else do
        logConfig <- view (reportingContext . loggerConfig)
        let allFiles = map snd $ retrieveLogFiles logConfig
        logFile <-
            maybeThrow (TextException onNoPubfiles)
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
    takeGlobalSize _ []            = []
    takeGlobalSize curLimit (t:xs) =
        let delta = curLimit - length t
        in bool [] (t:(takeGlobalSize delta xs)) (delta > 0)
    onNoServers =
        logInfo $ "sendReportNode: not sending report " <>
                  "because no reporting servers are specified"
    onNoPubfiles = "sendReportNode: can't find any .pub file in logconfig. " <>
        "Most probably public logging is misconfigured. Either set reporting " <>
        "servers to [] or include .pub files in log config"

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
    outputF = ("{ nodeParams: '"%stext%"', peers: '"%stext%"' }")


-- | Reports misbehaviour given reason string. Effectively designed
-- for 'WorkMode' context.
reportMisbehaviour
    :: (MonadReporting ctx m)
    => Bool -> Text -> m ()
reportMisbehaviour isCritical reason = do
    logError $ "Reporting misbehaviour \"" <> reason <> "\""
    nodeInfo <- getNodeInfo
    sendReportNode $
        RMisbehavior isCritical $ sformat misbehF reason nodeInfo
  where
    misbehF = stext%", nodeInfo: "%stext

-- FIXME catch and squelch *all* exceptions? Probably a bad idea.
-- | Report misbehaviour, but catch all errors inside
reportMisbehaviourSilent
    :: forall ctx m . (MonadReporting ctx m)
    => Bool -> Text -> m ()
reportMisbehaviourSilent isCritical reason =
    reportMisbehaviour isCritical reason `catch` handler
  where
    handler :: SomeException -> m ()
    handler e =
        logError $
        sformat ("Didn't manage to report misbehaveour "%stext%
                 " because of exception "%shown) reason e

-- | Execute action, report 'CardanoFatalError' and 'FatalError' if it
-- happens and rethrow. Errors related to reporting itself are caught,
-- logged and ignored.
reportingFatal
    :: forall ctx m a . (MonadReporting ctx m)
    => m a -> m a
reportingFatal action =
    action `catch` handler1 `catch` handler2
  where
    andThrow :: (Exception e, MonadThrow n) => (e -> n x) -> e -> n y
    andThrow foo e = foo e >> throwM e
    report reason = do
        logDebug $ "Reporting error \"" <> reason <> "\""
        let errorF = stext%", nodeInfo: "%stext
        nodeInfo <- getNodeInfo
        sendReportNode (RError $ sformat errorF reason nodeInfo) `catch`
            handlerSend reason
    handlerSend reason (e :: SomeException) =
        logError $
        sformat ("Didn't manage to report error "%stext%
                 " because of exception '"%shown%"' raised while sending") reason e
    handler1 = andThrow $ \(e :: CardanoFatalError) ->
        report (pretty e)
    handler2 = andThrow $ \(ErrorCall reason) ->
        report ("FatalError/error: " <> show reason)


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
    :: (MonadIO m, MonadMask m)
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
                (encode $ reportInfo curTime $ existingFiles ++ memlogFiles)
        -- If performance will ever be a concern, moving to a global manager
        -- should help a lot.
        reportManager <- newManager tlsManagerSettings

        -- Assemble the `Request` out of the Form data.
        rq <- Form.formDataBody (payloadPart : (memlogPart ++ pathsPart)) rq0

        -- Actually perform the HTTP `Request`.
        e  <- try $ httpLbs rq reportManager
        whenLeft e $ \(e' :: SomeException) -> throwM $ SendingError (show e')
  where
    partFile' fp = Form.partFile (toFileName fp) fp
    toFileName = toText . takeFileName
    reportInfo curTime files =
        ReportInfo
        { rApplication = appName
        -- We are using version of 'cardano-sl-infra' here. We agreed
        -- that the version of 'cardano-sl' and it subpackages should
        -- be same.
        , rVersion = version
        , rBuild = 0 -- what should be put here?
        , rOS = toText (os <> "-" <> arch)
        , rLogs = map toFileName files
        , rMagic = protocolMagic
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
