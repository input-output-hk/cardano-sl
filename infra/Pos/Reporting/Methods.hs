{-# LANGUAGE ScopedTypeVariables #-}

-- | Methods of reporting different unhealthy behaviour to server.

module Pos.Reporting.Methods
       ( sendReportNode
       , sendReportNodeNologs
       , getNodeInfo
       , reportMisbehaviour
       , reportMisbehaviourMasked
       , reportingFatal
       , sendReport
       , retrieveLogFiles
       ) where

import           Universum

import           Control.Exception        (ErrorCall (..), SomeException)
import           Control.Lens             (each, to)
import           Control.Monad.Catch      (try)
import           Data.Aeson               (encode)
import           Data.Bits                (Bits (..))
import qualified Data.HashMap.Strict      as HM
import           Data.List                (isSuffixOf)
import qualified Data.List.NonEmpty       as NE
import qualified Data.Text.IO             as TIO
import           Data.Time.Clock          (getCurrentTime)
import           Data.Version             (Version (..))
import qualified Ether
import           Formatting               (sformat, shown, stext, (%))
import           Network.Info             (IPv4 (..), getNetworkInterfaces, ipv4)
import           Network.Wreq             (partFile, partLBS, post)
import           Pos.ReportServer.Report  (ReportInfo (..), ReportType (..))
import           Serokell.Util.Exceptions (throwText)
import           Serokell.Util.Text       (listBuilderJSON, listJson)
import           System.Directory         (doesFileExist)
import           System.FilePath          (takeFileName)
import           System.Info              (arch, os)
import           System.IO                (hClose)
import           System.IO.Temp           (withSystemTempFile)
import           System.Wlog              (CanLog, HasLoggerName, LoggerConfig (..),
                                           hwFilePath, lcTree, logDebug, logError,
                                           ltFiles, ltSubloggers, retrieveLogContent)

import           Pos.Core.Constants       (protocolMagic)
import           Pos.Discovery.Class      (MonadDiscovery, getPeers)
import           Pos.Exception            (CardanoFatalError)
import           Pos.Reporting.Exceptions (ReportingError (..))
import           Pos.Reporting.MemState   (MonadReportingMem, rcLoggingConfig,
                                           rcReportServers)

-- TODO From Pos.Util, remove after refactoring.
-- | Concatenates two url part using regular slash '/'.
-- E.g. @"./dir/" <//> "/file" = "./dir/file"@.
(<//>) :: String -> String -> String
(<//>) lhs rhs = lhs' ++ "/" ++ rhs'
  where
    isSlash = (== '/')
    lhs' = reverse $ dropWhile isSlash $ reverse lhs
    rhs' = dropWhile isSlash rhs

----------------------------------------------------------------------------
-- Node-specific
----------------------------------------------------------------------------

-- | Sends node's logs, taking 'LoggerConfig' from 'NodeContext',
-- retrieving all logger files from it. List of servers is also taken
-- from node's configuration.
sendReportNode
    :: (MonadIO m, MonadMask m, MonadReportingMem m)
    => Version -> ReportType -> m ()
sendReportNode version reportType = do
    logConfig <- Ether.asks' (view rcLoggingConfig)
    let allFiles = map snd $ retrieveLogFiles logConfig
    logFile <- maybe
        (throwText "sendReportNode: can't find any .pub file in logconfig")
        pure
        (head $ filter (".pub" `isSuffixOf`) allFiles)
    logContent <-
        takeGlobalSize charsConst <$>
        retrieveLogContent logFile (Just 5000)
    sendReportNodeImpl (reverse logContent) version reportType
  where
    -- 2 megabytes, assuming we use chars which are ASCII mostly
    charsConst :: Int
    charsConst = 1024 * 1024 * 2
    takeGlobalSize :: Int -> [Text] -> [Text]
    takeGlobalSize _ []            = []
    takeGlobalSize curLimit (t:xs) =
        let delta = curLimit - length t
        in bool [] (t:(takeGlobalSize delta xs)) (delta > 0)

-- | Same as 'sendReportNode', but doesn't attach any logs.
sendReportNodeNologs
    :: (MonadIO m, MonadMask m, MonadReportingMem m)
    => Version -> ReportType -> m ()
sendReportNodeNologs = sendReportNodeImpl []

sendReportNodeImpl
    :: (MonadIO m, MonadMask m, MonadReportingMem m)
    => [Text] -> Version -> ReportType -> m ()
sendReportNodeImpl rawLogs version reportType = do
    servers <- Ether.asks' (view rcReportServers)
    errors <- fmap lefts $ forM servers $ try .
        sendReport [] rawLogs reportType "cardano-node" version . toString
    whenNotNull errors $ throwSE . NE.head
  where
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
getNodeInfo :: (MonadIO m, MonadDiscovery m) => m Text
getNodeInfo = do
    peers <- getPeers
    (ips :: [Text]) <-
        map show . filter ipExternal . map ipv4 <$>
        liftIO getNetworkInterfaces
    pure $ sformat outputF (pretty $ listBuilderJSON ips) peers
  where
    ipExternal (IPv4 w) =
        not $ ipv4Local w || w == 0 || w == 16777343 -- the last is 127.0.0.1
    outputF = ("{ nodeParams: '"%stext%"', otherNodes: "%listJson%" }")

type ReportingWorkMode m =
       ( MonadIO m
       , MonadMask m
       , MonadReportingMem m
       , HasLoggerName m
       , MonadDiscovery m
       , CanLog m
       )

-- | Reports misbehaviour given reason string. Effectively designed
-- for 'WorkMode' context.
reportMisbehaviour
    :: forall m . ReportingWorkMode m
    => Version -> Text -> m ()
reportMisbehaviour version reason = do
    logError $ "Reporting misbehaviour \"" <> reason <> "\""
    nodeInfo <- getNodeInfo
    sendReportNode version $ RMisbehavior $ sformat misbehF reason nodeInfo
  where
    misbehF = stext%", nodeInfo: "%stext

-- | Report misbehaviour, but catch all errors inside
--
--   FIXME very misleading name. Suggests reporting misbehaviours while
--   asynchronous exceptions are masked.
--
--   FIXME catch and squelch *all* exceptions? Probably a bad idea.
reportMisbehaviourMasked
    :: forall m . ReportingWorkMode m
    => Version -> Text -> m ()
reportMisbehaviourMasked version reason =
    reportMisbehaviour version reason `catch` handler
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
    :: forall m a . ReportingWorkMode m
    => Version -> m a -> m a
reportingFatal version action =
    action `catch` handler1 `catch` handler2
  where
    andThrow :: (Exception e, MonadThrow n) => (e -> n x) -> e -> n y
    andThrow foo e = foo e >> throwM e
    report reason = do
        logDebug $ "Reporting error \"" <> reason <> "\""
        let errorF = stext%", nodeInfo: "%stext
        nodeInfo <- getNodeInfo
        sendReportNode version (RError $ sformat errorF reason nodeInfo) `catch`
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
-- asked. All files _must_ exist. Report server URI should be in form
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
    -> Version
    -> String
    -> m ()
sendReport logFiles rawLogs reportType appName appVersion reportServerUri = do
    curTime <- liftIO getCurrentTime
    existingFiles <- filterM (liftIO . doesFileExist) logFiles
    when (null existingFiles && not (null logFiles)) $
        throwM $ CantRetrieveLogs logFiles
    putText $ "Rawlogs size is: " <> show (length rawLogs)
    withSystemTempFile "main.log" $ \tempFp tempHandle -> liftIO $ do
        for_ rawLogs $ TIO.hPutStrLn tempHandle
        hClose tempHandle
        let memlogFiles = bool [tempFp] [] (null rawLogs)
        let memlogPart = map partFile' memlogFiles
        let pathsPart = map partFile' existingFiles
        let payloadPart =
                partLBS "payload"
                (encode $ reportInfo curTime $ existingFiles ++ memlogFiles)
        e <- try $ liftIO $ post (reportServerUri <//> "report") $
             payloadPart : (memlogPart ++ pathsPart)
        whenLeft e $ \(e' :: SomeException) -> throwM $ SendingError (show e')
  where
    partFile' fp = partFile (toFileName fp) fp
    toFileName = toText . takeFileName
    reportInfo curTime files =
        ReportInfo
        { rApplication = appName
        , rVersion = appVersion
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
