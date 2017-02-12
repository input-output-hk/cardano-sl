{-# LANGUAGE ScopedTypeVariables #-}

-- | Methods of reporting different unhealthy behaviour to server.

module Pos.Reporting.Methods
       ( sendReportNode
       , getNodeInfo
       , reportMisbehaviour
       , sendReport
       , retrieveLogFiles
       , chooseLogFiles
       ) where

import           Control.Exception        (SomeException)
import           Control.Lens             (to)
import           Control.Monad.Catch      (try)
import           Data.Aeson               (encode)
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T
import           Data.Time.Clock          (getCurrentTime)
import           Data.Version             (Version (..))
import           Formatting               (build, sformat, stext, (%))
import           Network.Info             (IPv4 (..), getNetworkInterfaces, ipv4)
import           Network.Wreq             (partFile, partLBS, post)
import           Paths_cardano_sl         (version)
import           Pos.ReportServer.Report  (ReportInfo (..), ReportType (..))
import           Serokell.Util.Text       (listBuilderJSON, listJson)
import           System.Directory         (doesFileExist, listDirectory)
import           System.FilePath          (dropExtension, takeDirectory, takeExtension,
                                           takeFileName, (<.>), (</>))
import           System.Info              (arch, os)
import           System.Wlog              (CanLog, HasLoggerName, LoggerConfig (..),
                                           lcFilePrefix, lcTree, logDebug, ltFile,
                                           ltSubloggers)
import           Universum

import           Pos.Context              (WithNodeContext, getNodeContext,
                                           ncLoggerConfig, ncNodeParams, npReportServers)
import           Pos.DHT.Model            (MonadDHT, currentNodeKey, getKnownPeers)
import           Pos.Reporting.Exceptions (ReportingError (..))

----------------------------------------------------------------------------
-- Node-specific
----------------------------------------------------------------------------

-- | Sends node's logs, taking 'LoggerConfig' from 'NodeContext',
-- retrieving all logger files from it. List of servers is also taken
-- from node's configuration.
sendReportNode
    :: ( MonadIO m
       , MonadCatch m
       , WithNodeContext її m
       )
    => (([Text],FilePath) -> Bool) -> ReportType -> m ()
sendReportNode predicate reportType = do
    servers <- npReportServers . ncNodeParams <$> getNodeContext
    logConfig <- ncLoggerConfig <$> getNodeContext
    let logFileNames =
            map snd $ filter predicate $ retrieveLogFiles logConfig
    logFiles <- concat <$> mapM chooseLogFiles logFileNames
    forM_ servers $
        sendReport logFiles reportType "cardano-node" version . T.unpack

-- checks if ipv4 is from local range
ipv4Local :: Word32 -> Bool
ipv4Local w =
    or [b1 == 10, b1 == 172 && b2 >= 16 && b2 <= 31, b1 == 192 && b2 == 168]
  where
    b1 = w .&. 0xff
    b2 = (w `shiftR` 8) .&. 0xff

-- | Retrieves node info that we would like to know when analyzing
-- malicious behavior of node.
getNodeInfo :: (MonadDHT m, MonadIO m) => m Text
getNodeInfo = do
    peers <- getKnownPeers
    key <- currentNodeKey
    (ips :: [Text]) <-
        map show . filter ipExternal . map ipv4 <$>
        liftIO getNetworkInterfaces
    pure $ sformat outputF (pretty $ listBuilderJSON ips) key peers
  where
    ipExternal (IPv4 w) =
        not $ ipv4Local w || w == 0 || w == 16777343 -- the last is 127.0.0.1
    outputF = ("{ nodeParams: '"%stext%":"%build%"', otherNodes: "%listJson%" }")

-- | Reports misbehaviour given reason string. Effectively designed
-- for 'WorkMode' context.
reportMisbehaviour
    :: ( MonadIO m
       , MonadCatch m
       , MonadDHT m
       , WithNodeContext її m
       , HasLoggerName m
       , CanLog m
       )
    => Text -> m ()
reportMisbehaviour reason = do
    logDebug $ "Reporting misbehaviour \"" <> reason <> "\""
    nodeInfo <- getNodeInfo
    sendReportNode (const False) $ RMisbehavior $ sformat misbehF reason nodeInfo
  where
    misbehF = stext%", nodeInfo: "%stext

----------------------------------------------------------------------------
-- General purpose
----------------------------------------------------------------------------

-- | Given logs files list and report type, sends reports to URI
-- asked. All files _must_ exist. Report server URI should be in form
-- like "http(s)://host:port/" without specified endpoint.
sendReport
    :: (MonadIO m, MonadCatch m)
    => [FilePath] -> ReportType -> Text -> Version -> String -> m ()
sendReport logFiles reportType appName appVersion reportServerUri = do
    curTime <- liftIO getCurrentTime
    existingFiles <- filterM (liftIO . doesFileExist) logFiles
    when (null existingFiles && not (null logFiles)) $
        throwM $ CantRetrieveLogs logFiles
    e <- try $ liftIO $ post (reportServerUri </> "report") $
        partLBS "payload" (encode $ reportInfo curTime existingFiles) :
        map (\fp -> partFile (toFileName fp) fp) existingFiles
    whenLeft e $ \(e' :: SomeException) -> throwM $ SendingError (show e')
  where
    toFileName = T.pack . takeFileName
    reportInfo curTime files =
        ReportInfo
        { rApplication = appName
        , rVersion = appVersion
        , rBuild = 0 -- what should be put here?
        , rOS = T.pack (os <> "-" <> arch)
        , rLogs = map toFileName files
        , rDate = curTime
        , rReportType = reportType
        }

-- | Given logger config, retrieves all (logger name, filepath) for
-- every logger that has file handle.
retrieveLogFiles :: LoggerConfig -> [([Text], FilePath)]
retrieveLogFiles lconfig =
    map (second (prefix </>)) $ fromLogTree $ lconfig ^. lcTree
  where
    prefix = lconfig ^. lcFilePrefix . to (fromMaybe ".")
    fromLogTree lt =
        let curElem =
                case lt ^. ltFile of
                    Just filepath -> [([], filepath)]
                    Nothing       -> []
            addFoo (part, node) = map (first (part :)) $ fromLogTree node
        in curElem ++ concatMap addFoo (lt ^. ltSubloggers . to HM.toList)

-- | Retrieves real filepathes of logs given filepathes from log
-- description. Example: there's @component.log@ in config, but this
-- function will return @[component.log.122, component.log.123]@.
chooseLogFiles :: (MonadIO m) => FilePath -> m [FilePath]
chooseLogFiles filePath = liftIO $ do
    dirContents <- map (dir </>) <$> listDirectory dir
    dirFiles <- filterM doesFileExist dirContents
    let fileMatches = fileName `elem` map takeFileName dirFiles
    let samePrefix = filter (isPrefixOf fileName . takeFileName) dirFiles
    let rotationLogs :: [(FilePath, Int)]
        rotationLogs = flip mapMaybe samePrefix $ \candidate -> do
            let fname = takeFileName candidate
            let basename = dropExtension fname
            let ext = drop 1 $ takeExtension fname
            guard $ basename == fileName
            guard $ fname == basename <.> ext
            (candidate,) <$> readMaybe ext
    pure $ if | not (null rotationLogs) ->
                take 2 $ map fst $ reverse $ sortOn snd rotationLogs
              | fileMatches -> [filePath]
              | otherwise -> [] -- haven't found any logs
  where
    fileName = takeFileName filePath
    dir = takeDirectory filePath
