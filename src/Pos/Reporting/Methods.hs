{-# LANGUAGE ScopedTypeVariables #-}

-- | Methods of reporting different unhealthy behaviour to server.

module Pos.Reporting.Methods
       ( sendReportNode
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
import           Network.Wreq             (partFile, partLBS, post)
import           Paths_cardano_sl         (version)
import           Pos.ReportServer.Report  (ReportInfo (..), ReportType (..))
import           System.Directory         (doesFileExist, listDirectory)
import           System.FilePath          (dropExtension, takeDirectory, takeExtension,
                                           takeFileName, (<.>), (</>))
import           System.Info              (arch, os)
import           System.Wlog              (LoggerTree (..), lcTree, ltFile, ltSubloggers)
import           Universum

import           Pos.Context              (WithNodeContext, getNodeContext,
                                           ncLoggerConfig, ncNodeParams, npReportServers)
import           Pos.Reporting.Exceptions (ReportingError (..))

-- | Sends node's logs, taking 'LoggerConfig' from 'NodeContext',
-- retrieving all logger files from it. List of servers is also taken
-- from node's configuration.
sendReportNode
    :: (MonadIO m, MonadCatch m, WithNodeContext її m)
    => ReportType -> m ()
sendReportNode reportType = do
    servers <- npReportServers . ncNodeParams <$> getNodeContext
    logConfig <- ncLoggerConfig <$> getNodeContext
    let logFileNames = map snd $ retrieveLogFiles $ logConfig ^. lcTree
    -- put filter here, maybe we don't want to send all logs
    logFiles <- concat <$> mapM chooseLogFiles logFileNames
    forM_ servers $
        sendReport logFiles reportType "cardano-node" version . T.unpack

-- | Given logs files list and report type, sends reports to URI
-- asked. All files _must_ exist. Report server URI should be in form
-- like "http(s)://host:port/" without specified endpoint.
sendReport
    :: (MonadIO m, MonadCatch m)
    => [FilePath] -> ReportType -> Text -> Version -> String -> m ()
sendReport logFiles reportType appName appVersion reportServerUri = do
    curTime <- liftIO getCurrentTime
    existingFiles <- filterM (liftIO . doesFileExist) logFiles
    when (null existingFiles) $ throwM $ CantRetrieveLogs logFiles
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
retrieveLogFiles :: LoggerTree -> [([Text], FilePath)]
retrieveLogFiles lt =
    curElem ++ concatMap addFoo (lt ^. ltSubloggers . to HM.toList)
  where
    curElem =
        case lt ^. ltFile of
            Just filepath -> [([], filepath)]
            Nothing       -> []
    addFoo (part, node) = map (first (part :)) $ retrieveLogFiles node

-- | Retrieves real filepathes of logs given filepathes from log
-- description. Example: there's @component.log@ in config, but this
-- function will return @[component.log.122, component.log.123]@.
chooseLogFiles :: (MonadIO m) => FilePath -> m [FilePath]
chooseLogFiles filePath = liftIO $ do
    dirContents <- map (dir </>) <$> listDirectory dir
    print dirContents
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
