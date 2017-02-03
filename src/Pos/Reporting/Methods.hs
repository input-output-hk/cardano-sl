{-# LANGUAGE ScopedTypeVariables #-}

-- | Methods of reporting different unhealthy behaviour to server.

module Pos.Reporting.Methods
       ( sendReportNode
       , sendReport
       , retrieveLogFiles
       , retrieveLogs
       ) where

import           Control.Exception        (SomeException)
import           Control.Monad.Catch      (try)
import           Data.Aeson               (encode)
import qualified Data.Text                as T
import           Data.Time.Clock          (getCurrentTime)
import           Data.Version             (Version (..))
import           Network.Wreq             (partFile, partLBS, post)
import           Pos.ReportServer.Report  (ReportInfo (..), ReportType (..))
import           System.Directory         (doesFileExist)
import           System.FilePath          (takeFileName)
import           System.Info              (arch, os)
import           System.Wlog              (LoggerConfig)
import           Universum

import           Pos.Constants            (curSoftwareVersion, ourAppName)
import           Pos.Context              (WithNodeContext, getNodeContext,
                                           ncLoggerConfig, ncReportServers)
import           Pos.Reporting.Exceptions (ReportingError (..))
import           Pos.Types                (svNumber)

sendReportNode
    :: (MonadIO m, MonadCatch m, WithNodeContext її m)
    => ReportType -> m ()
sendReportNode reportType = do
    servers <- ncReportServers <$> getNodeContext
    logConfig <- ncLoggerConfig <$> getNodeContext
    let logFileNames = map snd $ retrieveLogFiles logConfig
    -- put filter here, we don't want to send all logs
    let logFiles = filter (const True) logFileNames
    forM_ servers $ sendReport logFiles reportType . T.unpack

-- | Given logs files list and report type, sends reports to URI
-- asked. All files _must_ exist.
sendReport
    :: (MonadIO m, MonadCatch m)
    => [FilePath] -> ReportType -> String -> m ()
sendReport logFiles reportType reportServerUri = do
    curTime <- liftIO getCurrentTime
    existingFiles <- filterM (liftIO . doesFileExist) logFiles
    when (null existingFiles) $ throwM $ CantRetrieveLogs logFiles
    e <- try $ liftIO $ post reportServerUri $
        partLBS "payload" (encode $ reportInfo curTime existingFiles) :
        map (\fp -> partFile (toFileName fp) fp) existingFiles
    whenLeft e $ \(e' :: SomeException) -> throwM $ SendingError (show e')
  where
    toFileName = T.pack . takeFileName
    reportInfo curTime files =
        ReportInfo
        { rApplication = pretty ourAppName
        , rVersion = Version [fromIntegral $ svNumber curSoftwareVersion] []
        , rBuild = 0 -- what should be put here?
        , rOS = T.pack (os <> "-" <> arch)
        , rLogs = map toFileName files
        , rDate = curTime
        , rReportType = reportType
        }

-- | Given logger config, retrieves all (logger name, filepath) for
-- every logger that has file handle.
retrieveLogFiles :: LoggerConfig -> [([Text], FilePath)]
retrieveLogFiles = undefined

-- | Reads log file
retrieveLogs :: (MonadIO m) => FilePath -> m (Maybe ByteString)
retrieveLogs = undefined
