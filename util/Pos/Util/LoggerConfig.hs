{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Pos.Util.LoggerConfig
       ( LoggerConfig(..)
       , RotationParameters(..)
       , loadLogConfig
       , parseLoggerConfig
       , retrieveLogFiles
       ) where

import           Data.Yaml      as Y
import           GHC.Generics
import           Universum

import           System.FilePath (normalise)

import           Pos.Util.Log.Severity


-- | @'RotationParameters'@ one of the two categories  used in the
--   logging config, specifying the log rotation parameters
data RotationParameters = RotationParameters
    { _rpLogLimit  :: !Word64  -- ^ max size of file in bytes
    , _rpKeepFiles :: !Word    -- ^ number of files to keep
    }
    deriving (Generic, Show)

instance ToJSON RotationParameters
instance FromJSON RotationParameters where
    parseJSON = withObject "rotation params" $ \o -> do
        _rpLogLimit  <- o .: "logLimit"
        _rpKeepFiles <- o .: "keepFiles"
        return RotationParameters{..}

-- | @'LogHandler'@ describes the output handler (file, stdout, ..)
--
-- | Wrapper over file handler with additional rounding option.
data LogHandler = LogHandler
    { _lhName :: !String
      -- ^ name of the handler
    , _lhFpath :: !(Maybe FilePath)
      -- ^ file path
    , _lhBackend :: !String
      -- ^ describes the backend (scribe for katip) to be loaded
    , _lhMinSeverity :: !(Maybe Severity)
      -- ^ the minimum severity to be logged
    } deriving (Generic,Show)

instance ToJSON LogHandler
instance FromJSON LogHandler where
    parseJSON = withObject "log handler" $ \o -> do
        (_lhName :: String) <- o .: "name"
        (_lhFpath :: Maybe FilePath) <- fmap normalise <$> o .:? "filepath"
        (_lhBackend :: String) <- o .: "backend"
        (_lhMinSeverity :: Maybe Severity) <- o .:? "severity"
        pure LogHandler{..}


-- | 'LoggerTree' contains the actual logging configuration,
--   only 'Severity' and 'Files' for now
data LoggerTree = LoggerTree
    {
      _ltMinSeverity :: !Severity
    , _ltHandlers    :: ![LogHandler]
    }
    deriving (Generic, Show)

instance ToJSON LoggerTree
instance FromJSON LoggerTree where
    parseJSON = withObject "logger tree" $ \o -> do
        (singleFile :: Maybe FilePath) <- fmap normalise <$> o .:? "file"
        (manyFiles :: [FilePath]) <- map normalise <$> (o .:? "files" .!= [])
        handlers <- o .:? "handlers" .!= []
        let fileHandlers =
              map (\fp -> LogHandler { _lhName=fp, _lhFpath=Just fp
                                     , _lhBackend="FileTextScribe", _lhMinSeverity=Just Debug }) $
                maybeToList singleFile ++ manyFiles
        let _ltHandlers = fileHandlers <> handlers
        (_ltMinSeverity :: Severity) <- o .: "severity" .!= Debug
        return LoggerTree{..}


-- | 'LoggerConfig' is the top level configuration datatype
data LoggerConfig = LoggerConfig
    {
        _lcRotation     :: !(Maybe RotationParameters)
    ,   _lcLoggerTree   :: !LoggerTree
    }
    deriving (Generic, Show)

instance ToJSON LoggerConfig
instance FromJSON LoggerConfig where
    parseJSON = withObject "config " $ \o -> do
        _lcRotation <- o .:? "rotation"
        _lcLoggerTree <- o .: "loggerTree"
        return LoggerConfig{..}

instance Monoid LoggerTree where
    mempty = LoggerTree { _ltMinSeverity = Debug
                   , _ltHandlers = [LogHandler { _lhName="node", _lhFpath=Just "node.log"
                                               , _lhBackend="FileTextScribe", _lhMinSeverity=Just Debug}]
                   }
    mappend = (<>)

instance Semigroup LoggerTree

instance Monoid LoggerConfig where
    mempty = LoggerConfig { _lcRotation = Just RotationParameters {
                                            _rpLogLimit = 10 * 1024 * 1024,
                                            _rpKeepFiles = 10 }
                     , _lcLoggerTree = mempty
                     }
    mappend = (<>)

instance Semigroup LoggerConfig


-- | 'parseLoggerConfig' parses a file for the standard logging
--    configuration. Exceptions about opening the file (non existent/permissions)
--    are not handled here. Currently porting log-warper's definition
parseLoggerConfig :: MonadIO m => FilePath -> m LoggerConfig
parseLoggerConfig lgPath =
    liftIO $ join $ either throwM return <$> Y.decodeFileEither lgPath

-- | load log config from file  TODO
loadLogConfig :: MonadIO m => Maybe FilePath -> Maybe FilePath -> m ()
loadLogConfig _ _ = return ()

-- | Given logger config, retrieves all (logger name, filepath) for
-- every logger that has file handle. Filepath inside does __not__
-- contain the common logger config prefix.
-- (this function was in infra/Pos/Reporting/Methods.hs)
retrieveLogFiles :: LoggerConfig -> [([Text], FilePath)]
{-
retrieveLogFiles lconfig = fromLogTree $ lconfig ^. lcTree
  where
    fromLogTree lt =
        let curElems = map ([],) (lt ^.. ltFiles . each . hwFilePath)
            iterNext (part, node) = map (first (part :)) $ fromLogTree node
        in curElems ++ concatMap iterNext (lt ^. ltSubloggers . to HM.toList)
-}
retrieveLogFiles _ = []

