{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Pos.Util.LoggerConfig
       ( LoggerConfig (..)
       , RotationParameters (..)
       , LogHandler (..)
       , LoggerTree (..)
       , BackendKind (..)
       , LogSecurityLevel (..)
       , defaultTestConfiguration
       , defaultInteractiveConfiguration
       , defaultStdErrConfiguration
       , jsonInteractiveConfiguration
       -- * access
       , lcLoggerTree
       , lcRotation
       , lcBasePath
       , ltHandlers
       , ltMinSeverity
       , rpKeepFiles
       , rpLogLimit
       , lhBackend
       , lhName
       , lhFpath
       , lhSecurityLevel
       , lhMinSeverity
       -- * functions
       , parseLoggerConfig
       , retrieveLogFiles
       , setLogPrefix
       ) where

import           Data.Yaml as Y
import           GHC.Generics
import           Universum

import           Control.Lens (each, makeLenses)
import qualified Data.Text as T

import           System.FilePath (normalise)

import           Pos.Util.Log.Severity


-- | @'BackendKind@ defines the available backends
data BackendKind = FileTextBE
                 | FileJsonBE
                 | StdoutBE
                 | StderrBE
                 | DevNullBE
                 deriving (Eq, Generic, Show)
deriving instance ToJSON BackendKind
deriving instance FromJSON BackendKind

-- | @'RotationParameters'@ one of the two categories  used in the
--   logging config, specifying the log rotation parameters
data RotationParameters = RotationParameters
    { _rpLogLimit  :: !Word64  -- ^ max size of file in bytes
    , _rpKeepFiles :: !Word    -- ^ number of files to keep
    } deriving (Generic, Show)

instance ToJSON RotationParameters
instance FromJSON RotationParameters where
    parseJSON = withObject "rotation params" $ \o -> do
        _rpLogLimit  <- o .: "logLimit"
        _rpKeepFiles <- o .: "keepFiles"
        return RotationParameters{..}

makeLenses ''RotationParameters

data LogSecurityLevel = SecretLogLevel
                      -- ^ the log contains all messages (i.e. also 'logInfoS')
                      | PublicLogLevel
                      -- ^ the log only contains public messages (i.e. 'logInfo')
                      deriving (Eq, Show, Generic)

deriving instance ToJSON LogSecurityLevel
deriving instance FromJSON LogSecurityLevel

-- | @'LogHandler'@ describes the output handler (file, stdout, ..)
--
data LogHandler = LogHandler
    { _lhName          :: !T.Text
      -- ^ name of the handler
    , _lhFpath         :: !(Maybe FilePath)
      -- ^ file path
    , _lhSecurityLevel :: !(Maybe LogSecurityLevel)
      -- ^ file will be public or private
    , _lhBackend       :: !BackendKind
      -- ^ describes the backend (scribe for katip) to be loaded
    , _lhMinSeverity   :: !(Maybe Severity)
      -- ^ the minimum severity to be logged
    } deriving (Generic,Show)

instance ToJSON LogHandler
instance FromJSON LogHandler where
    parseJSON = withObject "log handler" $ \o -> do
        (_lhName :: T.Text) <- o .: "name"
        (_lhFpath :: Maybe FilePath) <- fmap normalise <$> o .:? "filepath"
        (_lhSecurityLevel :: Maybe LogSecurityLevel) <- o .:? "logsafety" .!= Just PublicLogLevel
        (_lhBackend :: BackendKind ) <- o .: "backend"
        (_lhMinSeverity :: Maybe Severity) <- o .:? "severity"
        pure LogHandler{..}

makeLenses ''LogHandler

-- | @'LoggerTree'@ contains the actual logging configuration,
--    'Severity' and 'LogHandler'
data LoggerTree = LoggerTree
    { _ltMinSeverity :: !Severity
    , _ltHandlers    :: ![LogHandler]
    } deriving (Generic, Show)

instance ToJSON LoggerTree
instance FromJSON LoggerTree where
    parseJSON = withObject "logger tree" $ \o -> do
        (singleFile :: Maybe FilePath) <- fmap normalise <$> o .:? "file"
        (manyFiles :: [FilePath]) <- map normalise <$> (o .:? "files" .!= [])
        handlers <- o .:? "handlers" .!= []
        let consoleHandler =
                LogHandler { _lhName = "console",
                            _lhBackend = StdoutBE,
                            _lhFpath = Nothing,
                            _lhSecurityLevel = Just SecretLogLevel,
                            _lhMinSeverity = Just Debug }
        let fileHandlers =
              map (\fp ->
                let name = T.pack fp in
                LogHandler { _lhName=name
                           , _lhFpath=Just fp
                           , _lhBackend=FileTextBE
                           , _lhMinSeverity=Just Debug
                           , _lhSecurityLevel=case ".pub" `T.isSuffixOf` name of
                                True -> Just PublicLogLevel
                                _    -> Just SecretLogLevel
                           }) $
                maybeToList singleFile ++ manyFiles
        let _ltHandlers = fileHandlers <> handlers <> [consoleHandler]
        (_ltMinSeverity :: Severity) <- o .: "severity" .!= Debug
        return LoggerTree{..}

instance Semigroup LoggerTree
instance Monoid LoggerTree where
    mempty = LoggerTree { _ltMinSeverity = Debug
                   , _ltHandlers = [LogHandler { _lhName="node", _lhFpath=Just "node.log"
                                               , _lhBackend=FileTextBE
                                               , _lhMinSeverity=Just Debug
                                               , _lhSecurityLevel=Just PublicLogLevel}]
                   }
        --  default values
    mappend = (<>)

makeLenses ''LoggerTree

-- | @'LoggerConfig'@ is the top level configuration datatype
data LoggerConfig = LoggerConfig
    { _lcRotation   :: !(Maybe RotationParameters)
    , _lcLoggerTree :: !LoggerTree
    , _lcBasePath   :: !(Maybe FilePath)
    } deriving (Generic, Show)

instance ToJSON LoggerConfig
instance FromJSON LoggerConfig where
    parseJSON = withObject "config " $ \o -> do
        _lcRotation <- o .:? "rotation"
        _lcLoggerTree <- o .: "loggerTree"
        _lcBasePath <- o .:? "logdir"
        return LoggerConfig{..}

instance Semigroup LoggerConfig
instance Monoid LoggerConfig where
    mempty = LoggerConfig { _lcRotation = Just RotationParameters {
                                            _rpLogLimit = 10 * 1024 * 1024,
                                            _rpKeepFiles = 10 }
                     , _lcLoggerTree = mempty
                     , _lcBasePath = Nothing
                     }
        --  default values
    mappend = (<>)

makeLenses ''LoggerConfig


-- | 'parseLoggerConfig' parses a file for the standard logging
--    configuration. Exceptions about opening the file (non existent/permissions)
--    are not handled here. Currently porting log-warper's definition
parseLoggerConfig :: MonadIO m => FilePath -> m LoggerConfig
parseLoggerConfig lcPath =
    liftIO $ (either throwM return =<< Y.decodeFileEither lcPath)

-- | set log prefix
setLogPrefix :: Maybe FilePath -> LoggerConfig -> IO LoggerConfig
setLogPrefix Nothing lc     = return lc
setLogPrefix bp@(Just _) lc = return lc{ _lcBasePath = bp }


-- | Given logger config, retrieves all (logger name, filepath) for
-- every logger that has file handle. Filepath inside does __not__
-- contain the common logger config prefix.
-- (this function was in infra/Pos/Reporting/Methods.hs)
retrieveLogFiles :: LoggerConfig -> [(Text, FilePath)]
retrieveLogFiles lc =
    map (\LogHandler{..} -> (_lhName, fromMaybe "<unk>" _lhFpath)) $
      filter (\LogHandler{..} -> isJust _lhFpath) lhs
    where
        lhs = lc ^. lcLoggerTree ^. ltHandlers ^.. each

-- | @LoggerConfig@ used interactively
-- output to console and minimum Debug severity
defaultInteractiveConfiguration :: Severity -> LoggerConfig
defaultInteractiveConfiguration minSeverity =
    let _lcRotation = Nothing
        _lcBasePath = Nothing
        _lcLoggerTree = LoggerTree {
            _ltMinSeverity = Debug,
            _ltHandlers = [ LogHandler {
                _lhBackend = StdoutBE,
                _lhName = "console",
                _lhFpath = Nothing,
                _lhSecurityLevel = Just SecretLogLevel,
                _lhMinSeverity = Just minSeverity }
                          ]
          }
    in
    LoggerConfig{..}

-- | @LoggerConfig@ used interactively
-- output to console and minimum Debug severity
defaultStdErrConfiguration :: Severity -> LoggerConfig
defaultStdErrConfiguration minSeverity =
    let _lcRotation = Nothing
        _lcBasePath = Nothing
        _lcLoggerTree = LoggerTree {
            _ltMinSeverity = Debug,
            _ltHandlers = [ LogHandler {
                _lhBackend = StderrBE,
                _lhName = "stderr",
                _lhFpath = Nothing,
                _lhSecurityLevel = Just SecretLogLevel,
                _lhMinSeverity = Just minSeverity }
                          ]
          }
    in
    LoggerConfig{..}

-- | @LoggerConfig@ used in benchmarks
-- output to console and as JSON to file
jsonInteractiveConfiguration :: Severity -> LoggerConfig
jsonInteractiveConfiguration minSeverity =
    let _lcRotation = Nothing
        _lcBasePath = Nothing
        _lcLoggerTree = LoggerTree {
            _ltMinSeverity = Debug,
            _ltHandlers = [ LogHandler {
                _lhBackend = StdoutBE,
                _lhName = "console",
                _lhFpath = Nothing,
                _lhSecurityLevel = Just SecretLogLevel,
                _lhMinSeverity = Just minSeverity }
                          , LogHandler {
                _lhBackend = FileJsonBE,
                _lhName = "json",
                _lhFpath = Just "node.json",
                _lhSecurityLevel = Just SecretLogLevel,
                _lhMinSeverity = Just minSeverity }
                          ]
          }
    in
    LoggerConfig{..}

-- | @LoggerConfig@ used in testing
-- no output and minimum Debug severity
defaultTestConfiguration :: Severity -> LoggerConfig
defaultTestConfiguration minSeverity =
    let _lcRotation = Nothing
        _lcBasePath = Nothing
        _lcLoggerTree = LoggerTree {
            _ltMinSeverity = Debug,
            _ltHandlers = [ LogHandler {
                _lhBackend = DevNullBE,
                _lhName = "devnull",
                _lhFpath = Nothing,
                _lhSecurityLevel = Just PublicLogLevel,
                _lhMinSeverity = Just minSeverity } ]
          }
    in
    LoggerConfig{..}
