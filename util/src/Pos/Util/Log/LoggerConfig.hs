{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Pos.Util.Log.LoggerConfig
       ( LoggerConfig (..)
       , RotationParameters (..)
       , LogHandler (..)
       , LoggerTree (..)
       , NamedSeverity
       , BackendKind (..)
       , LogSecurityLevel (..)
       , defaultTestConfiguration
       , defaultInteractiveConfiguration
       , defaultStdErrConfiguration
       , jsonInteractiveConfiguration
       -- * access
       , lcLoggerTree, lcTree
       , lcRotation
       , lcBasePath
       , ltHandlers
       , ltMinSeverity
       , ltNamedSeverity
       , rpKeepFilesNum
       , rpLogLimitBytes
       , rpMaxAgeHours
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
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Traversable (for)

import           System.FilePath (normalise)

import           Pos.Util.Log.LoggerName (LoggerName)
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
    { _rpLogLimitBytes :: !Word64  -- ^ max size of file in bytes
    , _rpMaxAgeHours   :: !Word    -- ^ hours
    , _rpKeepFilesNum  :: !Word    -- ^ number of files to keep
    } deriving (Generic, Show, Eq)

instance ToJSON RotationParameters
instance FromJSON RotationParameters where
    parseJSON = withObject "rotation params" $ \o -> do
        _rpLogLimitBytes  <- o .: "logLimit"
        _rpMaxAgeHours    <- o .:? "maxAge" .!= 24
        _rpKeepFilesNum   <- o .: "keepFiles"
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
    } deriving (Eq, Generic, Show)

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

-- | a mapping LoggerName -> Severity
--   can override the '_ltMinSeverity' for a named context
type NamedSeverity = HashMap LoggerName Severity

filterKnowns :: HashMap Text a -> HashMap LoggerName a
filterKnowns = HM.filterWithKey (\k _ -> k `notElem` known)
  where
    known = ["file","files","severity","handlers"]

-- | @'LoggerTree'@ contains the actual logging configuration,
--    'Severity' and 'LogHandler'
data LoggerTree = LoggerTree
    { _ltMinSeverity   :: !Severity
    , _ltHandlers      :: ![LogHandler]
    , _ltNamedSeverity :: !NamedSeverity
    } deriving (Eq, Generic, Show)

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
                            _lhSecurityLevel = Just PublicLogLevel,
                            _lhMinSeverity = Just Info }
        let fileHandlers =
              map (\fp ->
                let name = T.pack fp in
                LogHandler { _lhName=name
                           , _lhFpath=Just fp
                           , _lhBackend=FileTextBE
                           , _lhMinSeverity=Just Info
                           , _lhSecurityLevel=case ".pub" `T.isSuffixOf` name of
                                True -> Just PublicLogLevel
                                _    -> Just SecretLogLevel
                           }) $
                maybeToList singleFile ++ manyFiles
        let _ltHandlers = fileHandlers <> handlers <> [consoleHandler]
        (_ltMinSeverity :: Severity) <- o .: "severity" .!= Info
        -- everything else is considered a severity filter
        (_ltNamedSeverity :: NamedSeverity) <- for (filterKnowns o) parseJSON
        return LoggerTree{..}

mkUniq :: [LogHandler] -> [LogHandler]
mkUniq handlers = mkUniq' handlers []
    where
        containedIn :: LogHandler -> [LogHandler] -> Bool
        containedIn lh lhs =
            -- by equal name
            any (\lh' -> _lhName lh == _lhName lh') lhs
        mkUniq' [] acc = acc
        mkUniq' (lh:lhs) acc | lh `containedIn` acc = mkUniq' lhs acc
                             | otherwise            = mkUniq' lhs (lh:acc)

instance Semigroup LoggerTree where
    lt1 <> lt2 = LoggerTree {
                  _ltMinSeverity = _ltMinSeverity lt1 `min` _ltMinSeverity lt2
                , _ltHandlers = mkUniq $ _ltHandlers lt1 <> _ltHandlers lt2
                , _ltNamedSeverity = _ltNamedSeverity lt1 <> _ltNamedSeverity lt2
                }
instance Monoid LoggerTree where
    mempty = LoggerTree { _ltMinSeverity = Info
                   , _ltHandlers = [LogHandler { _lhName="console", _lhFpath=Nothing
                                               , _lhBackend=StdoutBE
                                               , _lhMinSeverity=Just Info
                                               , _lhSecurityLevel=Just PublicLogLevel}]
                   , _ltNamedSeverity = HM.empty
                   }
        --  default values
    mappend = (<>)

makeLenses ''LoggerTree

-- | @'LoggerConfig'@ is the top level configuration datatype
data LoggerConfig = LoggerConfig
    { _lcRotation   :: !(Maybe RotationParameters)
    , _lcLoggerTree :: !LoggerTree
    , _lcBasePath   :: !(Maybe FilePath)
    } deriving (Generic, Show, Eq)

instance ToJSON LoggerConfig
instance FromJSON LoggerConfig where
    parseJSON = withObject "config " $ \o -> do
        _lcRotation <- o .:? "rotation"
        _lcLoggerTree <- o .: "loggerTree"
        _lcBasePath <- o .:? "logdir"
        return LoggerConfig{..}

instance Semigroup LoggerConfig where
    lc1 <> lc2 = LoggerConfig {
                  _lcRotation = _lcRotation  lc1
                , _lcLoggerTree = _lcLoggerTree lc1 <> _lcLoggerTree lc2
                , _lcBasePath = let basePath1 = _lcBasePath lc1 in
                                case basePath1 of
                                    Nothing -> _lcBasePath lc2
                                    _       -> basePath1
                }
instance Monoid LoggerConfig where
    mempty = LoggerConfig { _lcRotation = Just RotationParameters {
                                            _rpLogLimitBytes = 5 * 1024 * 1024,
                                            _rpKeepFilesNum  = 10,
                                            _rpMaxAgeHours   = 24 }
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
            _ltNamedSeverity = HM.empty,
            _ltHandlers = [ LogHandler {
                _lhBackend = StdoutBE,
                _lhName = "console",
                _lhFpath = Nothing,
                _lhSecurityLevel = Just PublicLogLevel,
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
            _ltNamedSeverity = HM.empty,
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
            _ltNamedSeverity = HM.empty,
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
            _ltNamedSeverity = HM.empty,
            _ltHandlers = [ LogHandler {
                _lhBackend = DevNullBE,
                _lhName = "devnull",
                _lhFpath = Nothing,
                _lhSecurityLevel = Just PublicLogLevel,
                _lhMinSeverity = Just minSeverity } ]
          }
    in
    LoggerConfig{..}

lcTree :: Functor f => (LoggerTree -> f LoggerTree) -> LoggerConfig -> f LoggerConfig
lcTree = lcLoggerTree

