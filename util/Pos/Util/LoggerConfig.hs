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

import           Pos.Util.LogSeverity


-- | @'RotationParameters'@ one of the two categories  used in the 
--   logging config, specifying the log rotation parameters
data RotationParameters = RotationParameters
    { _rpLogLimit  :: !Word64  -- ^ max size of file in bytes
    , _rpKeepFiles :: !Word    -- ^ number of files to keep
    } 
    deriving (Generic, Show)

instance FromJSON RotationParameters

-- | 'LoggerTree' contains the actual logging configuration,
--   only 'Severity' and 'Files' for now
data LoggerTree = LoggerTree
    {
      _ltMinSeverity :: !Severity
    , _ltFiles       :: ![FilePath]
    } 
    deriving (Generic, Show)

instance FromJSON LoggerTree


-- | 'LoggerConfig' is the top level configuration datatype
data LoggerConfig = LoggerConfig
    {
        _lcRotation     :: !(Maybe RotationParameters)
    ,   _lcLoggerTree   :: !LoggerTree
    } 
    deriving (Generic, Show)

instance FromJSON LoggerConfig

instance Monoid LoggerTree where
    mempty = LoggerTree { _ltMinSeverity = Debug, _ltFiles = ["node.log"] }
    mappend = (<>)

instance Semigroup LoggerTree

instance Monoid LoggerConfig where
    mempty = LoggerConfig { _lcRotation = Nothing, _lcLoggerTree = mempty }
    mappend = (<>)

instance Semigroup LoggerConfig


-- | 'parseLoggerConfig' parses a file for the standard logging 
--    configuration. Exceptions about opening the file (non existent/permissions)
--    are not handled here. Currently porting log-warper's definition
parseLoggerConfig :: MonadIO m => FilePath -> m LoggerConfig
parseLoggerConfig lgPath = 
    liftIO $ join $ either throwIO return <$> decodeFileEither lgPath

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

