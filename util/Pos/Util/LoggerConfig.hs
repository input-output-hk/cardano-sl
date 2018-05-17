
module Pos.Util.LoggerConfig
       ( LoggerConfig(..)
       , RotationParameters(..)
       , loadLogConfig
       ) where

import           Universum

import           Pos.Util.LogSeverity


-- | a placeholder
data RotationParameters = RotationParameters
    { rpLogLimit  :: !Word64  -- ^ max size of file in bytes
    , rpKeepFiles :: !Word    -- ^ number of files to keep
    } deriving (Generic, Show)

-- | a placeholder
data LoggerConfig = LoggerConfig
    {
        _lcRotation     :: Maybe RotationParameters
    ,   _lcMinSeverity  :: Severity
    }


-- | load log config from file  TODO
loadLogConfig :: MonadIO m => Maybe FilePath -> Maybe FilePath -> m ()
loadLogConfig _ _ = return ()

