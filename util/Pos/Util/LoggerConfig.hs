
module Pos.Util.LoggerConfig
       ( LoggerConfig(..)
       , RotationParameters(..)
       , loadLogConfig
       , retrieveLogFiles
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
    } deriving (Generic, Show)

instance Monoid LoggerConfig where
    mempty = LoggerConfig { _lcRotation = Nothing, _lcMinSeverity = Debug }
    mappend = (<>)

instance Semigroup LoggerConfig

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

