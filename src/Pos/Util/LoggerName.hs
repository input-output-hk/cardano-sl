module Pos.Util.LoggerName
       ( LoggerName
       , HasLoggerName'(..)
       , getLoggerNameDefault
       , modifyLoggerNameDefault
       ) where

import           Universum

import           System.Wlog (LoggerName)

class HasLoggerName' ctx where
    loggerName :: Lens' ctx LoggerName

getLoggerNameDefault
    :: (MonadReader ctx m, HasLoggerName' ctx)
    => m LoggerName
getLoggerNameDefault = view loggerName

modifyLoggerNameDefault
    :: (MonadReader ctx m, HasLoggerName' ctx)
    => (LoggerName -> LoggerName)
    -> m a
    -> m a
modifyLoggerNameDefault f = local (loggerName %~ f)

