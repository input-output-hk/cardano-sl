module Pos.Util.Log.LoggerName
       ( LoggerName
       , HasLoggerName'(..)
       , askLoggerNameDefault
       , modifyLoggerNameDefault
       ) where

import           Universum

type LoggerName = Text

class HasLoggerName' ctx where
    loggerName :: Lens' ctx LoggerName

askLoggerNameDefault
    :: (MonadReader ctx m, HasLoggerName' ctx)
    => m LoggerName
askLoggerNameDefault = view loggerName

modifyLoggerNameDefault
    :: (MonadReader ctx m, HasLoggerName' ctx)
    => (LoggerName -> LoggerName)
    -> m a
    -> m a
modifyLoggerNameDefault f = local (loggerName %~ f)

