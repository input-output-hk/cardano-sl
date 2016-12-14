-- | Communication-specific utility functions.

module Pos.Communication.Util
       ( modifyListenerLogger
       ) where

import           System.Wlog (HasLoggerName, LoggerName, modifyLoggerName)
import           Universum

import           Pos.DHT     (ListenerDHT (..))

-- | Append given logger name to the name used by listener.
modifyListenerLogger
    :: (Monad m, HasLoggerName m)
    => LoggerName -> ListenerDHT s m -> ListenerDHT s m
modifyListenerLogger name (ListenerDHT listener) =
    ListenerDHT $ \r -> modifyLoggerName (<> name) (listener r)
