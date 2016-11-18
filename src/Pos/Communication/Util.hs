-- | Communication-specific utility functions.

module Pos.Communication.Util
       ( modifyListenerLogger
       ) where

import           System.Wlog (LoggerName, WithNamedLogger, modifyLoggerName)
import           Universum

import           Pos.DHT     (ListenerDHT (..))

-- | Append given logger name to the name used by listener.
modifyListenerLogger
    :: (Monad m, WithNamedLogger m)
    => LoggerName -> ListenerDHT m -> ListenerDHT m
modifyListenerLogger name (ListenerDHT listener) =
    ListenerDHT $ \r -> modifyLoggerName (<> name) (listener r)
