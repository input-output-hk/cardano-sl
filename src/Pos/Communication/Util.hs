-- | Communication-specific utility functions.

module Pos.Communication.Util
       ( modifyListenerLogger
       ) where

import           Node        (ListenerAction (..))
import           System.Wlog (HasLoggerName, LoggerName, modifyLoggerName)
import           Universum


-- | Append given logger name to the name used by listener.
modifyListenerLogger
    :: (HasLoggerName m)
    => LoggerName -> ListenerAction p d m -> ListenerAction p d m
modifyListenerLogger name (ListenerActionOneMsg f) =
    ListenerActionOneMsg $ \d nId sA -> modifyLoggerName (<> name) . f d nId sA
modifyListenerLogger name (ListenerActionConversation f) =
    ListenerActionConversation $ \d nId -> modifyLoggerName (<> name) . f d nId
