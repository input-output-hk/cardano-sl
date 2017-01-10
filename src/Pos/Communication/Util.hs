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
    => LoggerName -> ListenerAction p m -> ListenerAction p m
modifyListenerLogger name (ListenerActionOneMsg f) =
    ListenerActionOneMsg $ \nId sA -> modifyLoggerName (<> name) . f nId sA
modifyListenerLogger name (ListenerActionConversation f) =
    ListenerActionConversation $ \nId -> modifyLoggerName (<> name) . f nId
