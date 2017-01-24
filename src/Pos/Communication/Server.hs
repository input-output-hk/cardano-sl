{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server part.

module Pos.Communication.Server
       ( allListeners
       , allStubListeners
       , serverLoggerName
       , module Pos.Communication.Server.SysStart
       ) where

import           Data.Tagged                       (untag)
import           Node                              (Listener, ListenerAction (..))
import           System.Wlog                       (LoggerName, WithLogger)
import           Universum

import           Pos.Binary.Communication          ()
import           Pos.Block.Network.Listeners       (blockListeners, blockStubListeners)
import           Pos.Communication.BiP             (BiP)
import           Pos.Communication.Server.Protocol (protocolListeners,
                                                    protocolStubListeners)
import           Pos.Communication.Server.SysStart
import           Pos.Communication.Util            (modifyListenerLogger)
import           Pos.Delegation.Listeners          (delegationListeners,
                                                    delegationStubListeners)
import           Pos.Ssc.Class.Listeners           (SscListenersClass (..))
import           Pos.Txp.Listeners                 (txListeners, txStubListeners)
import           Pos.Util                          (withWaitLog, withWaitLogConvL)
import           Pos.WorkMode                      (WorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, WorkMode ssc m)
    => [Listener BiP m]
allListeners =
    map addWaitLogging $
    map (modifyListenerLogger serverLoggerName) $
    concat
        [ map (modifyListenerLogger "block") blockListeners
        , map (modifyListenerLogger "ssc") $ untag sscListeners
        , map (modifyListenerLogger "tx") txListeners
        , map (modifyListenerLogger "delegation") delegationListeners
        , map (modifyListenerLogger "protocol") protocolListeners
        ]
  where
    addWaitLogging (ListenerActionOneMsg f) =
        ListenerActionOneMsg $ \nId sA msg -> f nId (withWaitLog sA) msg
    addWaitLogging (ListenerActionConversation f) =
        ListenerActionConversation $ \nId cA -> f nId (withWaitLogConvL nId cA)

-- | All listeners running on one node.
allStubListeners
    :: (SscListenersClass ssc, WithLogger m) => Proxy ssc -> [Listener BiP m]
allStubListeners p =
    concat
        [ blockStubListeners p
        , sscStubListeners p
        , txStubListeners p
        , delegationStubListeners
        , protocolStubListeners
        ]

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
