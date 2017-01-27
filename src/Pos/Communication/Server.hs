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
import           Pos.Communication.Server.SysStart
import           Pos.Communication.Types.Protocol  (PeerId)
import           Pos.Communication.Util            (modifyListenerLogger)
import           Pos.Constants                     (networkReceiveTimeout)
import           Pos.Delegation.Listeners          (delegationListeners,
                                                    delegationStubListeners)
import           Pos.Ssc.Class.Listeners           (SscListenersClass (..))
import           Pos.Txp.Listeners                 (txListeners, txStubListeners)
import           Pos.Update                        (usListeners, usStubListeners)
import           Pos.Util                          (convWithTimeLimit,
                                                    sendActionsWithTimeLimit, withWaitLog,
                                                    withWaitLogConvL)
import           Pos.WorkMode                      (WorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, WorkMode ssc m)
    => [Listener BiP PeerId m]
allListeners =
    map addWaitLogging $
    map (addTimeout networkReceiveTimeout) $
    map (modifyListenerLogger serverLoggerName) $
    concat
        [ map (modifyListenerLogger "block") blockListeners
        , map (modifyListenerLogger "ssc") $ untag sscListeners
        , map (modifyListenerLogger "tx") txListeners
        , map (modifyListenerLogger "delegation") delegationListeners
        , map (modifyListenerLogger "update") usListeners
        ]
  where
    addWaitLogging (ListenerActionOneMsg f) =
        ListenerActionOneMsg $ \d nId sA msg -> f d nId (withWaitLog sA) msg
    addWaitLogging (ListenerActionConversation f) =
        ListenerActionConversation $ \d nId cA -> f d nId (withWaitLogConvL nId cA)

    addTimeout timeout (ListenerActionOneMsg f) =
        ListenerActionOneMsg $ \d nId sA msg ->
            f d nId (sendActionsWithTimeLimit timeout sA) msg
    addTimeout timeout (ListenerActionConversation f) =
        ListenerActionConversation $ \d nId cA ->
            f d nId (convWithTimeLimit timeout nId cA)

-- | All listeners running on one node.
allStubListeners
    :: (SscListenersClass ssc, WithLogger m) => Proxy ssc -> [Listener BiP PeerId m]
allStubListeners p =
    concat
        [ blockStubListeners p
        , sscStubListeners p
        , txStubListeners p
        , delegationStubListeners
        , usStubListeners
        ]

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
