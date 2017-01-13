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
import           System.Wlog                       (LoggerName)
import           Universum

import           Pos.Binary.Communication          ()
import           Pos.Block.Network.Server          (blockListeners, blockStubListeners)
import           Pos.Communication.BiP             (BiP)
import           Pos.Communication.Server.Protocol (protocolListeners,
                                                    protocolStubListeners)
import           Pos.Communication.Server.SysStart
import           Pos.Communication.Util            (modifyListenerLogger)
import           Pos.Delegation.Listeners          (delegationListeners,
                                                    delegationStubListeners)
import           Pos.Ssc.Class.Listeners           (SscListenersClass (..))
import           Pos.Ssc.Class.Workers             (SscWorkersClass (..))
import           Pos.Txp.Listeners                 (txListeners, txStubListeners)
import           Pos.Util                          (withWaitLog, withWaitLogConvL)
import           Pos.WorkMode                      (WorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, SscWorkersClass ssc, WorkMode ssc m)
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
        ListenerActionConversation $ \nId sA cA -> f nId (withWaitLog sA) (withWaitLogConvL nId cA)

-- | All listeners running on one node.
allStubListeners
    :: (SscListenersClass ssc, Monad m) => Proxy ssc -> [Listener BiP m]
allStubListeners p =
    concat
        [ blockStubListeners p
        , sscStubListeners p
        , txStubListeners p
        , delegationStubListeners
        , protocolStubListeners
        ]

---- | ForkStrategy of whole server.
--forkStrategy
--    :: forall ssc.
--       Typeable ssc
--    => ForkStrategy MessageName
--forkStrategy = ForkStrategy forkStrategyImpl
--  where
--    forkStrategyImpl
--        :: forall m.
--           (MonadTimed m)
--        => MessageName -> m () -> m ()
--    forkStrategyImpl = fromMaybe fork_ . (blkForkStrategy @ssc)

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
