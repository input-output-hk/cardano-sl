{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server part.

module Pos.Communication.Server
       ( allListeners
       , forkStrategy
       , serverLoggerName
       , module Pos.Communication.Server.SysStart
       ) where

import           Control.TimeWarp.Rpc              (ForkStrategy (ForkStrategy),
                                                    MessageName)
import           Control.TimeWarp.Timed            (MonadTimed, fork_)
import           Data.Tagged                       (untag)
import           System.Wlog                       (LoggerName)
import           Universum

import           Pos.Binary.Communication          ()
import           Pos.Block.Network.Server          (blkForkStrategy)
import           Pos.Communication.Server.Protocol (protocolListeners)
import           Pos.Communication.Server.SysStart
import           Pos.Communication.Types           (MutPeerState)
import           Pos.Communication.Util            (modifyListenerLogger)
import           Pos.Delegation.Listeners          (delegationListeners)
import           Pos.DHT.Model                     (ListenerDHT, MonadDHTDialog)
import           Pos.Ssc.Class.Listeners           (SscListenersClass, sscListeners)
import           Pos.Txp.Listeners                 (txListeners)
import           Pos.WorkMode                      (WorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, MonadDHTDialog (MutPeerState ssc) m, WorkMode ssc m)
    => [ListenerDHT (MutPeerState ssc) m]
allListeners = notImplemented
-- TODO [CSL-447] Uncomment
    --map (modifyListenerLogger serverLoggerName) $
    --concat
    --    [ map (modifyListenerLogger "block") blockListeners           -- DONE
    --    , map (modifyListenerLogger "ssc") $ untag sscListeners       -- UNDONE
    --    , map (modifyListenerLogger "tx") txListeners                 -- DONE
    --    , map (modifyListenerLogger "delegation") delegationListeners -- UNDONE
    --    , map (modifyListenerLogger "protocol") protocolListeners     -- DONE
    --    ]

-- | ForkStrategy of whole server.
forkStrategy
    :: forall ssc.
       Typeable ssc
    => ForkStrategy MessageName
forkStrategy = ForkStrategy forkStrategyImpl
  where
    forkStrategyImpl
        :: forall m.
           (MonadTimed m)
        => MessageName -> m () -> m ()
    forkStrategyImpl = fromMaybe fork_ . (blkForkStrategy @ssc)

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
