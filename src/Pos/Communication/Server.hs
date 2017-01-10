{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server part.

module Pos.Communication.Server
       ( allListeners
       , serverLoggerName
       , module Pos.Communication.Server.SysStart
       ) where

import           Data.Tagged                       (untag)
import           Node                              (Listener)
import           System.Wlog                       (LoggerName)
import           Universum

import           Pos.Binary.Communication          ()
import           Pos.Block.Network.Server          (blockListeners)
import           Pos.Communication.BiP             (BiP)
import           Pos.Communication.Server.Protocol (protocolListeners)
import           Pos.Communication.Server.SysStart
import           Pos.Communication.Util            (modifyListenerLogger)
import           Pos.Delegation.Listeners          (delegationListeners)
import           Pos.Ssc.Class.Listeners           (SscListenersClass (..))
import           Pos.Txp.Listeners                 (txListeners)
import           Pos.WorkMode                      (NewWorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, NewWorkMode ssc m)
    => [Listener BiP m]
allListeners =
    map (modifyListenerLogger serverLoggerName) $
    concat
        [ map (modifyListenerLogger "block") blockListeners
        , map (modifyListenerLogger "ssc") $ untag sscListeners
        , map (modifyListenerLogger "tx") txListeners
        , map (modifyListenerLogger "delegation") delegationListeners
        , map (modifyListenerLogger "protocol") protocolListeners
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
