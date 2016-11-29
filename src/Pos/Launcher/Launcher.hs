{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       (
         -- * Node launchers.
         runNodeProduction
       , runNodeStats

         -- * Utility launchers.
       , submitTxReal
       ) where

import           Universum

import           Pos.DHT               (DHTNode (dhtAddr), DHTNodeType (..),
                                        MonadDHT (..), filterByNodeType)
import           Pos.DHT.Real          (KademliaDHTInstance)
import           Pos.Launcher.Param    (NodeParams (..))
import           Pos.Launcher.Runner   (bracketDHTInstance, runProductionMode,
                                        runRawRealMode, runStatsMode)
import           Pos.Launcher.Scenario (runNode, submitTx)
import           Pos.Ssc.Class         (SscConstraint)
import           Pos.Statistics        (getNoStatsT)
import           Pos.Types             (Address, Coin, TxId)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeProduction
    :: forall ssc.
       SscConstraint ssc
    => KademliaDHTInstance -> NodeParams -> IO ()
runNodeProduction inst np = runProductionMode inst np (runNode @ssc)

-- | Run full node in benchmarking node
runNodeStats
    :: forall ssc.
       SscConstraint ssc
    => KademliaDHTInstance -> NodeParams -> IO ()
runNodeStats inst np = runStatsMode inst np (runNode @ssc)

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | Submit tx in real mode.
submitTxReal
    :: forall ssc .
    SscConstraint ssc
    => NodeParams -> (TxId, Word32) -> (Address, Coin) -> IO ()
submitTxReal np input addrCoin = bracketDHTInstance (npBaseParams np) action
  where
    action inst = runRawRealMode @ssc inst np [] $ do
        peers <- getKnownPeers
        let na = dhtAddr <$> filterByNodeType DHTFull peers
        void $ getNoStatsT $ (submitTx @ssc) na input addrCoin
