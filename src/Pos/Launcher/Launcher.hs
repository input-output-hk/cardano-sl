{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       (
         -- * Node launchers.
         NodeRunner
       , NodeRunnerClass (runNodeIO)
       , runNodeProduction
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
import           Pos.WorkMode          (ProductionMode, StatsMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- Too hard :(
type NodeRunner m = KademliaDHTInstance -> [m ()] -> NodeParams -> IO ()

class NodeRunnerClass ssc m where
    runNodeIO :: KademliaDHTInstance -> [m ()] -> NodeParams -> IO ()

-- | Run full node in real mode.
runNodeProduction
    :: forall ssc.
       SscConstraint ssc
    => KademliaDHTInstance -> [ProductionMode ssc ()] -> NodeParams -> IO ()
runNodeProduction inst plugins np = runProductionMode inst np (runNode @ssc plugins)

instance SscConstraint ssc => NodeRunnerClass ssc (ProductionMode ssc) where
    runNodeIO = runNodeProduction

-- | Run full node in benchmarking node
runNodeStats
    :: forall ssc.
       SscConstraint ssc
    => KademliaDHTInstance -> [StatsMode ssc ()] -> NodeParams -> IO ()
runNodeStats inst plugins np = runStatsMode inst np (runNode @ssc plugins)

instance SscConstraint ssc => NodeRunnerClass ssc (StatsMode ssc) where
    runNodeIO = runNodeStats

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
