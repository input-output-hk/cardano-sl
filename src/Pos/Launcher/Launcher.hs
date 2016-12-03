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
import           Pos.Launcher.Scenario (runNode)
import           Pos.Ssc.Class         (SscConstraint)
import           Pos.Ssc.Class.Types   (SscParams)
import           Pos.Statistics        (getNoStatsT)
import           Pos.Types             (Address, Coin, TxId)
import           Pos.Wallet            (submitTx)
import           Pos.WorkMode          (ProductionMode, StatsMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- Too hard :(
type NodeRunner m = KademliaDHTInstance -> [m ()] -> NodeParams -> IO ()

class NodeRunnerClass ssc m where
    runNodeIO :: KademliaDHTInstance -> [m ()] -> NodeParams -> SscParams ssc -> IO ()

-- | Run full node in real mode.
runNodeProduction
    :: forall ssc.
       SscConstraint ssc
    => KademliaDHTInstance -> [ProductionMode ssc ()] -> NodeParams -> SscParams ssc -> IO ()
runNodeProduction inst plugins np sscnp = runProductionMode inst np sscnp (runNode @ssc plugins)

instance SscConstraint ssc => NodeRunnerClass ssc (ProductionMode ssc) where
    runNodeIO = runNodeProduction

-- | Run full node in benchmarking node
runNodeStats
    :: forall ssc.
       SscConstraint ssc
    => KademliaDHTInstance -> [StatsMode ssc ()] -> NodeParams -> SscParams ssc -> IO ()
runNodeStats inst plugins np sscnp = runStatsMode inst np sscnp (runNode @ssc plugins)

instance SscConstraint ssc => NodeRunnerClass ssc (StatsMode ssc) where
    runNodeIO = runNodeStats

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | Submit tx in real mode.
submitTxReal
    :: forall ssc .
    SscConstraint ssc
    => NodeParams -> SscParams ssc -> (TxId, Word32) -> (Address, Coin) -> IO ()
submitTxReal np sscnp input addrCoin = bracketDHTInstance (npBaseParams np) action
  where
    action inst = runRawRealMode @ssc inst np sscnp [] $ do
        peers <- getKnownPeers
        let na = dhtAddr <$> filterByNodeType DHTFull peers
        void $ getNoStatsT $ (submitTx @ssc) na input addrCoin
