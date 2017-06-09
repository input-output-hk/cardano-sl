{-# LANGUAGE ScopedTypeVariables #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launchers.
         runNodeProduction
       , runNodeStatic
       ) where

import           Data.Set                   (Set)
import           Mockable                   (Production)
import           Network.Transport.Abstract (Transport)

import           Pos.Communication          (NodeId)
import           Pos.Communication.Protocol (OutSpecs, WorkerSpec)
import           Pos.DHT.Real               (KademliaDHTInstance)
import           Pos.Launcher.Param         (NodeParams (..))
import           Pos.Launcher.Runner        (runProductionMode, runStaticMode)
import           Pos.Launcher.Scenario      (runNode)
import           Pos.Security               (SecurityWorkersClass)
import           Pos.Ssc.Class              (SscConstraint)
import           Pos.Ssc.Class.Types        (SscParams)
import           Pos.WorkMode               (ProductionMode, StaticMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeProduction
    :: forall ssc.
       (SscConstraint ssc, SecurityWorkersClass ssc)
    => KademliaDHTInstance
    -> Transport (ProductionMode ssc)
    -> ([WorkerSpec (ProductionMode ssc)], OutSpecs)
    -> NodeParams
    -> SscParams ssc
    -> Production ()
runNodeProduction kinst transport plugins np sscnp =
    runProductionMode kinst transport np sscnp (runNode @ssc plugins)

-- | Run full node in static mode
runNodeStatic
    :: forall ssc.
       (SscConstraint ssc, SecurityWorkersClass ssc)
    => Set NodeId
    -> Transport (StaticMode ssc)
    -> ([WorkerSpec (StaticMode ssc)], OutSpecs)
    -> NodeParams
    -> SscParams ssc
    -> Production ()
runNodeStatic peers transport plugins np sscnp =
    runStaticMode peers transport np sscnp (runNode @ssc plugins)
