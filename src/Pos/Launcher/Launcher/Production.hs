{-# LANGUAGE ScopedTypeVariables #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher.Production
       ( -- 'ProductionsMode' node launcher.
         runNodeProduction
       ) where

import           Mockable                   (Production)
import           Network.Transport.Abstract (Transport)

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec)
import           Pos.DHT.Real               (KademliaDHTInstance)
import           Pos.Launcher.Param         (NodeParams (..))
import           Pos.Launcher.Runner        (runProductionMode)
import           Pos.Launcher.Scenario      (runNode)
import           Pos.Ssc.Class              (SscConstraint)
import           Pos.Ssc.Class.Types        (SscParams)
import           Pos.WorkMode               (ProductionMode)

-- | Run full node in real mode.
runNodeProduction
    :: forall ssc.
       SscConstraint ssc
    => Transport (ProductionMode ssc)
    -> KademliaDHTInstance
    -> ([WorkerSpec (ProductionMode ssc)], OutSpecs)
    -> NodeParams
    -> SscParams ssc
    -> Production ()
runNodeProduction transport kinst plugins np sscnp =
    runProductionMode transport kinst np sscnp (runNode @ssc plugins)
