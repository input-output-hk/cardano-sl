{-# LANGUAGE ScopedTypeVariables #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher.Static
       ( -- 'StaticMode' node launcher.
         runNodeStatic
       ) where

import           Data.Set                   (Set)
import           Mockable                   (Production)
import           Network.Transport.Abstract (Transport)

import           Pos.Communication          (NodeId)
import           Pos.Communication.Protocol (OutSpecs, WorkerSpec)
import           Pos.Launcher.Param         (NodeParams (..))
import           Pos.Launcher.Runner        (runStaticMode)
import           Pos.Launcher.Scenario      (runNode)
import           Pos.Ssc.Class              (SscConstraint)
import           Pos.Ssc.Class.Types        (SscParams)
import           Pos.WorkMode               (StaticMode)

-- | Run full node in static mode
runNodeStatic
    :: forall ssc.
       SscConstraint ssc
    => Transport (StaticMode ssc)
    -> Set NodeId
    -> ([WorkerSpec (StaticMode ssc)], OutSpecs)
    -> NodeParams
    -> SscParams ssc
    -> Production ()
runNodeStatic transport peers plugins np sscnp =
    runStaticMode transport peers np sscnp (runNode @ssc plugins)
