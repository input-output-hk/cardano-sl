{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | High level workers.

module Pos.Worker
       ( allWorkers
       ) where

import           Universum

import           Data.Tagged          (untag)

import           Pos.Block.Worker     (blkWorkers)
import           Pos.Communication    (OutSpecs, WorkerSpec, localWorker, relayWorkers,
                                       wrapActionSpec, RelayContext)
import           Pos.Context          (NodeContext (..), recoveryCommGuard)
import           Pos.Delegation       (delegationRelays, dlgWorkers)
import           Pos.Discovery        (discoveryWorkers)
import           Pos.Lrc.Worker       (lrcOnNewSlotWorker)
import           Pos.Security.Workers (SecurityWorkersClass, securityWorkers)
import           Pos.Slotting         (logNewSlotWorker, slottingWorkers)
import           Pos.Ssc.Class        (SscListenersClass (sscRelays),
                                       SscWorkersClass (sscWorkers))
import           Pos.Txp              (txRelays)
import           Pos.Txp.Worker       (txpWorkers)
import           Pos.Update           (usRelays, usWorkers)
import           Pos.Util             (mconcatPair)
import           Pos.WorkMode         (WorkMode)

-- | All, but in reality not all, workers used by full node.
allWorkers
    :: ( SscListenersClass ssc
       , SscWorkersClass ssc
       , SecurityWorkersClass ssc
       , WorkMode ssc m
       )
    => RelayContext m -> NodeContext ssc  -> ([WorkerSpec m], OutSpecs)
allWorkers relayContext NodeContext {..} = mconcatPair
    [
      -- Only workers of "onNewSlot" type
      -- I have no idea what this ↑ comment means (@gromak).

      wrap' "dht"        $ discoveryWorkers ncDiscoveryContext

    , wrap' "ssc"        $ sscWorkers
    , wrap' "security"   $ untag securityWorkers
    , wrap' "lrc"        $ first one lrcOnNewSlotWorker
    , wrap' "us"         $ usWorkers

      -- Have custom loggers
    , wrap' "block"      $ blkWorkers
    , wrap' "txp"        $ txpWorkers relayContext
    , wrap' "delegation" $ dlgWorkers
    , wrap' "slotting"   $ (properSlottingWorkers, mempty)
    , wrap' "relay"      $ relayWorkers relayContext $ mconcat
        [delegationRelays relayContext, untag sscRelays, txRelays, usRelays]
    ]
  where
    properSlottingWorkers =
       fst (recoveryCommGuard (localWorker logNewSlotWorker)) :
       map (fst . localWorker) (slottingWorkers ncSlottingContext)
    wrap' lname = first (map $ wrapActionSpec $ "worker" <> lname)
