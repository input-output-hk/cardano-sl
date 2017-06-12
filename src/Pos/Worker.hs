{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | High level workers.

module Pos.Worker
       ( allWorkers
       , allWorkersCount
       ) where

import           Universum

import           Data.Tagged          (untag)

import           Pos.Block.Worker     (blkWorkers)
import           Pos.Communication    (OutSpecs, WorkerSpec, localWorker, relayWorkers,
                                       wrapActionSpec)
import           Pos.Context          (recoveryCommGuard)
import           Pos.Delegation       (delegationRelays, dlgWorkers)
import           Pos.Lrc.Worker       (lrcOnNewSlotWorker)
import           Pos.Security.Workers (SecurityWorkersClass, securityWorkers)
import           Pos.Slotting         (SlottingContextSum, logNewSlotWorker,
                                       slottingWorkers)
import           Pos.Ssc.Class        (SscListenersClass (sscRelays),
                                       SscWorkersClass (sscWorkers))
import           Pos.Txp              (txRelays)
import           Pos.Txp.Worker       (txpWorkers)
import           Pos.Update           (usRelays, usWorkers)
import           Pos.Util             (mconcatPair)
import           Pos.WorkMode.Class   (WorkMode)

-- | All, but in reality not all, workers used by full node.
allWorkers
    :: ( SscListenersClass ssc
       , SscWorkersClass ssc
       , SecurityWorkersClass ssc
       , WorkMode ssc m
       )
    => SlottingContextSum -> ([WorkerSpec m], OutSpecs)
allWorkers slottingCtx = mconcatPair
    [
      -- Only workers of "onNewSlot" type

      -- TODO cannot have this DHT worker here. It assumes Kademlia.
      --wrap' "dht"        $ dhtWorkers

      wrap' "ssc"        $ sscWorkers
    , wrap' "security"   $ untag securityWorkers
    , wrap' "lrc"        $ first one lrcOnNewSlotWorker
    , wrap' "us"         $ usWorkers

      -- Have custom loggers
    , wrap' "block"      $ blkWorkers
    , wrap' "txp"        $ txpWorkers
    , wrap' "delegation" $ dlgWorkers
    , wrap' "slotting"   $ (properSlottingWorkers, mempty)
    , wrap' "relay"      $ relayWorkers $ mconcat
        [delegationRelays, untag sscRelays, txRelays, usRelays]
    ]
  where
    properSlottingWorkers =
       fst (recoveryCommGuard (localWorker logNewSlotWorker)) :
       map (fst . localWorker) (slottingWorkers slottingCtx)
    wrap' lname = first (map $ wrapActionSpec $ "worker" <> lname)

-- FIXME this shouldn't be needed.
allWorkersCount
    :: forall ssc m.
       ( SscListenersClass ssc
       , SscWorkersClass ssc
       , SecurityWorkersClass ssc
       , WorkMode ssc m
       )
    => SlottingContextSum -> Int
allWorkersCount = length . fst . (allWorkers @ssc @m)
