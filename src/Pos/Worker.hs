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
import           Pos.DB               (MonadDBCore)
import           Pos.Delegation       (delegationRelays, dlgWorkers)
import           Pos.Lrc.Worker       (lrcOnNewSlotWorker)
import           Pos.Security.Workers (SecurityWorkersClass, securityWorkers)
import           Pos.Slotting.Class   (MonadSlots (slottingWorkers))
import           Pos.Slotting.Util    (logNewSlotWorker)
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
       , MonadDBCore m
       )
    => ([WorkerSpec m], OutSpecs)
allWorkers = mconcatPair
    [
      -- Only workers of "onNewSlot" type

      -- TODO cannot have this DHT worker here. It assumes Kademlia.
      --wrap' "dht"        $ dhtWorkers

      wrap' "ssc"        $ untag sscWorkers
    , wrap' "security"   $ untag securityWorkers
    , wrap' "lrc"        $ first pure lrcOnNewSlotWorker
    , wrap' "us"         $ usWorkers

      -- Have custom loggers
    , wrap' "block"      $ blkWorkers
    , wrap' "txp"        $ txpWorkers
    , wrap' "delegation" $ dlgWorkers
    , wrap' "slotting"   $ (properSlottingWorkers, mempty)
    , wrap' "relay"      $ relayWorkers $ mconcat
        [delegationRelays, untag sscRelays, txRelays, usRelays]

    -- I don't know, guys, I don't know :(
    -- , const ([], mempty) statsWorkers
    ]
  where
    properSlottingWorkers =
        map (fst . localWorker) (logNewSlotWorker : slottingWorkers)
    wrap' lname = first (map $ wrapActionSpec $ "worker" <> lname)

-- FIXME this shouldn't be needed.
allWorkersCount
    :: forall ssc m.
       ( SscListenersClass ssc
       , MonadDBCore m
       , SscWorkersClass ssc
       , SecurityWorkersClass ssc
       , WorkMode ssc m
       )
    => Int
allWorkersCount = length $ fst (allWorkers @ssc @m)
