-- We use undefined in this file (unfortunately) so we can't have -Werror
{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | High level workers.

module Pos.Worker
       ( allWorkers
       , allWorkersCount
       ) where

import           Data.Tagged             (untag)
import           Universum

import           Pos.Block.Worker        (blkWorkers)
import           Pos.Communication       (OutSpecs, WorkerSpec, localWorker, relayWorkers,
                                          wrapActionSpec, NodeId)
import           Pos.Communication.Specs (allOutSpecs)
import           Pos.DB                  (MonadDBCore)
import           Pos.Delegation          (dlgWorkers)
import           Pos.Lrc.Worker          (lrcOnNewSlotWorker)
import           Pos.Security.Workers    (SecurityWorkersClass, securityWorkers)
import           Pos.Slotting.Class      (MonadSlots (slottingWorkers))
import           Pos.Slotting.Util       (logNewSlotWorker)
import           Pos.Ssc.Class.Workers   (SscWorkersClass, sscWorkers)
import           Pos.Txp.Worker          (txpWorkers)
import           Pos.Update              (usWorkers)
import           Pos.Util                (mconcatPair)
import           Pos.WorkMode            (WorkMode)

-- | All, but in reality not all, workers used by full node.
allWorkers
    :: ( SscWorkersClass ssc
       , SecurityWorkersClass ssc
       , WorkMode ssc m
       , MonadDBCore m
       )
    => m (Set NodeId) -> ([WorkerSpec m], OutSpecs)
allWorkers getPeers = mconcatPair
    [
      -- Only workers of "onNewSlot" type

      -- TODO cannot have this DHT worker here. It assumes Kademlia.
      --wrap' "dht"        $ dhtWorkers

      wrap' "ssc"        $ untag (sscWorkers getPeers)
    , wrap' "security"   $ untag (securityWorkers getPeers)
    , wrap' "lrc"        $ first pure lrcOnNewSlotWorker
    , wrap' "us"         $ usWorkers

      -- Have custom loggers
    , wrap' "block"      $ blkWorkers getPeers
    , wrap' "txp"        $ txpWorkers getPeers
    , wrap' "delegation" $ dlgWorkers
    , wrap' "slotting"   $ (properSlottingWorkers, mempty)
    , wrap' "relay"      $ relayWorkers getPeers allOutSpecs

    -- I don't know, guys, I don't know :(
    -- , const ([], mempty) statsWorkers
    ]
  where
    properSlottingWorkers =
        map (fst . localWorker) (logNewSlotWorker:slottingWorkers)
    wrap' lname = first (map $ wrapActionSpec $ "worker" <> lname)

-- FIXME this shouldn't be needed.
allWorkersCount
    :: forall ssc m.
       (MonadDBCore m, SscWorkersClass ssc, SecurityWorkersClass ssc, WorkMode ssc m)
    => Int
allWorkersCount = length $ fst (allWorkers @ssc @m undefined)
