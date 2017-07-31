{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | High level workers.

module Pos.Worker
       ( allWorkers
       ) where

import           Universum

import           Data.Tagged             (untag)

import           Pos.Block.Worker        (blkWorkers)
import           Pos.Communication       (OutSpecs, WorkerSpec, localWorker,
                                          wrapActionSpec, Relay,
                                          relayPropagateOut)
import           Pos.Context             (NodeContext (..), recoveryCommGuard)
import           Pos.Delegation          (delegationRelays, dlgWorkers)
import           Pos.DHT.Workers         (dhtWorkers)
import           Pos.Launcher.Resource    (NodeResources (..))
import           Pos.Lrc.Worker          (lrcOnNewSlotWorker)
import           Pos.Network.Types       (NetworkConfig(..), SubscriptionWorker(..),
                                          topologySubscriptionWorker,
                                          topologyRunKademlia)
import           Pos.Security.Workers    (SecurityWorkersClass, securityWorkers)
import           Pos.Slotting            (logNewSlotWorker, slottingWorkers)
import           Pos.Ssc.Class           (SscListenersClass (sscRelays),
                                          SscWorkersClass (sscWorkers))
import           Pos.Subscription.Common (subscriptionWorker)
import           Pos.Subscription.Dns    (dnsSubscriptionWorker)
import           Pos.Subscription.Dht    (dhtSubscriptionWorker)
import           Pos.Txp                 (txRelays)
import           Pos.Txp.Worker          (txpWorkers)
import           Pos.Update              (usRelays, usWorkers)
import           Pos.Util                (mconcatPair)
import           Pos.WorkMode            (WorkMode)

-- | All, but in reality not all, workers used by full node.
allWorkers
    :: forall ssc ctx m .
       ( SscListenersClass ssc
       , SscWorkersClass ssc
       , SecurityWorkersClass ssc
       , WorkMode ssc ctx m
       )
    => NodeResources ssc m -> ([WorkerSpec m], OutSpecs)
allWorkers NodeResources {..} = mconcatPair
    [
      -- Only workers of "onNewSlot" type
      -- I have no idea what this ↑ comment means (@gromak).

      wrap' "ssc"        $ sscWorkers
    , wrap' "security"   $ untag securityWorkers
    , wrap' "lrc"        $ first one lrcOnNewSlotWorker
    , wrap' "us"         $ usWorkers

      -- Have custom loggers
    , wrap' "block"      $ blkWorkers
    , wrap' "txp"        $ txpWorkers
    , wrap' "delegation" $ dlgWorkers
    , wrap' "slotting"   $ (properSlottingWorkers, mempty)

    , wrap' "subscription" $ case topologySubscriptionWorker (ncTopology ncNetworkConfig) of
        Just (SubscriptionWorkerBehindNAT dnsDomains) ->
          subscriptionWorker (dnsSubscriptionWorker ncNetworkConfig dnsDomains)
        Just (SubscriptionWorkerKademlia kinst nodeType valency fallbacks) ->
          subscriptionWorker (dhtSubscriptionWorker kinst nodeType valency fallbacks)
        Nothing ->
          mempty

      -- MAGIC "relay" out specs.
      -- There's no cardano-sl worker for them; they're put out by the outbound
      -- queue system from time-warp (enqueueConversation on SendActions).
    , ([], relayPropagateOut (mconcat [delegationRelays, untag sscRelays, txRelays, usRelays] :: [Relay m]))

      -- Kademlia has some workers to run.
      --
      -- FIXME: perhaps these shouldn't be considered workers, but rather
      -- spawned when the DHT instance is created and killed when it's
      -- released.
    , case topologyRunKademlia (ncTopology ncNetworkConfig) of
        Just kinst -> dhtWorkers kinst
        Nothing -> mempty
    ]
  where
    NodeContext {..} = nrContext
    properSlottingWorkers =
       fst (localWorker (recoveryCommGuard logNewSlotWorker)) :
       map (fst . localWorker) (slottingWorkers ncSlottingContext)
    wrap' lname = first (map $ wrapActionSpec $ "worker" <> lname)
