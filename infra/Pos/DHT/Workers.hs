{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.DHT.Workers
       ( DhtWorkMode
       , dhtWorkers
       ) where

import           Control.Concurrent.STM     (retry)
import           Universum

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Set                   as S
import qualified Data.Store                 as Store
import           Formatting                 (sformat, shown, (%))
import           Mockable                   (Delay, Fork, Mockable)
import           Network.Broadcast.OutboundQueue.Types (simplePeers)
import           Network.Kademlia           (takeSnapshot)
import           System.Wlog                (WithLogger, logNotice)

import           Pos.Binary.Infra.DHTModel  ()
import           Pos.Communication.Protocol (NodeId, OutSpecs, WorkerSpec, Worker,
                                             localOnNewSlotWorker, ActionSpec (..))
import           Pos.Core.Slotting          (flattenSlotId)
import           Pos.Core.Types             (slotIdF)
import           Pos.DHT.Constants          (kademliaDumpInterval)
import           Pos.DHT.Real.Types         (KademliaDHTInstance (..))
import           Pos.DHT.Real.Real          (kademliaGetKnownPeers)
import           Pos.KnownPeers             (MonadKnownPeers (..))
import           Pos.Network.Types          (NodeType)
import           Pos.Recovery.Info          (MonadRecoveryInfo, recoveryCommGuard)
import           Pos.Reporting              (HasReportingContext)
import           Pos.Shutdown               (HasShutdownContext)
import           Pos.Slotting.Class         (MonadSlots)
import           Pos.Util.TimeWarp          (addressToNodeId)

type DhtWorkMode ctx m =
    ( WithLogger m
    , MonadSlots m
    , MonadIO m
    , MonadMask m
    , Mockable Fork m
    , Mockable Delay m
    , MonadKnownPeers m
    , MonadRecoveryInfo m
    , MonadReader ctx m
    , HasReportingContext ctx
    , HasShutdownContext ctx
    )

dhtWorkers
    :: ( DhtWorkMode ctx m
       )
    => KademliaDHTInstance -> ([WorkerSpec m], OutSpecs)
dhtWorkers kademliaInst = mconcat
    [ first pure (dumpKademliaStateWorker kademliaInst)
    , ([kademliaSubscriptionWorker kademliaInst], mempty)
    ]

dumpKademliaStateWorker
    :: ( DhtWorkMode ctx m
       )
    => KademliaDHTInstance
    -> (WorkerSpec m, OutSpecs)
dumpKademliaStateWorker kademliaInst = localOnNewSlotWorker True $ \slotId ->
    when (isTimeToDump slotId) $ recoveryCommGuard $ do
        let dumpFile = kdiDumpPath kademliaInst
        logNotice $ sformat ("Dumping kademlia snapshot on slot: "%slotIdF) slotId
        let inst = kdiHandle kademliaInst
        snapshot <- liftIO $ takeSnapshot inst
        case dumpFile of
            Just fp -> liftIO . BSL.writeFile fp . BSL.fromStrict $ Store.encode snapshot
            Nothing -> return ()
  where
    isTimeToDump slotId = flattenSlotId slotId `mod` kademliaDumpInterval == 0

kademliaSubscriptionWorker
    :: forall ctx m .
       ( DhtWorkMode ctx m
       )
    => KademliaDHTInstance
    -> WorkerSpec m
kademliaSubscriptionWorker kademliaInst = ActionSpec $ \_ _ -> do
    logNotice "Kademlia subscription worker started"
    blockAndUpdate S.empty
  where
    peerType :: NodeType
    peerType = kdiPeerType kademliaInst
    blockAndUpdate :: Set NodeId -> m x
    blockAndUpdate lastSet = do
        (additions, removals) <- atomically $ do
            peersList <- kademliaGetKnownPeers kademliaInst
            let peersSet = S.fromList (addressToNodeId <$> peersList)
                additions = peersSet S.\\ lastSet
                removals = lastSet S.\\ peersSet
            when (S.null additions && S.null removals) retry
            return (additions, removals)
        logNotice $
            sformat ("Kademlia peer changes: adding '{"%shown%"}' removing '{"%shown%"}'")
                additions
                removals
        let peersToAdd = simplePeers ((,) peerType <$> S.toList additions)
        forM_ (S.toList removals) removeKnownPeer
        addKnownPeers peersToAdd
        let newSet = (lastSet S.\\ removals) `S.union` additions
        blockAndUpdate newSet
