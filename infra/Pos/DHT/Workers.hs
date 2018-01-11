{-# LANGUAGE MultiParamTypeClasses #-}

module Pos.DHT.Workers
       ( DhtWorkMode
       , dhtWorkers
       ) where

import           Universum

import qualified Data.ByteString.Lazy as BSL
import           Formatting (sformat, (%))
import           Mockable (Async, Delay, Fork, Mockable)
import           Network.Kademlia (takeSnapshot)
import           System.Wlog (WithLogger, logNotice)

import           Pos.Binary.Class (serialize)
import           Pos.Binary.Infra.DHTModel ()
import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, localOnNewSlotWorker)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Slotting (flattenSlotId, slotIdF)
import           Pos.DHT.Configuration (kademliaDumpInterval)
import           Pos.DHT.Real.Types (KademliaDHTInstance (..))
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.KnownPeers (MonadKnownPeers)
import           Pos.Recovery.Info (MonadRecoveryInfo, recoveryCommGuard)
import           Pos.Reporting (MonadReporting)
import           Pos.Shutdown (HasShutdownContext)
import           Pos.Slotting.Class (MonadSlots)

type DhtWorkMode ctx m =
    ( WithLogger m
    , MonadSlots ctx m
    , MonadIO m
    , MonadMask m
    , Mockable Async m
    , Mockable Fork m
    , Mockable Delay m
    , MonadRecoveryInfo m
    , MonadReader ctx m
    , MonadReporting ctx m
    , MonadKnownPeers m
    , HasShutdownContext ctx
    , HasConfiguration
    , HasInfraConfiguration
    )

dhtWorkers
    :: ( DhtWorkMode ctx m
       )
    => KademliaDHTInstance -> ([WorkerSpec m], OutSpecs)
dhtWorkers kademliaInst@KademliaDHTInstance {..} = mconcat
    [ first pure (dumpKademliaStateWorker kademliaInst) ]

dumpKademliaStateWorker
    :: ( DhtWorkMode ctx m
       )
    => KademliaDHTInstance
    -> (WorkerSpec m, OutSpecs)
dumpKademliaStateWorker kademliaInst = localOnNewSlotWorker True $ \slotId ->
    when (isTimeToDump slotId) $ recoveryCommGuard "dump kademlia state" $ do
        let dumpFile = kdiDumpPath kademliaInst
        logNotice $ sformat ("Dumping kademlia snapshot on slot: "%slotIdF) slotId
        let inst = kdiHandle kademliaInst
        snapshot <- liftIO $ takeSnapshot inst
        case dumpFile of
            Just fp -> liftIO . BSL.writeFile fp . serialize $ snapshot
            Nothing -> return ()
  where
    isTimeToDump slotId = flattenSlotId slotId `mod` kademliaDumpInterval == 0
