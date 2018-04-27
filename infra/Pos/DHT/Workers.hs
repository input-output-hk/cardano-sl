{-# LANGUAGE MultiParamTypeClasses #-}

module Pos.DHT.Workers
       ( DhtWorkMode
       , dhtWorkers
       ) where

import           Universum

import qualified Data.ByteString.Lazy as BSL
import           Formatting (sformat, (%))
import           Mockable (Async, Delay, Mockable)
import           Network.Kademlia (takeSnapshot)
import           System.Wlog (WithLogger, logNotice)

import           Pos.Binary.Class (serialize)
import           Pos.Binary.Infra.DHTModel ()
import           Pos.Communication.Protocol (OutSpecs)
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Slotting (flattenSlotId, slotIdF)
import           Pos.DHT.Constants (kademliaDumpInterval)
import           Pos.DHT.Real.Types (KademliaDHTInstance (..))
import           Pos.Recovery.Info (MonadRecoveryInfo, recoveryCommGuard)
import           Pos.Reporting (MonadReporting)
import           Pos.Shutdown (HasShutdownContext)
import           Pos.Slotting.Class (MonadSlots)
import           Pos.Slotting.Util (defaultOnNewSlotParams)
import           Pos.Worker.Types (WorkerSpec, localOnNewSlotWorker)

type DhtWorkMode ctx m =
    ( WithLogger m
    , MonadSlots ctx m
    , MonadIO m
    , MonadMask m
    , Mockable Async m
    , Mockable Delay m
    , MonadRecoveryInfo m
    , MonadReader ctx m
    , MonadReporting ctx m
    , HasShutdownContext ctx
    , HasConfiguration
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
dumpKademliaStateWorker kademliaInst = localOnNewSlotWorker onsp $ \slotId ->
    when (isTimeToDump slotId) $ recoveryCommGuard "dump kademlia state" $ do
        let dumpFile = kdiDumpPath kademliaInst
        logNotice $ sformat ("Dumping kademlia snapshot on slot: "%slotIdF) slotId
        let inst = kdiHandle kademliaInst
        snapshot <- liftIO $ takeSnapshot inst
        case dumpFile of
            Just fp -> liftIO . BSL.writeFile fp . serialize $ snapshot
            Nothing -> return ()
  where
    onsp = defaultOnNewSlotParams
    isTimeToDump slotId = flattenSlotId slotId `mod` kademliaDumpInterval == 0
