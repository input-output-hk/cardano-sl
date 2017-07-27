{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.DHT.Workers
       ( DhtWorkMode
       , dhtWorkers
       ) where

import           Universum

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Store                 as Store
import           Formatting                 (sformat, (%))
import           Mockable                   (Delay, Async, Fork, Mockable, Catch)
import           Network.Kademlia           (takeSnapshot)
import           System.Wlog                (WithLogger, logNotice)

import           Pos.Binary.Infra.DHTModel  ()
import           Pos.Communication.Protocol (OutSpecs, WorkerSpec,
                                             localOnNewSlotWorker)
import           Pos.Core.Slotting          (flattenSlotId)
import           Pos.Core.Types             (slotIdF)
import           Pos.DHT.Constants          (kademliaDumpInterval)
import           Pos.DHT.Real.Types         (KademliaDHTInstance (..))
import           Pos.KnownPeers             (MonadKnownPeers, MonadFormatPeers)
import           Pos.Recovery.Info          (MonadRecoveryInfo, recoveryCommGuard)
import           Pos.Reporting              (HasReportingContext)
import           Pos.Shutdown               (HasShutdownContext)
import           Pos.Slotting.Class         (MonadSlots)

type DhtWorkMode ctx m =
    ( WithLogger m
    , MonadSlots m
    , MonadIO m
    , MonadMask m
    , Mockable Async m
    , Mockable Fork m
    , Mockable Delay m
    , Mockable Catch m
    , MonadRecoveryInfo m
    , MonadReader ctx m
    , MonadKnownPeers m
    , MonadFormatPeers m
    , HasReportingContext ctx
    , HasShutdownContext ctx
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
