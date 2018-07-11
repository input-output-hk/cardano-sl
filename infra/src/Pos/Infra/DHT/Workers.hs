{-# LANGUAGE MultiParamTypeClasses #-}

module Pos.Infra.DHT.Workers
       ( DhtWorkMode
       , dhtWorkers
       ) where

import           Universum

import qualified Data.ByteString.Lazy as BSL
import           Formatting (sformat, (%))
import           Mockable (Async, Delay, Mockable)
import           Network.Kademlia (takeSnapshot)

import           Pos.Binary.Class (serialize)
import           Pos.Core (HasProtocolConstants)
import           Pos.Core.Slotting (flattenSlotId, slotIdF)
import           Pos.Infra.Binary.DHTModel ()
import           Pos.Infra.DHT.Constants (kademliaDumpInterval)
import           Pos.Infra.DHT.Real.Types (KademliaDHTInstance (..))
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Recovery.Info (MonadRecoveryInfo, recoveryCommGuard)
import           Pos.Infra.Shutdown (HasShutdownContext)
import           Pos.Infra.Slotting.Util (defaultOnNewSlotParams, onNewSlot)
import           Pos.Sinbin.Reporting (MonadReporting)
import           Pos.Sinbin.Slotting.Class (MonadSlots)
import           Pos.Util.Trace.Named (TraceNamed, logNotice)

type DhtWorkMode ctx m =
    ( MonadSlots ctx m
    , MonadIO m
    , MonadMask m
    , Mockable Async m
    , Mockable Delay m
    , MonadRecoveryInfo m
    , MonadReader ctx m
    , MonadReporting m
    , HasShutdownContext ctx
    )

dhtWorkers
    :: ( DhtWorkMode ctx m
       , HasProtocolConstants
       )
    => TraceNamed m
    -> KademliaDHTInstance -> [Diffusion m -> m ()]
dhtWorkers logTrace kademliaInst@KademliaDHTInstance {..} =
    [ dumpKademliaStateWorker logTrace kademliaInst ]

dumpKademliaStateWorker
    :: ( DhtWorkMode ctx m
       , HasProtocolConstants
       )
    => TraceNamed m
    -> KademliaDHTInstance
    -> Diffusion m
    -> m ()
dumpKademliaStateWorker logTrace kademliaInst =
    \_ -> onNewSlot logTrace onsp $ \slotId ->
    when (isTimeToDump slotId) $ recoveryCommGuard logTrace "dump kademlia state" $ do
        let dumpFile = kdiDumpPath kademliaInst
        logNotice logTrace $ sformat ("Dumping kademlia snapshot on slot: "%slotIdF) slotId
        let inst = kdiHandle kademliaInst
        snapshot <- liftIO $ takeSnapshot inst
        case dumpFile of
            Just fp -> liftIO . BSL.writeFile fp . serialize $ snapshot
            Nothing -> return ()
  where
    onsp = defaultOnNewSlotParams
    isTimeToDump slotId = flattenSlotId slotId `mod` kademliaDumpInterval == 0
