-- | Port of the old Worker stuff from cardano-sl-infra

module Pos.Worker.Types
    ( Worker
    , WorkerSpec
    , worker
    , worker'
    , workerHelper
    , onNewSlot'
    , onNewSlotWorker
    , localOnNewSlotWorker
    , localWorker
    ) where

import           Universum

import           Pos.Communication.Protocol (LocalOnNewSlotComm, OnNewSlotComm)
import           Pos.Communication.Types.Protocol (OutSpecs)
import           Pos.Communication.Util (Action, ActionSpec (..), localSpecs, toAction)
import           Pos.Slotting (OnNewSlotParams (..), SlotId, onNewSlot)
import           Pos.Core (HasProtocolConstants)

type Worker m = Action m ()
type WorkerSpec m = ActionSpec m ()

worker :: OutSpecs -> Worker m -> (WorkerSpec m, OutSpecs)
worker outSpecs = (,outSpecs) . toAction

workerHelper :: OutSpecs -> (arg -> Worker m) -> (arg -> WorkerSpec m, OutSpecs)
workerHelper outSpecs h = (,outSpecs) $ toAction . h

worker' :: OutSpecs -> Worker m -> (WorkerSpec m, OutSpecs)
worker' outSpecs h =
    (,outSpecs) $ ActionSpec $ h

onNewSlot'
    :: (OnNewSlotComm ctx m, HasProtocolConstants)
    => OnNewSlotParams -> (SlotId -> WorkerSpec m, outSpecs) -> (WorkerSpec m, outSpecs)
onNewSlot' params (h, outs) =
    (,outs) . ActionSpec $ \sA ->
        onNewSlot params $
            \slotId -> let ActionSpec h' = h slotId
                        in h' sA
onNewSlotWorker
    :: (OnNewSlotComm ctx m, HasProtocolConstants)
    => OnNewSlotParams -> OutSpecs -> (SlotId -> Worker m) -> (WorkerSpec m, OutSpecs)
onNewSlotWorker params outs = onNewSlot' params . workerHelper outs

localOnNewSlotWorker
    :: (LocalOnNewSlotComm ctx m, HasProtocolConstants)
    => OnNewSlotParams -> (SlotId -> m ()) -> (WorkerSpec m, OutSpecs)
localOnNewSlotWorker params h = (ActionSpec $ \__sA -> onNewSlot params h, mempty)

localWorker :: m () -> (WorkerSpec m, OutSpecs)
localWorker = localSpecs
