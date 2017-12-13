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
import           Pos.Communication.Types.Protocol (OutSpecs)
import           Pos.Communication.Protocol (OnNewSlotComm, LocalOnNewSlotComm)
import           Pos.Communication.Util (Action, ActionSpec (..), toAction, localSpecs)
import           Pos.Slotting (SlotId, onNewSlot)

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
    :: OnNewSlotComm ctx m
    => Bool -> (SlotId -> WorkerSpec m, outSpecs) -> (WorkerSpec m, outSpecs)
onNewSlot' startImmediately (h, outs) =
    (,outs) . ActionSpec $ \sA ->
        onNewSlot startImmediately $
            \slotId -> let ActionSpec h' = h slotId
                        in h' sA
onNewSlotWorker
    :: OnNewSlotComm ctx m
    => Bool -> OutSpecs -> (SlotId -> Worker m) -> (WorkerSpec m, OutSpecs)
onNewSlotWorker b outs = onNewSlot' b . workerHelper outs

localOnNewSlotWorker
    :: LocalOnNewSlotComm ctx m
    => Bool -> (SlotId -> m ()) -> (WorkerSpec m, OutSpecs)
localOnNewSlotWorker b h = (ActionSpec $ \__sA -> onNewSlot b h, mempty)

localWorker :: m () -> (WorkerSpec m, OutSpecs)
localWorker = localSpecs
