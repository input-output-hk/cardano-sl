module Pos.Generator.BlockEvent.DSL
       (
         -- * Core re-exports
         BlockDesc(..)
       , PathSegment
       , Path
       , pathSequence
       , BlockApplyResult(..)
       , BlockRollbackResult(..)
       , BlockRollbackFailure(..)
       , BlockScenario
       , BlockScenario'
       , CheckCount(..)
       , enrichWithSnapshotChecking
       , Chance(..)
       , byChance
         -- * The monad
       , BlockEventGenT
       , runBlockEventGenT
       , runBlockEventGenT'
         -- * The operations
       , describeBlock
       , emitBlockApply
       , emitBlockRollback
       , snapshotSave
       , snapshotLoad
       , snapshotEq
       ) where

import           Universum

import           Control.Lens (at, makeLenses, (%=), (.=))
import           Control.Monad.Random.Strict (RandT, RandomGen, mapRandT)
import qualified Data.Map as Map

import           Pos.AllSecrets (AllSecrets)
import           Pos.Core (GenesisWStakeholders)
import           Pos.Generator.Block (BlockTxpGenMode, MonadBlockGen)
import           Pos.Generator.BlockEvent (BlockApplyResult (..), BlockDesc (..), BlockEvent' (..),
                                           BlockEventApply' (..), BlockEventRollback' (..),
                                           BlockRollbackFailure (..), BlockRollbackResult (..),
                                           BlockScenario, BlockScenario' (..), Chance (..),
                                           CheckCount (..), Path, PathSegment, SnapshotId,
                                           SnapshotOperation (..), byChance,
                                           enrichWithSnapshotChecking, genBlocksInStructure,
                                           pathSequence)
import           Pos.Txp.Configuration (HasTxpConfiguration)
import           Pos.Util.Chrono (NE, NewestFirst (..), OldestFirst (..), toOldestFirst,
                                  _NewestFirst)

data BlockEventGenState = BlockEventGenState
    { _begsEvents      :: !(NewestFirst [] (BlockEvent' Path))
    , _begsAnnotations :: !(Map Path BlockDesc)
    } deriving ()

makeLenses 'BlockEventGenState

type BlockEventGenT g m = RandT g (StateT BlockEventGenState m)

describeBlock ::
       MonadState BlockEventGenState m
    => Path -- ^ path to the block in the blockchain tree
    -> BlockDesc -- ^ block description
    -> m ()
describeBlock path blockDesc =
    begsAnnotations . at path .= Just blockDesc

emitEvent ::
       MonadState BlockEventGenState m
    => BlockEvent' Path
    -> m ()
emitEvent ev = begsEvents . _NewestFirst %= (ev:)

emitBlockApply ::
       MonadState BlockEventGenState m
    => BlockApplyResult -- ^ expected result
    -> OldestFirst NE Path -- ^ blocks to apply
    -> m ()
emitBlockApply res blocks = emitEvent $
    BlkEvApply BlockEventApply
        { _beaInput = blocks
        , _beaOutValid = res
        }

emitBlockRollback ::
       MonadState BlockEventGenState m
    => BlockRollbackResult -- ^ expected result
    -> NewestFirst NE Path -- ^ blocks to rollback
    -> m ()
emitBlockRollback res blocks = emitEvent $
    BlkEvRollback BlockEventRollback
        { _berInput = blocks
        , _berOutValid = res
        }

snapshotSave ::
       MonadState BlockEventGenState m
    => SnapshotId
    -> m ()
snapshotSave snapshotId = emitEvent $
    BlkEvSnap (SnapshotSave snapshotId)

snapshotLoad ::
       MonadState BlockEventGenState m
    => SnapshotId
    -> m ()
snapshotLoad snapshotId = emitEvent $
    BlkEvSnap (SnapshotLoad snapshotId)

snapshotEq ::
       MonadState BlockEventGenState m
    => SnapshotId
    -> m ()
snapshotEq snapshotId = emitEvent $
    BlkEvSnap (SnapshotEq snapshotId)

runBlockEventGenT
    :: (HasTxpConfiguration, BlockTxpGenMode g ctx m)
    => AllSecrets
    -> GenesisWStakeholders
    -> BlockEventGenT g m ()
    -> RandT g m BlockScenario
runBlockEventGenT secrets genStakeholders m = do
    (annotations, preBlockScenario) <- runBlockEventGenT' m
    genBlocksInStructure secrets genStakeholders annotations preBlockScenario

runBlockEventGenT' ::
    (RandomGen g, MonadBlockGen ctx m) =>
    BlockEventGenT g m () ->
    RandT g m (Map Path BlockDesc, BlockScenario' Path)
runBlockEventGenT' m = do
    let
        initialBlockEventGenState = BlockEventGenState
            { _begsEvents = NewestFirst []
            , _begsAnnotations = Map.empty
            }
    begs <-
        let reorder (((), g), begs) = (begs, g)
        in mapRandT (fmap reorder . flip runStateT initialBlockEventGenState) m
    let
        OldestFirst preBlockEvents = toOldestFirst (begs ^. begsEvents)
        annotations = begs ^. begsAnnotations
    return (annotations, BlockScenario preBlockEvents)
