{-# LANGUAGE TypeFamilies #-}

module Test.Pos.Block.Logic.Event
       (
       -- * Running events and scenarios
         runBlockEvent
       , runBlockScenario
       , BlockScenarioResult(..)

       -- * Exceptions
       , SnapshotMissingEx(..)
       , DbNotEquivalentToSnapshot(..)
       ) where

import           Universum

import           Control.Monad.Catch       (catch)
import qualified Data.Map                  as Map

import           Pos.Block.Logic.VAR       (BlockLrcMode, rollbackBlocks,
                                            verifyAndApplyBlocks)
import           Pos.Block.Types           (Blund)
import           Pos.Core                  (HeaderHash)
import           Pos.DB.Pure               (DBPureDiff, MonadPureDB, dbPureDiff,
                                            dbPureDump, dbPureReset)
import           Pos.Generator.BlockEvent  (BlockEvent, BlockEvent' (..), BlockScenario,
                                            BlockScenario' (..), IsBlockEventFailure (..),
                                            SnapshotId, SnapshotOperation (..), beaInput,
                                            berInput)
import           Pos.Ssc.GodTossing.Type   (SscGodTossing)
import           Pos.Util.Chrono           (NE, OldestFirst)
import           Pos.Util.Util             (eitherToThrow, lensOf)
import           Test.Pos.Block.Logic.Mode (BlockTestContext, PureDBSnapshotsVar (..))
import           Test.Pos.Block.Logic.Util (satisfySlotCheck)

data SnapshotMissingEx = SnapshotMissingEx SnapshotId
    deriving (Show)

instance Exception SnapshotMissingEx

data DbNotEquivalentToSnapshot = DbNotEquivalentToSnapshot SnapshotId DBPureDiff
    deriving (Show)

instance Exception DbNotEquivalentToSnapshot

newtype IsExpected = IsExpected Bool

data BlockEventResult
    = BlockEventSuccess
    | BlockEventFailure IsExpected SomeException
    | BlockEventDbChanged DbNotEquivalentToSnapshot

mkBlockEventFailure ::
       IsBlockEventFailure ev
    => ev
    -> SomeException
    -> BlockEventResult
mkBlockEventFailure ev =
    BlockEventFailure (IsExpected (isBlockEventFailure ev))

verifyAndApplyBlocks' ::
       BlockLrcMode SscGodTossing BlockTestContext m
    => OldestFirst NE (Blund SscGodTossing)
    -> m ()
verifyAndApplyBlocks' blunds = do
    satisfySlotCheck blocks $ do
        (_ :: HeaderHash) <- eitherToThrow =<<
            verifyAndApplyBlocks True blocks
        return ()
  where
    blocks = fst <$> blunds

-- | Execute a single block event.
runBlockEvent ::
       BlockLrcMode SscGodTossing BlockTestContext m
    => BlockEvent
    -> m BlockEventResult
runBlockEvent (BlkEvApply ev) =
   (BlockEventSuccess <$ verifyAndApplyBlocks' (ev ^. beaInput))
      `catch` (return . mkBlockEventFailure ev)
runBlockEvent (BlkEvRollback ev) =
   (BlockEventSuccess <$ rollbackBlocks (ev ^. berInput))
      `catch` (return . mkBlockEventFailure ev)
runBlockEvent (BlkEvSnap ev) =
   (BlockEventSuccess <$ runSnapshotOperation ev)
      `catch` (return . BlockEventDbChanged)

-- | Execute a snapshot operation.
runSnapshotOperation ::
       MonadPureDB BlockTestContext m
    => SnapshotOperation
    -> m ()
runSnapshotOperation snapOp = do
    PureDBSnapshotsVar snapsRef <- view (lensOf @PureDBSnapshotsVar)
    case snapOp of
        SnapshotSave snapId -> do
            currentDbState <- dbPureDump
            modifyIORef snapsRef $ Map.insert snapId currentDbState
        SnapshotLoad snapId -> do
            snap <- getSnap snapsRef snapId
            dbPureReset snap
        SnapshotEq snapId -> do
            currentDbState <- dbPureDump
            snap <- getSnap snapsRef snapId
            whenJust (dbPureDiff snap currentDbState) $ \dbDiff ->
                throwM $ DbNotEquivalentToSnapshot snapId dbDiff
  where
    getSnap snapsRef snapId = do
        mSnap <- Map.lookup snapId <$> readIORef snapsRef
        maybe (throwM $ SnapshotMissingEx snapId) return mSnap

data BlockScenarioResult
    = BlockScenarioFinishedOk
    | BlockScenarioUnexpectedFailure SomeException
    | BlockScenarioDbChanged DbNotEquivalentToSnapshot

-- | Execute a block scenario: a sequence of block events that either ends with
-- an expected failure or with a rollback to the initial state.
runBlockScenario ::
       (BlockLrcMode SscGodTossing ctx m, MonadPureDB ctx m, ctx ~ BlockTestContext)
    => BlockScenario
    -> m BlockScenarioResult
runBlockScenario (BlockScenario []) =
    return BlockScenarioFinishedOk
runBlockScenario (BlockScenario (ev:evs)) = do
    runBlockEvent ev >>= \case
        BlockEventSuccess ->
            runBlockScenario (BlockScenario evs)
        BlockEventFailure (IsExpected isExp) e ->
            return $ if isExp
                then BlockScenarioFinishedOk
                else BlockScenarioUnexpectedFailure e
        BlockEventDbChanged d ->
            return $ BlockScenarioDbChanged d
