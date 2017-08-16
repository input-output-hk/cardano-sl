{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Pos.Block.Logic.Event
       (
       -- * Running events and scenarios
         runBlockEvent
       , runBlockScenario
       , BlockScenarioErrorContext(..)
       , BlockScenarioResult(..)

       -- * Exceptions
       , SnapshotMissingEx(..)
       , DbNotEquivalentToSnapshot(..)
       ) where

import           Universum

import           Control.Lens              (_Right)
import           Control.Monad.Catch       (catch, fromException)
import qualified Data.List.NonEmpty        as NE
import qualified Data.Map                  as Map
import qualified Data.Text                 as T
import qualified Data.Text.Buildable
import           Formatting                (bprint, build, (%))
import           Serokell.Util             (listJson)

import           Pos.Block.Logic.VAR       (BlockLrcMode, rollbackBlocks,
                                            verifyAndApplyBlocks)
import           Pos.Block.Types           (Blund)
import           Pos.Core                  (HeaderHash, headerHash, getEpochOrSlot, unEpochOrSlot, prevBlockL, EpochOrSlot)
import           Pos.DB.Pure               (DBPureDiff, MonadPureDB, dbPureDiff,
                                            dbPureDump, dbPureReset)
import           Pos.Exception             (CardanoFatalError (..))
import           Pos.Generator.BlockEvent  (BlockApplyResult (..), BlockEvent,
                                            BlockEvent' (..), BlockRollbackFailure (..),
                                            BlockRollbackResult (..), BlockScenario,
                                            BlockScenario' (..), SnapshotId,
                                            SnapshotOperation (..), beaInput, beaOutValid,
                                            berInput, berOutValid)
import           Pos.Ssc.GodTossing.Type   (SscGodTossing)
import           Pos.Util.Chrono           (NE, OldestFirst, getOldestFirst)
import           Pos.Util.Util             (eitherToThrow, lensOf)
import           Test.Pos.Block.Logic.Mode (BlockTestContext, PureDBSnapshotsVar (..))
import           Test.Pos.Block.Logic.Util (withCurrentSlot)

data SnapshotMissingEx = SnapshotMissingEx SnapshotId
    deriving (Show)

instance Exception SnapshotMissingEx

data DbNotEquivalentToSnapshot = DbNotEquivalentToSnapshot SnapshotId DBPureDiff
    deriving (Show)

instance Exception DbNotEquivalentToSnapshot

newtype IsExpected = IsExpected Bool

data BlockEventResult
    = BlockEventSuccess IsExpected
    | BlockEventFailure IsExpected SomeException
    | BlockEventDbChanged DbNotEquivalentToSnapshot

verifyAndApplyBlocks' ::
       BlockLrcMode SscGodTossing BlockTestContext m
    => OldestFirst NE (Blund SscGodTossing)
    -> m ()
verifyAndApplyBlocks' bs = do
    let mSlot = (unEpochOrSlot . getEpochOrSlot . fst . NE.last . getOldestFirst) bs ^? _Right
    maybe identity withCurrentSlot mSlot $ do
        (_ :: HeaderHash) <- eitherToThrow =<<
            verifyAndApplyBlocks True (fst <$> bs)
        return ()

-- | Execute a single block event.
runBlockEvent ::
       BlockLrcMode SscGodTossing BlockTestContext m
    => BlockEvent
    -> m BlockEventResult

runBlockEvent (BlkEvApply ev) =
    (onSuccess <$ verifyAndApplyBlocks' (ev ^. beaInput))
        `catch` (return . onFailure)
  where
    onSuccess = case ev ^. beaOutValid of
        BlockApplySuccess -> BlockEventSuccess (IsExpected True)
        BlockApplyFailure -> BlockEventSuccess (IsExpected False)
    onFailure (e :: SomeException) = case ev ^. beaOutValid of
        BlockApplySuccess -> BlockEventFailure (IsExpected False) e
        BlockApplyFailure -> BlockEventFailure (IsExpected True) e

runBlockEvent (BlkEvRollback ev) =
    (onSuccess <$ rollbackBlocks (ev ^. berInput))
       `catch` (return . onFailure)
  where
    onSuccess = case ev ^. berOutValid of
        BlockRollbackSuccess   -> BlockEventSuccess (IsExpected True)
        BlockRollbackFailure _ -> BlockEventSuccess (IsExpected False)
    onFailure (e :: SomeException) = case ev ^. berOutValid of
        BlockRollbackSuccess -> BlockEventFailure (IsExpected False) e
        BlockRollbackFailure brf ->
            let
                isExpected = case brf of
                    BlkRbSecurityLimitExceeded
                        | Just cfe <- fromException e
                        , CardanoFatalError msg <- cfe
                        , "security risk" `T.isInfixOf` msg ->
                          True
                        | otherwise ->
                          False
            in
                BlockEventFailure (IsExpected isExpected) e

runBlockEvent (BlkEvSnap ev) =
    (onSuccess <$ runSnapshotOperation ev)
        `catch` (return . onFailure)
  where
    onSuccess = BlockEventSuccess (IsExpected True)
    onFailure = BlockEventDbChanged


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

data BlockScenarioErrorContext = BlockScenarioErrorContext
    { _bsecExecuted  :: ![BlockEvent] -- newest-first
    , _bsecFailed    :: !BlockEvent
    , _bsecRemaining :: ![BlockEvent] -- oldest-first
    }

data BlockSummary = BlockSummary
    { _bsEpochOrSlot    :: !EpochOrSlot
    , _bsPrevHeaderHash :: !HeaderHash
    , _bsHeaderHash     :: !HeaderHash
    }

instance Buildable BlockSummary where
    build (BlockSummary eos prevHh hh) = bprint (build % ": " % build % "->" % build) eos prevHh hh

instance Buildable BlockScenarioErrorContext where
    build bsec =
        bprint ("Executed: "%listJson%"\nRemaining: "%listJson%"\nFailed: "%build)
        (map toBlkSm . reverse $ _bsecExecuted bsec)
        (map toBlkSm $ _bsecRemaining bsec)
        (toBlkSm $ _bsecFailed bsec)
      where
        toBlockSummary blk = BlockSummary
            { _bsEpochOrSlot    = getEpochOrSlot blk
            , _bsPrevHeaderHash = headerHash (blk ^. prevBlockL)
            , _bsHeaderHash     = headerHash blk }
        toBlkSm = fmap (toBlockSummary . fst)

data BlockScenarioResult
    = BlockScenarioFinishedOk
    | BlockScenarioUnexpectedSuccess BlockScenarioErrorContext
    | BlockScenarioUnexpectedFailure BlockScenarioErrorContext SomeException
    | BlockScenarioDbChanged BlockScenarioErrorContext DbNotEquivalentToSnapshot

-- | Execute a block scenario: a sequence of block events that either ends with
-- an expected failure or with a rollback to the initial state.
runBlockScenario ::
       (BlockLrcMode SscGodTossing ctx m, MonadPureDB ctx m, ctx ~ BlockTestContext)
    => BlockScenario
    -> m BlockScenarioResult
runBlockScenario (BlockScenario evs) = runBlockScenario' [] evs

runBlockScenario' ::
       (BlockLrcMode SscGodTossing ctx m, MonadPureDB ctx m, ctx ~ BlockTestContext)
    => [BlockEvent] -- executed (newest-first)
    -> [BlockEvent] -- remaining (oldest-first)
    -> m BlockScenarioResult
runBlockScenario' _ [] =
    return BlockScenarioFinishedOk
runBlockScenario' prevEvs (ev:evs) = do
    runBlockEvent ev >>= \case
        BlockEventSuccess (IsExpected isExp) ->
            if isExp
                then runBlockScenario' (ev:prevEvs) evs
                else return $ BlockScenarioUnexpectedSuccess bsec
        BlockEventFailure (IsExpected isExp) e ->
            return $ if isExp
                then BlockScenarioFinishedOk
                else BlockScenarioUnexpectedFailure bsec e
        BlockEventDbChanged d ->
            return $ BlockScenarioDbChanged bsec d
  where
    bsec = BlockScenarioErrorContext
        { _bsecExecuted  = prevEvs
        , _bsecFailed    = ev
        , _bsecRemaining = evs }
