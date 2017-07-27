{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pos.Block.Logic.Event
    ( runBlockEvent
    , runBlockScenario
    , BlockScenarioResult(..)
    ) where

import           Universum

import           Control.Lens              (_Right)
import           Control.Monad.Catch       (catch)
import qualified Data.List.NonEmpty        as NE

import           Pos.Block.Logic.VAR       (BlockLrcMode, rollbackBlocks,
                                            verifyAndApplyBlocks)
import           Pos.Block.Types           (Blund)
import           Pos.Core                  (HeaderHash, getEpochOrSlot, unEpochOrSlot)
import           Pos.DB.Pure               (DBPureDiff, MonadPureDB, dbPureDiff,
                                            dbPureDump)
import           Pos.Generator.BlockEvent  (BlockEvent, BlockEvent' (..),
                                            IsBlockEventFailure (..), beaInput, berInput)
import           Pos.Ssc.GodTossing.Type   (SscGodTossing)
import           Pos.Util.Chrono           (NE, OldestFirst, getOldestFirst)
import           Pos.Util.Util             (eitherToThrow)
import           Test.Pos.Block.Logic.Mode (BlockTestContext)
import           Test.Pos.Block.Logic.Util (withCurrentSlot)

newtype IsExpected = IsExpected Bool

data BlockEventResult
    = BlockEventSuccess
    | BlockEventFailure IsExpected SomeException

mkBlockEventFailure ::
       IsBlockEventFailure ev
    => ev
    -> SomeException
    -> BlockEventResult
mkBlockEventFailure ev =
    BlockEventFailure (IsExpected (isBlockEventFailure ev))

verifyAndApplyBlocks' ::
       (BlockLrcMode SscGodTossing ctx m, MonadReader BlockTestContext m)
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
       (BlockLrcMode SscGodTossing ctx m, MonadReader BlockTestContext m)
    => BlockEvent
    -> m BlockEventResult
runBlockEvent (BlkEvApply ev) =
   (BlockEventSuccess <$ verifyAndApplyBlocks' (ev ^. beaInput))
      `catch` (return . mkBlockEventFailure ev)
runBlockEvent (BlkEvRollback ev) =
   (BlockEventSuccess <$ rollbackBlocks (ev ^. berInput))
      `catch` (return . mkBlockEventFailure ev)

data BlockScenarioResult
    = BlockScenarioFinishedOk
    | BlockScenarioUnexpectedFailure SomeException
    | BlockScenarioDbChanged DBPureDiff

-- | Execute a block scenario: a sequence of block events that either ends with
-- an expected failure or with a rollback to the initial state.
runBlockScenario ::
       (BlockLrcMode SscGodTossing ctx m, MonadPureDB ctx m, MonadReader BlockTestContext m)
    => [BlockEvent]
    -> m BlockScenarioResult
runBlockScenario events = do
    dbBeforeEvents <- dbPureDump
    let
        runBlockScenario' [] = do
            dbAfterEvents <- dbPureDump
            return $ case dbPureDiff dbBeforeEvents dbAfterEvents of
                Nothing     -> BlockScenarioFinishedOk
                Just dbDiff -> BlockScenarioDbChanged dbDiff
        runBlockScenario' (ev:evs) = do
            evRes <- runBlockEvent ev
            case evRes of
                BlockEventSuccess -> runBlockScenario' evs
                BlockEventFailure (IsExpected isExp) e ->
                    return $ if isExp
                        then BlockScenarioFinishedOk
                        else BlockScenarioUnexpectedFailure e
    runBlockScenario' events
