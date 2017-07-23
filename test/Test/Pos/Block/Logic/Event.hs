module Test.Pos.Block.Logic.Event
    ( runBlockEvent
    , runBlockScenario
    , BlockScenarioResult(..)
    ) where

import           Universum

import           Control.Monad.Catch      (catch)

import           Pos.Block.Logic.VAR      (BlockLrcMode, applyBlocks, rollbackBlocks)
import           Pos.DB.Pure              (DBPureDiff, MonadPureDB, dbPureDiff,
                                           dbPureDump)
import           Pos.Generator.BlockEvent (BlockEvent, BlockEvent' (..),
                                           IsBlockEventFailure (..), beaInput, berInput)
import           Pos.Ssc.GodTossing.Type  (SscGodTossing)

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

-- | Execute a single block event.
runBlockEvent ::
       BlockLrcMode SscGodTossing ctx m
    => BlockEvent
    -> m BlockEventResult
runBlockEvent (BlkEvApply ev) =
   (BlockEventSuccess <$ applyBlocks True Nothing (ev ^. beaInput))
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
       (BlockLrcMode SscGodTossing ctx m, MonadPureDB ctx m)
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
