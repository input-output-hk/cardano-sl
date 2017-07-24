{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pos.Block.Logic.Event
    ( runBlockEvent
    , runBlockScenario
    , BlockScenarioResult(..)
    ) where

import           Universum

import           Control.Monad.Catch      (catch)

import           Pos.Block.Logic.VAR      (BlockLrcMode, rollbackBlocks,
                                           verifyAndApplyBlocks)
import           Pos.Block.Types          (Blund)
import           Pos.Core                 (HeaderHash)
import           Pos.DB.Pure              (DBPureDiff, MonadPureDB, dbPureDiff,
                                           dbPureDump)
import           Pos.Generator.BlockEvent (BlockEvent, BlockEvent' (..),
                                           IsBlockEventFailure (..), beaInput, berInput)
import           Pos.Ssc.GodTossing.Type  (SscGodTossing)
import           Pos.Util.Chrono          (NE, OldestFirst)
import           Pos.Util.Util            (eitherToThrow)

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
       BlockLrcMode ssc ctx m
    => OldestFirst NE (Blund ssc)
    -> m ()
verifyAndApplyBlocks' bs = do
    (_ :: HeaderHash) <- eitherToThrow identity =<<
        verifyAndApplyBlocks True (fst <$> bs)
    return ()

-- | Execute a single block event.
runBlockEvent ::
       BlockLrcMode SscGodTossing ctx m
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
