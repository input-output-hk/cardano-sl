{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logic of local data processing in Update System.

module Pos.Update.Logic.Global
       ( usApplyBlocks
       , usRollbackBlocks
       , usVerifyBlocks
       ) where

import           Control.Monad.Except      (MonadError, runExceptT)
import           Data.Default              (Default (def))
import qualified Data.HashMap.Strict       as HM
import           System.Wlog               (WithLogger, logError)
import           Universum

import qualified Pos.DB                    as DB
import           Pos.DB.GState             (UpdateOp (..))
import           Pos.Script.Type           (ScriptVersion)
import           Pos.Types                 (ApplicationName, Block, NEBlocks,
                                            NumSoftwareVersion, ProtocolVersion,
                                            SoftwareVersion (..), difficultyL, gbBody,
                                            gbHeader, mbUpdatePayload)
import           Pos.Update.Core           (UpId)
import           Pos.Update.Error          (USError (USInternalError))
import           Pos.Update.Poll           (DBPoll, MonadPoll, PollModifier (..), PollT,
                                            PollVerFailure, ProposalState, USUndo,
                                            execPollT, rollbackUSPayload, runDBPoll,
                                            runPollT, verifyAndApplyUSPayload)
import           Pos.Update.Poll.RollTrans (execRollT)
import           Pos.Util                  (Color (Red), colorize, inAssertMode)

type USGlobalApplyMode endless_useless m = (WithLogger m, DB.MonadDB endless_useless m)
type USGlobalVerifyMode ы m = (DB.MonadDB ы m, MonadError PollVerFailure m)

-- TODO: I suppose blocks are needed here only for sanity check, but who knows.
-- Anyway, it's ok.
-- TODO: but actually I suppose that such sanity checks should be done at higher
-- level.
-- | Apply chain of /definitely/ valid blocks to US part of GState DB
-- and to US local data. Head must be the __oldest__ block.  This
-- function assumes that no other thread applies block in parallel. It
-- also assumes that parent of oldest block is current tip.
usApplyBlocks
    :: (MonadThrow m, USGlobalApplyMode ssc m)
    => NEBlocks ssc -> PollModifier -> m [DB.SomeBatchOp]
usApplyBlocks blocks _ = do
    inAssertMode $ do
        verdict <- runExceptT $ usVerifyBlocks blocks
        either onFailure (const pass) verdict
    return []
  where
    onFailure failure = do
        let msg = "usVerifyBlocks failed in 'apply': " <> pretty failure
        logError $ colorize Red msg
        throwM $ USInternalError msg

-- | Revert application of given blocks to US part of GState DB
-- and US local data. Head must be the __youngest__ block. Caller must
-- ensure that tip stored in DB is 'headerHash' of head.
usRollbackBlocks
    :: forall ssc m.
       USGlobalApplyMode ssc m
    => NonEmpty (Block ssc, USUndo) -> m [DB.SomeBatchOp]
usRollbackBlocks blunds =
    modifierToBatch <$> (runDBPoll . execPollT def $ mapM_ rollbackDo blunds)
  where
    rollbackDo :: (Block ssc, USUndo) -> PollT (DBPoll m) ()
    rollbackDo (Left _, _) = pass
    rollbackDo (Right blk, undo) = do
        verdict <-
            runExceptT $ rollbackUSPayload
                (blk ^. difficultyL)
                (blk ^. gbBody . mbUpdatePayload)
                undo
        either onFailure (const pass) verdict
    onFailure failure = do
        let msg = "usRollbackBlocks failed: " <> pretty failure
        logError $ colorize Red msg
        throwM $ USInternalError msg

-- | Verify whether sequence of blocks can be applied to US part of
-- current GState DB.  This function doesn't make pure checks, they
-- are assumed to be done earlier, most likely during objects
-- construction.
usVerifyBlocks
    :: USGlobalVerifyMode ssc m
    => NEBlocks ssc -> m (PollModifier, NonEmpty USUndo)
usVerifyBlocks blocks = swap <$> run (mapM verifyBlock blocks)
  where
    run = runDBPoll . runPollT def

verifyBlock
    :: (USGlobalVerifyMode ssc m, MonadPoll m)
    => Block ssc -> m USUndo
verifyBlock (Left _)    = pure def
verifyBlock (Right blk) = execRollT $ do
    verifyAndApplyUSPayload
        True
        (Right $ blk ^. gbHeader)
        (blk ^. gbBody . mbUpdatePayload)

----------------------------------------------------------------------------
-- Conversion to batch
----------------------------------------------------------------------------

modifierToBatch :: PollModifier -> [DB.SomeBatchOp]
modifierToBatch PollModifier {..} =
    concat $
    [ scModifierToBatch pmNewScriptVersions pmDelScriptVersions
    , pvModifierToBatch pmLastAdoptedPV
    , confirmedModifierToBatch pmNewConfirmed pmDelConfirmed
    , upModifierToBatch pmNewActiveProps pmDelActivePropsIdx
    ]

scModifierToBatch
    :: HashMap ProtocolVersion ScriptVersion
    -> HashSet ProtocolVersion
    -> [DB.SomeBatchOp]
scModifierToBatch (HM.toList -> added) (toList -> deleted) = addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . uncurry SetScriptVersion) added
    delOps = map (DB.SomeBatchOp . DelScriptVersion) deleted

pvModifierToBatch :: Maybe ProtocolVersion -> [DB.SomeBatchOp]
pvModifierToBatch Nothing  = []
pvModifierToBatch (Just v) = [DB.SomeBatchOp $ SetLastPV v]

confirmedModifierToBatch :: HashMap ApplicationName NumSoftwareVersion
                         -> HashSet ApplicationName
                         -> [DB.SomeBatchOp]
confirmedModifierToBatch (HM.toList -> added) (toList -> deleted) = addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . ConfirmVersion . uncurry SoftwareVersion) added
    delOps = map (DB.SomeBatchOp . DelConfirmedVersion) deleted

upModifierToBatch :: HashMap UpId ProposalState
                  -> HashMap ApplicationName UpId
                  -> [DB.SomeBatchOp]
upModifierToBatch (toList -> added) (HM.toList -> deleted) = addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . PutProposal) added
    delOps = map (DB.SomeBatchOp . uncurry (flip DeleteProposal)) deleted
