{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Logic of local data processing in Update System.

module Pos.Update.Logic.Global
       ( usApplyBlocks
       , usCanCreateBlock
       , usRollbackBlocks
       , usVerifyBlocks
       ) where

import           Control.Monad.Except (MonadError, runExceptT)
import           Data.Default         (Default (def))
import qualified Data.HashMap.Strict  as HM
import           System.Wlog          (WithLogger, logError, modifyLoggerName)
import           Universum

import           Pos.Constants        (lastKnownBlockVersion)
import           Pos.Context          (WithNodeContext)
import qualified Pos.DB               as DB
import           Pos.DB.GState        (UpdateOp (..))
import           Pos.Ssc.Class        (SscHelpersClass)
import           Pos.Types            (ApplicationName, Block, BlockVersion,
                                       NumSoftwareVersion, SoftwareVersion (..),
                                       addressHash, blockSize, blockSlot, epochIndexL,
                                       gbBody, gbHeader, gbhConsensus, gbhExtra,
                                       headerHash, mbUpdatePayload, mcdLeaderKey,
                                       mehBlockVersion)
import           Pos.Update.Core      (BlockVersionData, UpId)
import           Pos.Update.Error     (USError (USInternalError))
import           Pos.Update.Poll      (BlockVersionState, ConfirmedProposalState,
                                       MonadPoll, PollModifier (..), PollVerFailure,
                                       ProposalState, USUndo, canCreateBlockBV, execPollT,
                                       execRollT, processGenesisBlock,
                                       recordBlockIssuance, rollbackUS, runDBPoll,
                                       runPollT, verifyAndApplyUSPayload, verifyBlockSize)
import           Pos.Util             (Color (Red), NE, NewestFirst, OldestFirst,
                                       colorize, inAssertMode)

type USGlobalApplyMode ssc m = ( WithLogger m
                               , DB.MonadDB ssc m
                               , SscHelpersClass ssc
                               , WithNodeContext ssc m)
type USGlobalVerifyMode ssc m = ( DB.MonadDB ssc m
                                , MonadError PollVerFailure m
                                , SscHelpersClass ssc
                                , WithNodeContext ssc m
                                , WithLogger m)

withUSLogger :: WithLogger m => m a -> m a
withUSLogger = modifyLoggerName (<> "us")

-- | Apply chain of /definitely/ valid blocks to US part of GState DB
-- and to US local data. This function assumes that no other thread
-- applies block in parallel. It also assumes that parent of oldest
-- block is current tip.  If verification is done prior to
-- application, one can pass 'PollModifier' obtained from verification
-- to this function.
usApplyBlocks
    :: (MonadThrow m, USGlobalApplyMode ssc m)
    => OldestFirst NE (Block ssc)
    -> Maybe PollModifier
    -> m [DB.SomeBatchOp]
usApplyBlocks blocks modifierMaybe = withUSLogger $
    case modifierMaybe of
        Nothing -> do
            verdict <- runExceptT $ usVerifyBlocks blocks
            either onFailure (return . modifierToBatch . fst) verdict
        Just modifier -> do
            -- TODO: I suppose such sanity checks should be done at higher
            -- level.
            inAssertMode $ do
                verdict <- runExceptT $ usVerifyBlocks blocks
                either onFailure (const pass) verdict
            return $ modifierToBatch modifier
  where
    onFailure failure = do
        let msg = "usVerifyBlocks failed in 'apply': " <> pretty failure
        logError $ colorize Red msg
        throwM $ USInternalError msg

-- | Revert application of given blocks to US part of GState DB and US local
-- data. The caller must ensure that the tip stored in DB is 'headerHash' of
-- head.
usRollbackBlocks
    :: forall ssc m.
       USGlobalApplyMode ssc m
    => NewestFirst NE (Block ssc, USUndo) -> m [DB.SomeBatchOp]
usRollbackBlocks blunds = withUSLogger $
    modifierToBatch <$>
    (runDBPoll . execPollT def $ mapM_ (rollbackUS . snd) blunds)

-- | Verify whether sequence of blocks can be applied to US part of
-- current GState DB.  This function doesn't make pure checks, they
-- are assumed to be done earlier, most likely during objects
-- construction.
usVerifyBlocks
    :: (USGlobalVerifyMode ssc m)
    => OldestFirst NE (Block ssc) -> m (PollModifier, OldestFirst NE USUndo)
usVerifyBlocks blocks = withUSLogger $ swap <$> run (mapM verifyBlock blocks)
  where
    run = runDBPoll . runPollT def

verifyBlock
    :: (USGlobalVerifyMode ssc m, MonadPoll m)
    => Block ssc -> m USUndo
verifyBlock (Left genBlk) =
    execRollT $ processGenesisBlock (genBlk ^. epochIndexL)
verifyBlock (Right blk) =
    execRollT $ do
        verifyAndApplyUSPayload
            True
            (Right $ blk ^. gbHeader)
            (blk ^. gbBody . mbUpdatePayload)
        -- Block issuance can't affect verification and application of US payload,
        -- so it's fine to separate it.
        -- Note, however, that it's important to do it after
        -- 'verifyAndApplyUSPayload', because there we assume that block
        -- version is confirmed.
        let leaderPk = blk ^. gbHeader . gbhConsensus . mcdLeaderKey
        recordBlockIssuance
            (addressHash leaderPk)
            (blk ^. gbHeader . gbhExtra . mehBlockVersion)
            (blk ^. blockSlot)
            (headerHash blk)
        -- Block size check doesn't interfere with other checks too, so
        -- it's separated.
        verifyBlockSize (headerHash blk) (blockSize blk)

-- | Checks whether our software can create block according to current
-- global state.
usCanCreateBlock
    :: (WithLogger m, WithNodeContext ssc m, DB.MonadDB ssc m)
    => m Bool
usCanCreateBlock =
    withUSLogger $ runDBPoll $ canCreateBlockBV lastKnownBlockVersion

----------------------------------------------------------------------------
-- Conversion to batch
----------------------------------------------------------------------------

modifierToBatch :: PollModifier -> [DB.SomeBatchOp]
modifierToBatch PollModifier {..} =
    concat $
    [ bvsModifierToBatch pmNewBVs pmDelBVs
    , lastAdoptedModifierToBatch pmAdoptedBVFull
    , confirmedVerModifierToBatch  pmNewConfirmed pmDelConfirmed
    , confirmedPropModifierToBatch pmNewConfirmedProps pmDelConfirmedProps
    , upModifierToBatch pmNewActiveProps pmDelActivePropsIdx
    ]

bvsModifierToBatch
    :: HashMap BlockVersion BlockVersionState
    -> HashSet BlockVersion
    -> [DB.SomeBatchOp]
bvsModifierToBatch (HM.toList -> added) (toList -> deleted) = addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . uncurry SetBVState) added
    delOps = map (DB.SomeBatchOp . DelBV) deleted

lastAdoptedModifierToBatch :: Maybe (BlockVersion, BlockVersionData) -> [DB.SomeBatchOp]
lastAdoptedModifierToBatch Nothing          = []
lastAdoptedModifierToBatch (Just (bv, bvd)) = [DB.SomeBatchOp $ SetAdopted bv bvd]

confirmedVerModifierToBatch
    :: HashMap ApplicationName NumSoftwareVersion
    -> HashSet ApplicationName
    -> [DB.SomeBatchOp]
confirmedVerModifierToBatch (HM.toList -> added) (toList -> deleted) =
    addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . ConfirmVersion . uncurry SoftwareVersion) added
    delOps = map (DB.SomeBatchOp . DelConfirmedVersion) deleted

confirmedPropModifierToBatch
    :: HashMap SoftwareVersion ConfirmedProposalState
    -> HashSet SoftwareVersion
    -> [DB.SomeBatchOp]
confirmedPropModifierToBatch (toList -> confAdded) (toList -> confDeleted) =
    confAddOps ++ confDelOps
  where
    confAddOps = map (DB.SomeBatchOp . AddConfirmedProposal) confAdded
    confDelOps = map (DB.SomeBatchOp . DelConfirmedProposal) confDeleted

upModifierToBatch :: HashMap UpId ProposalState
                  -> HashMap ApplicationName UpId
                  -> [DB.SomeBatchOp]
upModifierToBatch (toList -> added) (HM.toList -> deleted) = addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . PutProposal) added
    delOps = map (DB.SomeBatchOp . uncurry (flip DeleteProposal)) deleted
