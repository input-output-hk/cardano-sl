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
import           System.Wlog          (WithLogger, logError)
import           Universum

import           Pos.Constants        (lastKnownBlockVersion)
import           Pos.Context          (WithNodeContext)
import           Pos.Crypto           (ProxySignature (pdDelegatePk))
import qualified Pos.DB               as DB
import           Pos.DB.GState        (UpdateOp (..))
import           Pos.Ssc.Class        (Ssc)
import           Pos.Types            (ApplicationName, Block, BlockSignature (..),
                                       BlockVersion, NumSoftwareVersion,
                                       SoftwareVersion (..), addressHash, blockSlot,
                                       epochIndexL, gbBody, gbHeader, gbhConsensus,
                                       gbhExtra, headerHash, mbUpdatePayload,
                                       mcdLeaderKey, mcdSignature, mehBlockVersion)
import           Pos.Update.Core      (UpId)
import           Pos.Update.Error     (USError (USInternalError))
import           Pos.Update.Poll      (BlockVersionState, ConfirmedProposalState,
                                       MonadPoll, PollModifier (..), PollVerFailure,
                                       ProposalState, USUndo, canCreateBlockBV, execPollT,
                                       execRollT, processGenesisBlock,
                                       recordBlockIssuance, rollbackUS, runDBPoll,
                                       runPollT, verifyAndApplyUSPayload)
import           Pos.Util             (Color (Red), NE, NewestFirst, OldestFirst,
                                       colorize, inAssertMode)

type USGlobalApplyMode ssc m = (WithLogger m
                               , DB.MonadDB ssc m
                               , Ssc ssc
                               , WithNodeContext ssc m)
type USGlobalVerifyMode ssc m = (DB.MonadDB ssc m
                                , MonadError PollVerFailure m
                                , Ssc ssc
                                , WithNodeContext ssc m)

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
usApplyBlocks blocks modifierMaybe =
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
usRollbackBlocks blunds =
    modifierToBatch <$>
    (runDBPoll . execPollT def $ mapM_ (rollbackUS . snd) blunds)

-- | Verify whether sequence of blocks can be applied to US part of
-- current GState DB.  This function doesn't make pure checks, they
-- are assumed to be done earlier, most likely during objects
-- construction.
usVerifyBlocks
    :: (USGlobalVerifyMode ssc m)
    => OldestFirst NE (Block ssc) -> m (PollModifier, OldestFirst NE USUndo)
usVerifyBlocks blocks = swap <$> run (mapM verifyBlock blocks)
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
        let consensusData = blk ^. gbHeader . gbhConsensus
        let issuerPk =
                case consensusData ^. mcdSignature of
                    BlockSignature _         -> consensusData ^. mcdLeaderKey
                    BlockPSignatureEpoch ps  -> pdDelegatePk ps
                    BlockPSignatureSimple ps -> pdDelegatePk ps
        recordBlockIssuance
            (addressHash issuerPk)
            (blk ^. gbHeader . gbhExtra . mehBlockVersion)
            (blk ^. blockSlot)
            (headerHash blk)

-- | Checks whether our software can create block according to current
-- global state.
usCanCreateBlock :: (WithNodeContext ssc m, DB.MonadDB ssc m) => m Bool
usCanCreateBlock = runDBPoll $ canCreateBlockBV lastKnownBlockVersion

----------------------------------------------------------------------------
-- Conversion to batch
----------------------------------------------------------------------------

modifierToBatch :: PollModifier -> [DB.SomeBatchOp]
modifierToBatch PollModifier {..} =
    concat $
    [ bvsModifierToBatch pmNewBVs pmDelBVs
    , lastAdoptedModifierToBatch pmLastAdoptedBV
    , confirmedModifierToBatch pmNewConfirmed pmDelConfirmed pmNewConfirmedProps
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

lastAdoptedModifierToBatch :: Maybe BlockVersion -> [DB.SomeBatchOp]
lastAdoptedModifierToBatch Nothing  = []
lastAdoptedModifierToBatch (Just v) = [DB.SomeBatchOp $ SetLastAdopted v]

confirmedModifierToBatch :: HashMap ApplicationName NumSoftwareVersion
                         -> HashSet ApplicationName
                         -> HashMap SoftwareVersion ConfirmedProposalState
                         -> [DB.SomeBatchOp]
confirmedModifierToBatch
    (HM.toList -> added)
    (toList -> deleted)
    (toList -> confAdded) = addOps ++ delOps ++ confAddOps
  where
    addOps = map (DB.SomeBatchOp . ConfirmVersion . uncurry SoftwareVersion) added
    delOps = map (DB.SomeBatchOp . DelConfirmedVersion) deleted
    confAddOps = map (DB.SomeBatchOp . AddConfirmedProposal) confAdded

upModifierToBatch :: HashMap UpId ProposalState
                  -> HashMap ApplicationName UpId
                  -> [DB.SomeBatchOp]
upModifierToBatch (toList -> added) (HM.toList -> deleted) = addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . PutProposal) added
    delOps = map (DB.SomeBatchOp . uncurry (flip DeleteProposal)) deleted
