{-# LANGUAGE ConstraintKinds     #-}
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
import           Serokell.Util        (Color (Red), colorize)
import           System.Wlog          (WithLogger, logError, modifyLoggerName)
import           Universum

import           Pos.Constants        (lastKnownBlockVersion)
import           Pos.Core             (ApplicationName, BlockVersion, NumSoftwareVersion,
                                       SoftwareVersion (..), addressHash, blockVersionL,
                                       epochIndexL, headerHashG, headerLeaderKeyL,
                                       headerSlotL)
import qualified Pos.DB               as DB
import           Pos.Lrc.Context      (LrcContext)
import           Pos.Slotting         (SlottingData)
import           Pos.Update.Core      (BlockVersionData, UpId, UpdateBlock)
import           Pos.Update.DB        (UpdateOp (..))
import           Pos.Update.Error     (USError (USInternalError))
import           Pos.Update.Poll      (BlockVersionState, ConfirmedProposalState,
                                       MonadPoll, PollModifier (..), PollVerFailure,
                                       ProposalState, USUndo, canCreateBlockBV, execPollT,
                                       execRollT, processGenesisBlock,
                                       recordBlockIssuance, rollbackUS, runDBPoll,
                                       runPollT, verifyAndApplyUSPayload)
import           Pos.Util             (inAssertMode)
import           Pos.Util.Chrono      (NE, NewestFirst, OldestFirst)
import           Pos.Util.Context     (HasContext)
import qualified Pos.Util.Modifier    as MM

type USGlobalApplyMode m = ( WithLogger m
                           , DB.MonadDB m
                           , HasContext LrcContext m
                           )
type USGlobalVerifyMode m = ( WithLogger m
                            , DB.MonadDB m
                            , HasContext LrcContext m
                            , MonadError PollVerFailure m
                            )

withUSLogger :: WithLogger m => m a -> m a
withUSLogger = modifyLoggerName (<> "us")

-- | Apply chain of /definitely/ valid blocks to US part of GState DB
-- and to US local data. This function assumes that no other thread
-- applies block in parallel. It also assumes that parent of oldest
-- block is current tip.  If verification is done prior to
-- application, one can pass 'PollModifier' obtained from verification
-- to this function.
usApplyBlocks
    :: (MonadThrow m, USGlobalApplyMode m)
    => OldestFirst NE UpdateBlock
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
    :: forall m.
       USGlobalApplyMode m
    => NewestFirst NE (UpdateBlock, USUndo) -> m [DB.SomeBatchOp]
usRollbackBlocks blunds = withUSLogger $
    modifierToBatch <$>
    (runDBPoll . execPollT def $ mapM_ (rollbackUS . snd) blunds)

-- | Verify whether sequence of blocks can be applied to US part of
-- current GState DB.  This function doesn't make pure checks, they
-- are assumed to be done earlier, most likely during objects
-- construction.
usVerifyBlocks
    :: (USGlobalVerifyMode m)
    => OldestFirst NE UpdateBlock -> m (PollModifier, OldestFirst NE USUndo)
usVerifyBlocks blocks = withUSLogger $ swap <$> run (mapM verifyBlock blocks)
  where
    run = runDBPoll . runPollT def

verifyBlock
    :: (USGlobalVerifyMode m, MonadPoll m)
    => UpdateBlock -> m USUndo
verifyBlock (Left genBlk) =
    execRollT $ processGenesisBlock (genBlk ^. epochIndexL)
verifyBlock (Right (header, payload)) =
    execRollT $ do
        verifyAndApplyUSPayload
            True
            (Right header)
            payload
        -- Block issuance can't affect verification and application of US
        -- payload, so it's fine to separate it. Note, however, that it's
        -- important to do it after 'verifyAndApplyUSPayload', because there
        -- we assume that block version is confirmed.
        let leaderPk = header ^. headerLeaderKeyL
        recordBlockIssuance
            (addressHash leaderPk)
            (header ^. blockVersionL)
            (header ^. headerSlotL)
            (header ^. headerHashG)

-- | Checks whether our software can create block according to current
-- global state.
usCanCreateBlock
    :: (WithLogger m, DB.MonadDB m, HasContext LrcContext m)
    => m Bool
usCanCreateBlock =
    withUSLogger $ runDBPoll $ canCreateBlockBV lastKnownBlockVersion

----------------------------------------------------------------------------
-- Conversion to batch
----------------------------------------------------------------------------

modifierToBatch :: PollModifier -> [DB.SomeBatchOp]
modifierToBatch PollModifier {..} =
    concat $
    [ bvsModifierToBatch (MM.insertions pmBVs) (MM.deletions pmBVs)
    , lastAdoptedModifierToBatch pmAdoptedBVFull
    , confirmedVerModifierToBatch
          (MM.insertions pmConfirmed)
          (MM.deletions pmConfirmed)
    , confirmedPropModifierToBatch
          (MM.insertions pmConfirmedProps)
          (MM.deletions pmConfirmedProps)
    , upModifierToBatch (MM.insertions pmActiveProps) pmDelActivePropsIdx
    , sdModifierToBatch pmSlottingData
    ]

bvsModifierToBatch
    :: [(BlockVersion, BlockVersionState)]
    -> [BlockVersion]
    -> [DB.SomeBatchOp]
bvsModifierToBatch added deleted = addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . uncurry SetBVState) added
    delOps = map (DB.SomeBatchOp . DelBV) deleted

lastAdoptedModifierToBatch :: Maybe (BlockVersion, BlockVersionData) -> [DB.SomeBatchOp]
lastAdoptedModifierToBatch Nothing          = []
lastAdoptedModifierToBatch (Just (bv, bvd)) = [DB.SomeBatchOp $ SetAdopted bv bvd]

confirmedVerModifierToBatch
    :: [(ApplicationName, NumSoftwareVersion)]
    -> [ApplicationName]
    -> [DB.SomeBatchOp]
confirmedVerModifierToBatch added deleted =
    addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . ConfirmVersion . uncurry SoftwareVersion) added
    delOps = map (DB.SomeBatchOp . DelConfirmedVersion) deleted

confirmedPropModifierToBatch
    :: [(SoftwareVersion, ConfirmedProposalState)]
    -> [SoftwareVersion]
    -> [DB.SomeBatchOp]
confirmedPropModifierToBatch (map snd -> confAdded) confDeleted =
    confAddOps ++ confDelOps
  where
    confAddOps = map (DB.SomeBatchOp . AddConfirmedProposal) confAdded
    confDelOps = map (DB.SomeBatchOp . DelConfirmedProposal) confDeleted

upModifierToBatch :: [(UpId, ProposalState)]
                  -> HashMap ApplicationName (HashSet UpId)
                  -> [DB.SomeBatchOp]
upModifierToBatch (map snd -> added) (HM.toList -> deleted)
      = addOps ++ delOps
  where
    deepToList = concatMap (\(x, y) -> zip (repeat x) (toList y))
    addOps = map (DB.SomeBatchOp . PutProposal) added
    delOps = map (DB.SomeBatchOp . uncurry (flip DeleteProposal)) (deepToList deleted)

sdModifierToBatch :: Maybe SlottingData -> [DB.SomeBatchOp]
sdModifierToBatch Nothing   = []
sdModifierToBatch (Just sd) = [DB.SomeBatchOp $ PutSlottingData sd]
