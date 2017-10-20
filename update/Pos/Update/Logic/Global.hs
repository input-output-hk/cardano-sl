-- | Logic of local data processing in Update System.

module Pos.Update.Logic.Global
       ( usApplyBlocks
       , usCanCreateBlock
       , usRollbackBlocks
       , usVerifyBlocks
       ) where

import           Universum

import           Control.Monad.Except     (MonadError, runExceptT)
import           Data.Default             (Default (def))
import           System.Wlog              (WithLogger, modifyLoggerName)

import           Pos.Core                 (ApplicationName, BlockVersion,
                                           HasConfiguration, NumSoftwareVersion,
                                           SoftwareVersion (..), StakeholderId,
                                           addressHash, blockVersionL, epochIndexL,
                                           headerHashG, headerLeaderKeyL, headerSlotL)
import qualified Pos.DB.BatchOp           as DB
import qualified Pos.DB.Class             as DB
import           Pos.Exception            (reportFatalError)
import           Pos.Lrc.Context          (HasLrcContext)
import           Pos.Reporting            (MonadReporting)
import           Pos.Slotting             (MonadSlotsData, SlottingData, slottingVar)
import           Pos.Update.Configuration (HasUpdateConfiguration, lastKnownBlockVersion)
import           Pos.Update.Core          (BlockVersionData, UpId, UpdateBlock)
import           Pos.Update.DB            (UpdateOp (..))
import           Pos.Update.Poll          (BlockVersionState, ConfirmedProposalState,
                                           MonadPoll, PollModifier (..), PollVerFailure,
                                           ProposalState, USUndo, canCreateBlockBV,
                                           execPollT, execRollT, processGenesisBlock,
                                           recordBlockIssuance, reportUnexpectedError,
                                           rollbackUS, runDBPoll, runPollT,
                                           verifyAndApplyUSPayload)
import           Pos.Util.Chrono          (NE, NewestFirst, OldestFirst)
import qualified Pos.Util.Modifier        as MM
import           Pos.Util.Util            (inAssertMode)

type USGlobalVerifyMode ctx m =
    ( WithLogger m
    , MonadIO m
    , DB.MonadDBRead m
    , MonadReader ctx m
    , HasLrcContext ctx
    , HasConfiguration
    , HasUpdateConfiguration
    )

type USGlobalApplyMode ctx m =
    ( USGlobalVerifyMode ctx m
    , MonadSlotsData ctx m
    , MonadReporting ctx m
    )

withUSLogger :: WithLogger m => m a -> m a
withUSLogger = modifyLoggerName (<> "us")

-- | Apply chain of /definitely/ valid blocks to US part of GState DB
-- and to US local data. This function assumes that no other thread
-- applies block in parallel. It also assumes that parent of oldest
-- block is current tip.  If verification is done prior to
-- application, one can pass 'PollModifier' obtained from verification
-- to this function.
--
-- This function also updates in-memory state of slotting
-- ('MonadSlotsData') whenever it should be updated. Note that at this
-- point we can't be sure that blocks will be eventually applied to
-- DB. So it can happen that we update in-memory data, but don't
-- update DB. However, it's not a problem, because slotting data is
-- stable, i. e. once we know it for some epoch, we can be sure it
-- will never change. Also note that we store slotting data for all
-- epochs in memory, so adding new one can't make anything worse.
usApplyBlocks
    :: (MonadThrow m, USGlobalApplyMode ctx m)
    => OldestFirst NE UpdateBlock
    -> Maybe PollModifier
    -> m [DB.SomeBatchOp]
usApplyBlocks blocks modifierMaybe =
    withUSLogger $
    processModifier =<<
    case modifierMaybe of
        Nothing -> do
            verdict <- usVerifyBlocks False blocks
            either onFailure (return . fst) verdict
        Just modifier -> do
            -- TODO: I suppose such sanity checks should be done at higher
            -- level.
            inAssertMode $ do
                verdict <- usVerifyBlocks False blocks
                whenLeft verdict $ \v -> onFailure v
            return modifier
  where
    onFailure failure = do
        let msg = "usVerifyBlocks failed in 'apply': " <> pretty failure
        reportFatalError msg

-- | Revert application of given blocks to US part of GState DB and US local
-- data. The caller must ensure that the tip stored in DB is 'headerHash' of
-- head.
usRollbackBlocks
    :: forall ctx m.
       USGlobalApplyMode ctx m
    => NewestFirst NE (UpdateBlock, USUndo) -> m [DB.SomeBatchOp]
usRollbackBlocks blunds =
    withUSLogger $
    processModifier =<<
    (runDBPoll . execPollT def $ mapM_ (rollbackUS . snd) blunds)

-- This function takes a 'PollModifier' corresponding to a sequence of
-- blocks, updates in-memory slotting data and converts this modifier
-- to '[SomeBatchOp]'.
processModifier ::
       forall ctx m. (HasConfiguration, MonadSlotsData ctx m)
    => PollModifier
    -> m [DB.SomeBatchOp]
processModifier pm@PollModifier {pmSlottingData = newSlottingData} =
    modifierToBatch pm <$ whenJust newSlottingData setNewSlottingData
  where
    setNewSlottingData newSD = do
        var <- view slottingVar
        atomically $ writeTVar var newSD

-- | Verify whether sequence of blocks can be applied to US part of
-- current GState DB.  This function doesn't make pure checks, they
-- are assumed to be done earlier, most likely during objects
-- construction.
--
-- If the first argument is 'True' it means that all data must be
-- known. Currently it only means that 'UpdateProposal's must have
-- only known attributes, but I can't guarantee this comment will
-- always be up-to-date.
usVerifyBlocks
    :: (USGlobalVerifyMode ctx m, MonadReporting ctx m)
    => Bool
    -> OldestFirst NE UpdateBlock
    -> m (Either PollVerFailure (PollModifier, OldestFirst NE USUndo))
usVerifyBlocks verifyAllIsKnown blocks =
    withUSLogger $
    reportUnexpectedError $
    runExceptT (swap <$> run (mapM (verifyBlock verifyAllIsKnown) blocks))
  where
    run = runDBPoll . runPollT def

verifyBlock
    :: (USGlobalVerifyMode ctx m, MonadPoll m, MonadError PollVerFailure m)
    => Bool -> UpdateBlock -> m USUndo
verifyBlock _ (Left genBlk) =
    execRollT $ processGenesisBlock (genBlk ^. epochIndexL)
verifyBlock verifyAllIsKnown (Right (header, payload)) =
    execRollT $ do
        verifyAndApplyUSPayload
            verifyAllIsKnown
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
usCanCreateBlock ::
       ( WithLogger m
       , MonadIO m
       , DB.MonadDBRead m
       , MonadReader ctx m
       , HasLrcContext ctx
       , HasConfiguration
       , HasUpdateConfiguration
       )
    => m Bool
usCanCreateBlock =
    withUSLogger $ runDBPoll $ canCreateBlockBV lastKnownBlockVersion

----------------------------------------------------------------------------
-- Conversion to batch
----------------------------------------------------------------------------

modifierToBatch :: HasConfiguration => PollModifier -> [DB.SomeBatchOp]
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
    , upModifierToBatch
          (MM.insertions pmActiveProps)
          (MM.deletions pmActiveProps)
    , sdModifierToBatch pmSlottingData
    , epModifierToBatch pmEpochProposers
    ]

bvsModifierToBatch
    :: HasConfiguration
    => [(BlockVersion, BlockVersionState)]
    -> [BlockVersion]
    -> [DB.SomeBatchOp]
bvsModifierToBatch added deleted = addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . uncurry SetBVState) added
    delOps = map (DB.SomeBatchOp . DelBV) deleted

lastAdoptedModifierToBatch :: HasConfiguration => Maybe (BlockVersion, BlockVersionData) -> [DB.SomeBatchOp]
lastAdoptedModifierToBatch Nothing          = []
lastAdoptedModifierToBatch (Just (bv, bvd)) = [DB.SomeBatchOp $ SetAdopted bv bvd]

confirmedVerModifierToBatch
    :: HasConfiguration
    => [(ApplicationName, NumSoftwareVersion)]
    -> [ApplicationName]
    -> [DB.SomeBatchOp]
confirmedVerModifierToBatch added deleted =
    addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . ConfirmVersion . uncurry SoftwareVersion) added
    delOps = map (DB.SomeBatchOp . DelConfirmedVersion) deleted

confirmedPropModifierToBatch
    :: HasConfiguration
    => [(SoftwareVersion, ConfirmedProposalState)]
    -> [SoftwareVersion]
    -> [DB.SomeBatchOp]
confirmedPropModifierToBatch (map snd -> confAdded) confDeleted =
    confAddOps ++ confDelOps
  where
    confAddOps = map (DB.SomeBatchOp . AddConfirmedProposal) confAdded
    confDelOps = map (DB.SomeBatchOp . DelConfirmedProposal) confDeleted

upModifierToBatch
    :: HasConfiguration
    => [(UpId, ProposalState)]
    -> [UpId]
    -> [DB.SomeBatchOp]
upModifierToBatch (map snd -> added) deleted
      = addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . PutProposal) added
    delOps = map (DB.SomeBatchOp . DeleteProposal) deleted

sdModifierToBatch :: HasConfiguration => Maybe SlottingData -> [DB.SomeBatchOp]
sdModifierToBatch Nothing   = []
sdModifierToBatch (Just sd) = [DB.SomeBatchOp $ PutSlottingData sd]

epModifierToBatch :: HasConfiguration => Maybe (HashSet StakeholderId) -> [DB.SomeBatchOp]
epModifierToBatch Nothing   = []
epModifierToBatch (Just ep) = [DB.SomeBatchOp $ PutEpochProposers ep]
