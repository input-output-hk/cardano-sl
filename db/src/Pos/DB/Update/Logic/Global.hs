-- | Logic of local data processing in Update System.
{-# LANGUAGE NoMonomorphismRestriction #-}

module Pos.DB.Update.Logic.Global
       ( UpdateBlock

       , usApplyBlocks
       , usCanCreateBlock
       , usRollbackBlocks
       , usVerifyBlocks
       ) where

import           Universum

import           Control.Monad.Except (MonadError, runExceptT)
import           Data.Default (Default (def))
import           UnliftIO (MonadUnliftIO)

import           Pos.Chain.Block (ComponentBlock (..), headerHashG,
                     headerLeaderKeyL, headerSlotL)
import           Pos.Chain.Update (BlockVersionState, ConfirmedProposalState,
                     HasUpdateConfiguration, MonadPoll, PollModifier (..),
                     PollT, PollVerFailure, ProposalState, USUndo, execPollT,
                     execRollT, getAdoptedBV, lastKnownBlockVersion,
                     reportUnexpectedError, runPollT)
import           Pos.Core (HasCoreConfiguration, HasProtocolConstants,
                     ProtocolMagic, StakeholderId, addressHash, epochIndexL)
import           Pos.Core.Chrono (NE, NewestFirst, OldestFirst)
import           Pos.Core.Exception (traceFatalError)
import           Pos.Core.Reporting (MonadReporting)
import           Pos.Core.Slotting (MonadSlotsData, SlottingData, slottingVar)
import           Pos.Core.Update (ApplicationName, BlockVersion,
                     BlockVersionData, NumSoftwareVersion,
                     SoftwareVersion (..), UpId, UpdatePayload, blockVersionL)
import qualified Pos.DB.BatchOp as DB
import qualified Pos.DB.Class as DB
import           Pos.DB.Lrc (HasLrcContext)
import           Pos.DB.Update.GState (UpdateOp (..))
import           Pos.DB.Update.Poll.DBPoll (DBPoll, runDBPoll)
import           Pos.DB.Update.Poll.Logic.Apply (verifyAndApplyUSPayload)
import           Pos.DB.Update.Poll.Logic.Base (canCreateBlockBV)
import           Pos.DB.Update.Poll.Logic.Rollback (rollbackUS)
import           Pos.DB.Update.Poll.Logic.Softfork (processGenesisBlock,
                     recordBlockIssuance)
import           Pos.Util.AssertMode (inAssertMode)
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Trace.Named (TraceNamed, appendName, natTrace)


----------------------------------------------------------------------------
-- UpdateBlock
----------------------------------------------------------------------------

type UpdateBlock = ComponentBlock UpdatePayload

----------------------------------------------------------------------------
-- Constraints
----------------------------------------------------------------------------

type USGlobalVerifyMode ctx m =
    ( MonadIO m
    , MonadReader ctx m
    , HasLrcContext ctx
    , HasUpdateConfiguration
    )

type USGlobalApplyMode ctx m =
    ( USGlobalVerifyMode ctx m
    , DB.MonadDBRead m
    , MonadUnliftIO m
    , MonadSlotsData ctx m
    , MonadReporting m
    )

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

withUSLogger :: TraceNamed m -> TraceNamed m
withUSLogger = appendName "us"

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
    :: forall ctx m. (MonadThrow m, USGlobalApplyMode ctx m)
    => TraceNamed IO
    -> ProtocolMagic
    -> BlockVersion
    -> OldestFirst NE UpdateBlock
    -> Maybe PollModifier
    -> m [DB.SomeBatchOp]
usApplyBlocks logTrace0 pm bv blocks modifierMaybe =
    processModifier =<<
    case modifierMaybe of
        Nothing -> do
            verdict <- usVerifyBlocks logTrace pm False bv blocks
            either onFailure (return . fst) verdict
        Just modifier -> do
            -- TODO: I suppose such sanity checks should be done at higher
            -- level.
            inAssertMode $ do
                verdict <- usVerifyBlocks logTrace pm False bv blocks
                whenLeft verdict $ \v -> onFailure v
            return modifier
  where
    logTrace = withUSLogger logTrace0
    onFailure failure = do
        let msg = "usVerifyBlocks failed in 'apply': " <> pretty failure
        traceFatalError (natTrace liftIO logTrace) msg

-- | Revert application of given blocks to US part of GState DB and US local
-- data. The caller must ensure that the tip stored in DB is 'headerHash' of
-- head.
usRollbackBlocks
    :: (USGlobalApplyMode ctx m)
    => TraceNamed IO
    -> NewestFirst NE (UpdateBlock, USUndo)
    -> m [DB.SomeBatchOp]
usRollbackBlocks logTrace0 blunds =
    processModifier =<<
        (runDBPoll . execPollT def $ mapM_ ((rollbackUS logTrace) . snd) blunds)
  where
    logTrace = withUSLogger logTrace0

-- This function takes a 'PollModifier' corresponding to a sequence of
-- blocks, updates in-memory slotting data and converts this modifier
-- to '[SomeBatchOp]'.
processModifier ::
       forall ctx m. (MonadSlotsData ctx m, HasCoreConfiguration)
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
--
-- All blocks must be from the same epoch.
usVerifyBlocks ::
       ( USGlobalVerifyMode ctx m
       , DB.MonadDBRead m
       , MonadUnliftIO m
       , MonadReporting m
       )
    => TraceNamed IO
    -> ProtocolMagic
    -> Bool
    -> BlockVersion
    -> OldestFirst NE UpdateBlock
    -> m (Either PollVerFailure (PollModifier, OldestFirst NE USUndo))
usVerifyBlocks logTrace0 pm verifyAllIsKnown adoptedBV blocks =
    reportUnexpectedError $
        processRes <$> run (runExceptT action)
  where
    logTrace = withUSLogger logTrace0
    action = do
        mapM (verifyBlock logTrace pm adoptedBV verifyAllIsKnown) blocks
    run :: PollT (DBPoll n) a -> n (a, PollModifier)
    run = runDBPoll . runPollT def
    processRes ::
           (Either PollVerFailure (OldestFirst NE USUndo), PollModifier)
        -> Either PollVerFailure (PollModifier, OldestFirst NE USUndo)
    processRes (Left failure, _)       = Left failure
    processRes (Right undos, modifier) = Right (modifier, undos)

verifyBlock
    :: (USGlobalVerifyMode ctx m, MonadPoll m, MonadError PollVerFailure m, HasProtocolConstants)
    => TraceNamed IO -> ProtocolMagic -> BlockVersion -> Bool -> UpdateBlock -> m USUndo
verifyBlock logTrace _ _ _ (ComponentBlockGenesis genBlk) =
    execRollT $ processGenesisBlock logTrace (genBlk ^. epochIndexL)
verifyBlock logTrace pm lastAdopted verifyAllIsKnown (ComponentBlockMain header payload) =
    execRollT $ do
        verifyAndApplyUSPayload
            logTrace
            pm
            lastAdopted
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
       ( MonadUnliftIO m
       , DB.MonadDBRead m
       , MonadReader ctx m
       , HasLrcContext ctx
       , HasUpdateConfiguration
       )
    => m Bool
usCanCreateBlock =
    runDBPoll $ do
        lastAdopted <- getAdoptedBV
        canCreateBlockBV lastAdopted lastKnownBlockVersion

----------------------------------------------------------------------------
-- Conversion to batch
----------------------------------------------------------------------------

modifierToBatch :: HasCoreConfiguration => PollModifier -> [DB.SomeBatchOp]
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
    :: HasCoreConfiguration
    => [(BlockVersion, BlockVersionState)]
    -> [BlockVersion]
    -> [DB.SomeBatchOp]
bvsModifierToBatch added deleted = addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . uncurry SetBVState) added
    delOps = map (DB.SomeBatchOp . DelBV) deleted

lastAdoptedModifierToBatch :: HasCoreConfiguration => Maybe (BlockVersion, BlockVersionData) -> [DB.SomeBatchOp]
lastAdoptedModifierToBatch Nothing          = []
lastAdoptedModifierToBatch (Just (bv, bvd)) = [DB.SomeBatchOp $ SetAdopted bv bvd]

confirmedVerModifierToBatch
    :: HasCoreConfiguration
    => [(ApplicationName, NumSoftwareVersion)]
    -> [ApplicationName]
    -> [DB.SomeBatchOp]
confirmedVerModifierToBatch added deleted =
    addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . ConfirmVersion . uncurry SoftwareVersion) added
    delOps = map (DB.SomeBatchOp . DelConfirmedVersion) deleted

confirmedPropModifierToBatch
    :: HasCoreConfiguration
    => [(SoftwareVersion, ConfirmedProposalState)]
    -> [SoftwareVersion]
    -> [DB.SomeBatchOp]
confirmedPropModifierToBatch (map snd -> confAdded) confDeleted =
    confAddOps ++ confDelOps
  where
    confAddOps = map (DB.SomeBatchOp . AddConfirmedProposal) confAdded
    confDelOps = map (DB.SomeBatchOp . DelConfirmedProposal) confDeleted

upModifierToBatch
    :: HasCoreConfiguration
    => [(UpId, ProposalState)]
    -> [UpId]
    -> [DB.SomeBatchOp]
upModifierToBatch (map snd -> added) deleted
      = addOps ++ delOps
  where
    addOps = map (DB.SomeBatchOp . PutProposal) added
    delOps = map (DB.SomeBatchOp . DeleteProposal) deleted

sdModifierToBatch :: HasCoreConfiguration => Maybe SlottingData -> [DB.SomeBatchOp]
sdModifierToBatch Nothing   = []
sdModifierToBatch (Just sd) = [DB.SomeBatchOp $ PutSlottingData sd]

epModifierToBatch :: HasCoreConfiguration => Maybe (HashSet StakeholderId) -> [DB.SomeBatchOp]
epModifierToBatch Nothing   = []
epModifierToBatch (Just ep) = [DB.SomeBatchOp $ PutEpochProposers ep]
