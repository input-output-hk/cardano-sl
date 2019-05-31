{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This module does some hard work related to block processing
-- logic, but not specific to any particular part of the
-- system. On the contrary, modules like 'Pos.Chain.Block.VAR'
-- unite logic from multiple components into functions.

module Pos.DB.Block.Slog.Logic
       ( mustDataBeKnown

       , MonadSlogBase
       , MonadSlogVerify
       , slogVerifyBlocks

       , MonadSlogApply
       , slogApplyBlocks
       , slogRollbackBlocks

       , BypassSecurityCheck(..)
       , ShouldCallBListener (..)
       ) where

import           Universum

import           Control.Lens (_Wrapped)
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set (fromList)
import           Formatting
import           Serokell.Util (Color (Red), colorize)
import           Serokell.Util.Verify (formatAllErrors, verResToMonadError)

import           Pos.Chain.Block (Block, Blund, ConsensusEraLeaders (..),
                     HasSlogGState, LastBlkSlots, LastSlotInfo (..),
                     SlogUndo (..), blockLastSlotInfo, genBlockLeaders,
                     headerHash, headerHashG, prevBlockL, verifyBlocks)
import qualified Pos.Chain.Block.Slog.LastBlkSlots as LastBlkSlots
import           Pos.Chain.Genesis as Genesis (Config (..),
                     configBlkSecurityParam, configEpochSlots,
                     configGenesisWStakeholders)
import           Pos.Chain.Txp (mkLiveTxValidationRules)
import           Pos.Chain.Update (BlockVersion (..), ConsensusEra (..),
                     ObftConsensusStrictness (..), UpdateConfiguration,
                     lastKnownBlockVersion)
import           Pos.Core (BlockCount, difficultyL, epochIndexL,
                     epochOrSlotToEpochIndex, kEpochSlots, pcBlkSecurityParam)
import           Pos.Core.Chrono (NE, NewestFirst (getNewestFirst),
                     OldestFirst (..), toOldestFirst, _OldestFirst)
import           Pos.Core.Exception (assertionFailed, reportFatalError)
import           Pos.Core.NetworkMagic (NetworkMagic (..), makeNetworkMagic)
import           Pos.Core.Slotting (HasEpochIndex, MonadSlots, SlotId (..),
                     getEpochOrSlot)
import           Pos.DB (SomeBatchOp (..))
import           Pos.DB.Block.BListener (MonadBListener (..))
import qualified Pos.DB.Block.GState.BlockExtra as GS
import           Pos.DB.Block.Load (putBlunds)
import           Pos.DB.Block.Slog.Context (slogGetLastBlkSlots,
                     slogPutLastSlots)
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDB (..), MonadDBRead)
import qualified Pos.DB.GState.Common as GS
                     (CommonOp (PutMaxSeenDifficulty, PutTip),
                     getMaxSeenDifficulty)
import           Pos.DB.Lrc (HasLrcContext, lrcActionOnEpochReason)
import qualified Pos.DB.Lrc as LrcDB
import           Pos.DB.Lrc.OBFT (getEpochSlotLeaderScheduleObft)
import           Pos.DB.Update (getAdoptedBVFull, getConsensusEra)
import           Pos.Util (_neHead, _neLast)
import           Pos.Util.AssertMode (inAssertMode)
import           Pos.Util.Util (HasLens', lensOf)
import           Pos.Util.Wlog (WithLogger, logInfo)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Check whether data from block must be fully known to our application.
--
-- We verify that data in blocks is known if protocol version used
-- by this software is greater than or equal to the adopted
-- version. That's because in this case:
-- 1. Authors of this software are aware of adopted version.
-- 2. Each issued block must be formed with respect to adopted version.
--
-- Comparison is quite tricky here. Table below demonstrates it.
--
--   Our | Adopted | Check?
-- ————————————————————————
-- 1.2.3 |  1.2.3  | Yes
-- 1.2.3 |  1.2.4  | No
-- 1.2.3 |  1.2.2  | No
-- 1.2.3 |  1.3.2  | No
-- 1.2.3 |  1.1.1  | Yes
-- 2.2.8 |  1.9.9  | Yes
--
-- If `(major, minor)` of our version is greater than of adopted
-- one, then check is certainly done. If it's equal, then check is
-- done only if `alt` component is the same as adopted one. In
-- other cases (i. e. when our `(major, minor)` is less than from
-- adopted version) check is not done.
mustDataBeKnown :: UpdateConfiguration -> BlockVersion -> Bool
mustDataBeKnown uc adoptedBV =
    lastKnownMajMin > adoptedMajMin || lastKnownBlockVersion uc == adoptedBV
  where
    toMajMin BlockVersion {..} = (bvMajor, bvMinor)
    lastKnownMajMin = toMajMin (lastKnownBlockVersion uc)
    adoptedMajMin = toMajMin adoptedBV

----------------------------------------------------------------------------
-- VAR
----------------------------------------------------------------------------

-- | Set of basic constraints needed by Slog.
type MonadSlogBase ctx m =
    ( MonadSlots ctx m
    , MonadIO m
    , MonadDBRead m
    , WithLogger m
    , HasLens' ctx UpdateConfiguration
    )

-- | Set of constraints needed for Slog verification.
type MonadSlogVerify ctx m =
    ( MonadSlogBase ctx m
    , MonadReader ctx m
    , HasLrcContext ctx
    )

-- | Verify everything from block that is not checked by other components.
-- All blocks must be from the same epoch.
--
-- The algorithm works as follows:
--
-- 1.  If the oldest block is a genesis block, verify that its leaders
--     match the ones computed by LRC.
-- 2.  Call pure verification. If it fails, throw.
-- 3.  Compute 'SlogUndo's and return them.
slogVerifyBlocks
    :: (MonadSlogVerify ctx m, HasSlogGState ctx)
    => Genesis.Config
    -> Maybe SlotId -- ^ current slot
    -> OldestFirst NE Block
    -> m (Either Text (OldestFirst NE SlogUndo))
slogVerifyBlocks genesisConfig curSlot blocks = runExceptT $ do
    uc <- view (lensOf @UpdateConfiguration)
    era <- getConsensusEra
    logInfo $ sformat ("slogVerifyBlocks: Consensus era is " % shown) era
    (adoptedBV, adoptedBVD) <- lift getAdoptedBVFull
    let dataMustBeKnown = mustDataBeKnown uc adoptedBV


    lastBlkSlots <- slogGetLastBlkSlots

    leaders <- case era of
        Original ->
            let headEpoch = blocks ^. _Wrapped . _neHead . epochIndexL
            in lift $
                OriginalLeaders <$> lrcActionOnEpochReason
                    headEpoch
                    (sformat
                        ("slogVerifyBlocks Original: there are no leaders for epoch " %build)
                        headEpoch)
                    LrcDB.getLeadersForEpoch
        OBFT ObftStrict -> do
            initialSlot <- case curSlot of
                                Just cs -> pure cs
                                Nothing -> throwError "slogVerifyBlocks ObftStrict: curSlot set to Nothing - \
                                            \this occurs in EBBs which should not appear"
            pure $ ObftStrictLeaders $
                getEpochSlotLeaderScheduleObft genesisConfig
                                               (siEpoch initialSlot)
        OBFT ObftLenient -> do
            -- The lenient OBFT block validation algorithm only requires a
            -- collection of "acceptable" slot leaders rather than a slot
            -- leader schedule.
            let gStakeholders = Genesis.configGenesisWStakeholders genesisConfig
            pure $
                ObftLenientLeaders (Set.fromList gStakeholders)
                                   (configBlkSecurityParam genesisConfig)
                                   lastBlkSlots


    -- This is pretty much equivalent to performing a case on `era` since the
    -- `leaders` were evaluated above based on the `era`.
    case leaders of
        OriginalLeaders ls ->
            -- We take head here, because blocks are in oldest first order and
            -- we know that all of them are from the same epoch. So if there
            -- is a genesis block, it must be head and only head.
            case blocks ^. _OldestFirst . _neHead of
                (Left block) ->
                    when (block ^. genBlockLeaders /= ls) $
                    throwError "Genesis block leaders don't match with LRC-computed"
                _ -> pass
        ObftStrictLeaders _ -> pass
        ObftLenientLeaders {} -> pass

    -- Do pure block verification.
    currentEpoch <- epochOrSlotToEpochIndex . getEpochOrSlot <$> DB.getTipHeader
    let txValRulesConfig = configTxValRules $ genesisConfig
        txValRules = mkLiveTxValidationRules currentEpoch txValRulesConfig
        blocksList :: OldestFirst [] Block
        blocksList = OldestFirst (NE.toList (getOldestFirst blocks))
    verResToMonadError formatAllErrors $
        verifyBlocks
            genesisConfig
            era
            txValRules
            curSlot
            dataMustBeKnown
            adoptedBVD
            leaders
            blocksList

    -- Here we need to compute 'SlogUndo'. When we apply a block,
    -- we can remove one of the last slots stored in 'BlockExtra'.
    -- This removed slot must be put into 'SlogUndo'.

    -- these slots will be added if we apply all blocks
    let newSlots :: [LastSlotInfo]
        newSlots =
            mapMaybe (blockLastSlotInfo (configEpochSlots genesisConfig)) $ toList blocks

    let removedSlots = snd $ LastBlkSlots.updateManyR lastBlkSlots (OldestFirst newSlots)

    -- Note: here we exploit the fact that genesis block can be only 'head'.
    -- If we have genesis block, then size of 'newSlots' will be less than
    -- number of blocks we verify. It means that there will definitely
    -- be 'Nothing' in the head of the result.
    --
    -- It also works fine if we store less than 'blkSecurityParam' slots.
    -- In this case we will use 'Nothing' for the oldest blocks.
    let slogUndo :: OldestFirst [] (Maybe LastSlotInfo)
        slogUndo =
            map Just removedSlots & _Wrapped %~
            (replicate (length blocks - length removedSlots) Nothing <>)
    -- NE.fromList is safe here, because it's obvious that the size of
    -- 'slogUndo' is the same as the size of 'blocks'.
    return $ over _Wrapped NE.fromList $ map (SlogUndo . fmap lsiFlatSlotId) slogUndo

-- | Set of constraints necessary to apply/rollback blocks in Slog.
type MonadSlogApply ctx m =
    ( MonadSlogBase ctx m
    , MonadDB m
    , MonadBListener m
    , MonadMask m
    , MonadReader ctx m
    , HasSlogGState ctx
    )

-- | Flag determining whether to call BListener callback.
newtype ShouldCallBListener = ShouldCallBListener Bool

-- | This function does everything that should be done when blocks are
-- applied and is not done in other components.
--
-- The algorithm works as follows:
-- 1.  Put blunds in BlockDB (done first to preserve the invariant that the tip must exist in BlockDB).
-- 2.  Call 'BListener', get extra 'SomeBatchOp's from it.
-- 3.  Update @lastBlkSlots@ in-memory.
-- 4.  Return 'SomeBatchOp's for:
--     1. Updating tip
--     2. Updating max seen difficulty
--     3. 'BListener''s batch
--     4. Updating @lastBlkSlots@ in the DB
--     5. Adding new forward links
--     6. Setting @inMainChain@ flags
slogApplyBlocks
    :: MonadSlogApply ctx m
    => NetworkMagic
    -> BlockCount
    -> ShouldCallBListener
    -> OldestFirst NE Blund
    -> m SomeBatchOp
slogApplyBlocks nm k (ShouldCallBListener callBListener) blunds = do
    -- Note: it's important to put blunds first. The invariant is that
    -- the sequence of blocks corresponding to the tip must exist in
    -- BlockDB. If program is interrupted after we put blunds and
    -- before we update GState, this invariant won't be violated. If
    -- we update GState first, this invariant may be violated.
    putBlunds $ blunds ^. _OldestFirst
    -- If the program is interrupted at this point (after putting blunds
    -- in BlockDB), we will have garbage blunds in BlockDB, but it's not a
    -- problem.
    bListenerBatch <- if callBListener then onApplyBlocks nm blunds
                      else pure mempty

    let newestBlock = NE.last $ getOldestFirst blunds
        newestDifficulty = newestBlock ^. difficultyL
    let putTip = SomeBatchOp $ GS.PutTip $ headerHash newestBlock

    lastSlots <- slogGetLastBlkSlots
    slogPutLastSlots $ newLastSlots lastSlots

    putDifficulty <- GS.getMaxSeenDifficulty <&> \x ->
        SomeBatchOp [GS.PutMaxSeenDifficulty newestDifficulty
                        | newestDifficulty > x]
    return $ SomeBatchOp
        [ putTip
        , putDifficulty
        , bListenerBatch
        , SomeBatchOp blockExtraBatch
        ]
  where
    blocks = fmap fst blunds
    forwardLinks = map (view prevBlockL &&& view headerHashG) $ toList blocks
    forwardLinksBatch = map (uncurry GS.AddForwardLink) forwardLinks
    inMainBatch =
        toList $
        fmap (GS.SetInMainChain True . view headerHashG . fst) blunds

    blockExtraBatch :: [GS.BlockExtraOp]
    blockExtraBatch =
        mconcat [forwardLinksBatch, inMainBatch]

    newSlots :: [LastSlotInfo]
    newSlots = mapMaybe (blockLastSlotInfo (kEpochSlots k)) $ toList blocks

    newLastSlots :: LastBlkSlots -> LastBlkSlots
    newLastSlots lbs =
        LastBlkSlots.updateMany lbs $ OldestFirst newSlots


newtype BypassSecurityCheck = BypassSecurityCheck Bool

-- | This function does everything that should be done when rollback
-- happens and that is not done in other components.
--
-- The algorithm works as follows:
-- 1.  Assert that we are not rolling back 0th genesis block.
-- 2.  Check that we are not rolling back more than 'blkSecurityParam' blocks.
-- 3.  Call 'BListener', get extra 'SomeBatchOp's from it.
-- 4.  Return 'SomeBatchOp's for:
--     1. Reverting tip
--     2. 'BListener''s batch
--     3. Reverting @lastBlkSlots@
--     4. Removing forward links
--     5. Removing @inMainChain@ flags
{-# ANN slogRollbackBlocks ("HLint: ignore Reduce duplication" :: Text) #-}

slogRollbackBlocks ::
       MonadSlogApply ctx m
    => Genesis.Config
    -> BypassSecurityCheck -- ^ is rollback for more than k blocks allowed?
    -> ShouldCallBListener
    -> NewestFirst NE Blund
    -> m SomeBatchOp
slogRollbackBlocks genesisConfig (BypassSecurityCheck bypassSecurity) (ShouldCallBListener callBListener) blunds = do
    inAssertMode $ when (isGenesis0 (blocks ^. _Wrapped . _neLast)) $
        assertionFailed $
        colorize Red "FATAL: we are TRYING TO ROLLBACK 0-TH GENESIS block"
    -- We should never allow a situation when we summarily roll back by more
    -- than 'k' blocks
    maxSeenDifficulty <- GS.getMaxSeenDifficulty
    resultingDifficulty <-
        maybe 0 (view difficultyL) <$>
        DB.getHeader (NE.head (getOldestFirst . toOldestFirst $ blunds) ^. prevBlockL)
    let
        secure =
            -- no underflow from subtraction
            maxSeenDifficulty >= resultingDifficulty &&
            -- no rollback further than k blocks
            maxSeenDifficulty - resultingDifficulty <= fromIntegral (pcBlkSecurityParam $ configProtocolConstants genesisConfig)
    unless (bypassSecurity || secure) $
        reportFatalError "slogRollbackBlocks: the attempted rollback would \
                         \lead to a more than 'k' distance between tip and \
                         \last seen block, which is a security risk. Aborting."
    bListenerBatch <- if callBListener
                        then onRollbackBlocks
                                (makeNetworkMagic $ configProtocolMagic genesisConfig)
                                (configProtocolConstants genesisConfig)
                                blunds
                        else pure mempty

    let putTip =
            SomeBatchOp $ GS.PutTip $
            (NE.last $ getNewestFirst blunds) ^. prevBlockL

    lastSlots <- slogGetLastBlkSlots
    slogPutLastSlots $ newLastSlots lastSlots

    return $
        SomeBatchOp $
            [putTip, bListenerBatch, SomeBatchOp blockExtraBatch]
  where
    blocks = fmap fst blunds
    inMainBatch =
        map (GS.SetInMainChain False . view headerHashG) $ toList blocks
    forwardLinksBatch =
        map (GS.RemoveForwardLink . view prevBlockL) $ toList blocks

    blockExtraBatch :: [GS.BlockExtraOp]
    blockExtraBatch =
        mconcat [forwardLinksBatch, inMainBatch]

    lastSlotsToAppend :: [LastSlotInfo]
    lastSlotsToAppend =
        mapMaybe (blockLastSlotInfo (configEpochSlots genesisConfig) . fst)
            $ toList (toOldestFirst blunds)

    newLastSlots :: LastBlkSlots -> LastBlkSlots
    newLastSlots lbs =
        LastBlkSlots.updateMany lbs $ OldestFirst lastSlotsToAppend


isGenesis0 :: HasEpochIndex s => Either s b -> Bool
isGenesis0 (Left genesisBlk) = genesisBlk ^. epochIndexL == 0
isGenesis0 (Right _)         = False
