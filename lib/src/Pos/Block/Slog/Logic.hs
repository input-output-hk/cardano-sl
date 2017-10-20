{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This module does some hard work related to block processing
-- logic, but not specific to any particular part of the
-- system. Contrary, modules like 'Pos.Block.Logic.VAR' unite logic
-- from multiple components into functions.

module Pos.Block.Slog.Logic
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

import           Control.Lens             (_Wrapped)
import           Control.Monad.Except     (MonadError (throwError))
import qualified Data.List.NonEmpty       as NE
import           Formatting               (build, sformat, (%))
import           Serokell.Util            (Color (Red), colorize)
import           Serokell.Util.Verify     (formatAllErrors, verResToMonadError)
import           System.Wlog              (WithLogger)

import           Pos.Binary.Core          ()
import           Pos.Block.BListener      (MonadBListener (..))
import           Pos.Block.Core           (Block, genBlockLeaders, mainBlockSlot)
import           Pos.Block.Pure           (verifyBlocks)
import           Pos.Block.Slog.Context   (slogGetLastSlots, slogPutLastSlots)
import           Pos.Block.Slog.Types     (HasSlogGState, LastBlkSlots, SlogUndo (..))
import           Pos.Block.Types          (Blund, Undo (..))
import           Pos.Context              (lrcActionOnEpochReason)
import           Pos.Core                 (BlockVersion (..), FlatSlotId,
                                           HasConfiguration, blkSecurityParam,
                                           difficultyL, epochIndexL, flattenSlotId,
                                           headerHash, headerHashG, prevBlockL)
import           Pos.DB                   (SomeBatchOp (..))
import           Pos.DB.Block             (MonadBlockDBWrite, blkGetHeader)
import           Pos.DB.Class             (MonadDBRead, dbPutBlund)
import           Pos.Exception            (assertionFailed, reportFatalError)
import qualified Pos.GState               as GS
import           Pos.Lrc.Context          (HasLrcContext)
import qualified Pos.Lrc.DB               as LrcDB
import           Pos.Slotting             (MonadSlots (getCurrentSlot))
import           Pos.Update.Configuration (HasUpdateConfiguration, lastKnownBlockVersion)
import           Pos.Util                 (inAssertMode, _neHead, _neLast)
import           Pos.Util.Chrono          (NE, NewestFirst (getNewestFirst),
                                           OldestFirst (..), toOldestFirst)

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
mustDataBeKnown :: HasUpdateConfiguration => BlockVersion -> Bool
mustDataBeKnown adoptedBV =
    lastKnownMajMin > adoptedMajMin || lastKnownBlockVersion == adoptedBV
  where
    toMajMin BlockVersion {..} = (bvMajor, bvMinor)
    lastKnownMajMin = toMajMin lastKnownBlockVersion
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
    , HasConfiguration
    , HasUpdateConfiguration
    )

-- | Set of constraints needed for Slog verification.
type MonadSlogVerify ctx m =
    ( MonadSlogBase ctx m
    , MonadReader ctx m
    , HasLrcContext ctx
    )

-- | Verify everything from block that is not checked by other components.
-- All blocks must be from the same epoch.
slogVerifyBlocks
    :: forall ctx m.
    ( MonadSlogVerify ctx m
    , MonadError Text m
    )
    => OldestFirst NE Block
    -> m (OldestFirst NE SlogUndo)
slogVerifyBlocks blocks = do
    curSlot <- getCurrentSlot
    (adoptedBV, adoptedBVD) <- GS.getAdoptedBVFull
    let dataMustBeKnown = mustDataBeKnown adoptedBV
    let headEpoch = blocks ^. _Wrapped . _neHead . epochIndexL
    leaders <-
        lrcActionOnEpochReason
            headEpoch
            (sformat
                 ("slogVerifyBlocks: there are no leaders for epoch " %build)
                 headEpoch)
            LrcDB.getLeaders
    -- We take head here, because blocks are in oldest first order and
    -- we know that all of them are from the same epoch. So if there
    -- is a genesis block, it must be head and only head.
    case blocks ^. _Wrapped . _neHead of
        (Left block) ->
            when (block ^. genBlockLeaders /= leaders) $
            throwError "Genesis block leaders don't match with LRC-computed"
        _ -> pass
    verResToMonadError formatAllErrors $
        verifyBlocks curSlot dataMustBeKnown adoptedBVD leaders blocks
    -- Here we need to compute 'SlogUndo'. When we add apply a block,
    -- we can remove one of the last slots stored in
    -- 'BlockExtra'. This removed slot must be put into 'SlogUndo'.
    lastSlots <- GS.getLastSlots
    let toFlatSlot = fmap (flattenSlotId . view mainBlockSlot) . rightToMaybe
    -- these slots will be added if we apply all blocks
    let newSlots = mapMaybe toFlatSlot (toList blocks)
    let combinedSlots :: OldestFirst [] FlatSlotId
        combinedSlots = lastSlots & _Wrapped %~ (<> newSlots)
    -- these slots will be removed if we apply all blocks, because we store
    -- only limited number of slots
    let removedSlots :: OldestFirst [] FlatSlotId
        removedSlots =
            combinedSlots & _Wrapped %~
            (take $ length combinedSlots - fromIntegral blkSecurityParam)
    -- Note: here we exploit the fact that genesis block can be only 'head'.
    -- If we have genesis block, then size of 'newSlots' will be less than
    -- number of blocks we verify. It means that there will definitely
    -- be 'Nothing' in the head of the result.
    --
    -- It also works fine if we store less than 'blkSecurityParam' slots.
    -- In this case we will use 'Nothing' for the oldest blocks.
    let slogUndo :: OldestFirst [] (Maybe FlatSlotId)
        slogUndo =
            map Just removedSlots & _Wrapped %~
            (replicate (length blocks - length removedSlots) Nothing <>)
    -- NE.fromList is safe here, because it's obvious that the size of
    -- 'slogUndo' is the same as the size of 'blocks'.
    return $ over _Wrapped NE.fromList $ map SlogUndo slogUndo

-- | Set of constraints necessary to apply/rollback blocks in Slog.
type MonadSlogApply ctx m =
    ( MonadSlogBase ctx m
    , MonadBlockDBWrite m
    , MonadBListener m
    , MonadMask m
    , MonadReader ctx m
    , HasSlogGState ctx
    )

-- {-# ANN slogApplyBlocks ("HLint: ignore Reduce duplication" :: Text) #-}
-- ↑ Doesn't work, HALP HALP
-- {-# ANN slogRollbackBlocks ("HLint: ignore Reduce duplication" :: Text) #-}
-- ↑ Doesn't work, HALP HALP
{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}
-- ↑ I reduced duplication by introducing 'slogCommon', but it wants
-- more and I don't.

-- | Flag determining whether to call BListener callback.
newtype ShouldCallBListener = ShouldCallBListener Bool

-- | This function does everything that should be done when blocks are
-- applied and is not done in other components.
slogApplyBlocks
    :: forall ctx m. (MonadSlogApply ctx m)
    => ShouldCallBListener
    -> OldestFirst NE Blund
    -> m SomeBatchOp
slogApplyBlocks (ShouldCallBListener callBListener) blunds = do
    -- Note: it's important to put blunds first. The invariant is that
    -- the sequence of blocks corresponding to the tip must exist in
    -- BlockDB. If program is interrupted after we put blunds and
    -- before we update GState, this invariant won't be violated. If
    -- we update GState first, this invariant may be violated.
    mapM_ dbPutBlund blunds
    -- If the program is interrupted at this point (after putting on
    -- block), we will have a garbage block in BlockDB, but it's not a
    -- problem.
    bListenerBatch <- if callBListener then onApplyBlocks blunds
                      else pure mempty

    let newestBlock = NE.last $ getOldestFirst blunds
        newestDifficulty = newestBlock ^. difficultyL
    let putTip = SomeBatchOp $ GS.PutTip $ headerHash newestBlock
    lastSlots <- slogGetLastSlots
    slogCommon (newLastSlots lastSlots)
    putDifficulty <- GS.getMaxSeenDifficulty <&> \x ->
        SomeBatchOp [GS.PutMaxSeenDifficulty newestDifficulty
                        | newestDifficulty > x]
    return $ SomeBatchOp
        [ putTip
        , putDifficulty
        , bListenerBatch
        , SomeBatchOp (blockExtraBatch lastSlots) ]
  where
    blocks = fmap fst blunds
    forwardLinks = map (view prevBlockL &&& view headerHashG) $ toList blocks
    forwardLinksBatch = map (uncurry GS.AddForwardLink) forwardLinks
    inMainBatch =
        toList $
        fmap (GS.SetInMainChain True . view headerHashG . fst) blunds
    mainBlocks = rights $ toList blocks
    newSlots = flattenSlotId . view mainBlockSlot <$> mainBlocks
    newLastSlots lastSlots = lastSlots & _Wrapped %~ updateLastSlots
    knownSlotsBatch lastSlots
        | null newSlots = []
        | otherwise = [GS.SetLastSlots $ newLastSlots lastSlots]
    -- Slots are in 'OldestFirst' order. So we put new slots to the
    -- end and drop old slots from the beginning.
    updateLastSlots lastSlots =
        leaveAtMostN (fromIntegral blkSecurityParam) (lastSlots ++ newSlots)
    leaveAtMostN :: Int -> [a] -> [a]
    leaveAtMostN n lst = drop (length lst - n) lst
    blockExtraBatch lastSlots =
        mconcat [knownSlotsBatch lastSlots, forwardLinksBatch, inMainBatch]

newtype BypassSecurityCheck = BypassSecurityCheck Bool

-- | This function does everything that should be done when rollback
-- happens and that is not done in other components.
slogRollbackBlocks ::
       forall ctx m. (MonadSlogApply ctx m)
    => BypassSecurityCheck -- ^ is rollback for more than k blocks allowed?
    -> ShouldCallBListener
    -> NewestFirst NE Blund
    -> m SomeBatchOp
slogRollbackBlocks (BypassSecurityCheck bypassSecurity) (ShouldCallBListener callBListener) blunds = do
    inAssertMode $ when (isGenesis0 (blocks ^. _Wrapped . _neLast)) $
        assertionFailed $
        colorize Red "FATAL: we are TRYING TO ROLLBACK 0-TH GENESIS block"
    -- We should never allow a situation when we summarily roll back by more
    -- than 'k' blocks
    maxSeenDifficulty <- GS.getMaxSeenDifficulty
    resultingDifficulty <-
        maybe 0 (view difficultyL) <$>
        blkGetHeader (NE.head (getOldestFirst . toOldestFirst $ blunds) ^. prevBlockL)
    let
        secure =
            -- no underflow from subtraction
            maxSeenDifficulty >= resultingDifficulty &&
            -- no rollback further than k blocks
            maxSeenDifficulty - resultingDifficulty <= fromIntegral blkSecurityParam
    unless (bypassSecurity || secure) $
        reportFatalError "slogRollbackBlocks: the attempted rollback would \
                         \lead to a more than 'k' distance between tip and \
                         \last seen block, which is a security risk. Aborting."
    bListenerBatch <- if callBListener then onRollbackBlocks blunds
                      else pure mempty
    let putTip =
            SomeBatchOp $ GS.PutTip $
            (NE.last $ getNewestFirst blunds) ^. prevBlockL
    lastSlots <- slogGetLastSlots
    slogCommon (newLastSlots lastSlots)
    return $
        SomeBatchOp
            [putTip, bListenerBatch, SomeBatchOp (blockExtraBatch lastSlots)]
  where
    blocks = fmap fst blunds
    inMainBatch =
        map (GS.SetInMainChain False . view headerHashG) $ toList blocks
    forwardLinksBatch =
        map (GS.RemoveForwardLink . view prevBlockL) $ toList blocks
    isGenesis0 (Left genesisBlk) = genesisBlk ^. epochIndexL == 0
    isGenesis0 (Right _)         = False
    lastSlotsToPrepend =
        mapMaybe (getSlogUndo . undoSlog . snd) $ toList (toOldestFirst blunds)
    newLastSlots lastSlots = lastSlots & _Wrapped %~ updateLastSlots
    dropEnd n xs = take (length xs - n) xs
    -- 'lastSlots' is what we currently store. It contains at most
    -- 'blkSecurityParam' slots. 'lastSlotsToPrepend' are slots for
    -- main blocks which are 'blkSecurityParam' far from the blocks we
    -- want to rollback.  Concatenation of these lists contains slots
    -- of some sequence of blocks. The last block in this sequence is
    -- the last block we rollback. The length of this sequence is
    -- 'min(total, n + blkSecurityParam)', where 'n' is the number of
    -- main blocks we want to rollback and 'total' is the total number
    -- of main blocks in our chain. So the final step is to drop last
    -- 'n' slots from this list.
    updateLastSlots lastSlots =
        dropEnd (length $ filter isRight $ toList blocks) $
        lastSlotsToPrepend ++
        lastSlots
    blockExtraBatch lastSlots =
        GS.SetLastSlots (newLastSlots lastSlots) :
        mconcat [forwardLinksBatch, inMainBatch]

-- Common actions for rollback and apply.
slogCommon
    :: MonadSlogApply ctx m
    => LastBlkSlots
    -> m ()
slogCommon newLastSlots = do
    slogPutLastSlots newLastSlots
