{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This module does some hard work related to block processing
-- logic, but not specific to any particular part of the
-- system. Contrary, modules like 'VAR' unite logic from multiple
-- components into functions.
--
-- You can treat this part as another block component, while another
-- part of 'Pos.Block' is dedicated to whole blocks.

module Pos.Block.Logic.Slog
       ( mustDataBeKnown

       , SlogMode
       , SlogVerifyMode
       , slogVerifyBlocks

       , SlogApplyMode
       , slogApplyBlocks
       , slogRollbackBlocks
       ) where

import           Universum

import           Control.Lens          (_Wrapped)
import           Control.Monad.Except  (MonadError (throwError))
import qualified Data.List.NonEmpty    as NE
import           Ether.Internal        (HasLens (..))
import           Formatting            (build, sformat, (%))
import           Serokell.Util         (Color (Red), colorize)
import           Serokell.Util.Verify  (formatAllErrors, verResToMonadError)
import           System.Wlog           (WithLogger)

import           Pos.Binary.Core       ()
import           Pos.Block.BListener   (MonadBListener (..))
import           Pos.Block.Core        (Block, genBlockLeaders)
import           Pos.Block.Pure        (verifyBlocks)
import           Pos.Block.Types       (Blund)
import           Pos.Constants         (lastKnownBlockVersion)
import           Pos.Context           (lrcActionOnEpochReason)
import           Pos.Core              (BlockVersion (..), epochIndexL, headerHash,
                                        headerHashG, prevBlockL)
import           Pos.DB                (SomeBatchOp (..))
import           Pos.DB.Block          (MonadBlockDBWrite)
import           Pos.DB.Class          (MonadDBRead, dbPutBlund)
import           Pos.DB.DB             (sanityCheckDB)
import qualified Pos.DB.GState         as GS
import           Pos.Exception         (assertionFailed)
import           Pos.Lrc.Context       (LrcContext)
import qualified Pos.Lrc.DB            as LrcDB
import           Pos.Slotting          (MonadSlots (getCurrentSlot), putSlottingData)
import           Pos.Ssc.Class.Helpers (SscHelpersClass (..))
import           Pos.Util              (inAssertMode, _neHead, _neLast)
import           Pos.Util.Chrono       (NE, NewestFirst (getNewestFirst),
                                        OldestFirst (getOldestFirst))

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
mustDataBeKnown :: BlockVersion -> Bool
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
type SlogMode ssc m =
    ( MonadSlots m
    , SscHelpersClass ssc
    , MonadDBRead m
    , WithLogger m
    )

-- | Set of constraints needed for Slog verification.
type SlogVerifyMode ssc ctx m =
    ( SlogMode ssc m
    , MonadError Text m
    , MonadIO m
    , MonadReader ctx m
    , HasLens LrcContext ctx LrcContext
    )

-- | Verify everything from block that is not checked by other components.
-- All blocks must be from the same epoch.
slogVerifyBlocks
    :: forall ssc ctx m.
       SlogVerifyMode ssc ctx m
    => OldestFirst NE (Block ssc)
    -> m ()
slogVerifyBlocks blocks = do
    curSlot <- getCurrentSlot
    (adoptedBV, adoptedBVD) <- GS.getAdoptedBVFull
    let dataMustBeKnown = mustDataBeKnown adoptedBV
    let headEpoch = blocks ^. _Wrapped . _neHead . epochIndexL
    leaders <-
        lrcActionOnEpochReason
        headEpoch
        (sformat
         ("slogVerifyBlocks: there are no leaders for epoch "%build) headEpoch)
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

-- | Set of constraints necessary to apply/rollback blocks in Slog.
type SlogApplyMode ssc m =
    ( SlogMode ssc m
    , MonadBlockDBWrite ssc m
    , MonadBListener m
    , MonadMask m
    , MonadIO m
    )

-- | This function does everything that should be done when blocks are
-- applied and is not done in other components.
slogApplyBlocks ::
       forall ssc m. SlogApplyMode ssc m
    => OldestFirst NE (Blund ssc)
    -> m SomeBatchOp
slogApplyBlocks blunds = do
    -- Note: it's important to put blunds first. The invariant is that
    -- the sequence of blocks corresponding to the tip must exist in
    -- BlockDB. If program is interrupted after we put blunds and
    -- before we update GState, this invariant won't be violated. If
    -- we update GState first, this invariant may be violated.
    mapM_ dbPutBlund blunds
    -- If the program is interrupted at this point (after putting on
    -- block), we will have a garbage block in BlockDB, but it's not a
    -- problem.
    bListenerBatch <- onApplyBlocks blunds

    let putTip =
            SomeBatchOp $
            GS.PutTip $ headerHash $ NE.last $ getOldestFirst blunds
    sanityCheckDB
    putSlottingData =<< GS.getSlottingData
    return $ SomeBatchOp [putTip, bListenerBatch, forwardLinksBatch, inMainBatch]
  where
    blocks = fmap fst blunds
    forwardLinks = map (view prevBlockL &&& view headerHashG) $ toList blocks
    forwardLinksBatch =
        SomeBatchOp $ map (uncurry GS.AddForwardLink) forwardLinks
    inMainBatch =
        SomeBatchOp . getOldestFirst $
        fmap (GS.SetInMainChain True . view headerHashG . fst) blunds

-- | This function does everything that should be done when rollback
-- happens and that is not done in other components.
slogRollbackBlocks ::
       forall ssc m. SlogApplyMode ssc m
    => NewestFirst NE (Blund ssc)
    -> m SomeBatchOp
slogRollbackBlocks blunds = do
    inAssertMode $
        when (isGenesis0 (blocks ^. _Wrapped . _neLast)) $
        assertionFailed $
        colorize Red "FATAL: we are TRYING TO ROLLBACK 0-TH GENESIS block"
    bListenerBatch <- onRollbackBlocks blunds

    let putTip = SomeBatchOp $
                 GS.PutTip $
                 headerHash $
                 (NE.last $ getNewestFirst blunds) ^. prevBlockL
    sanityCheckDB
    return $ SomeBatchOp [putTip, bListenerBatch, forwardLinksBatch, inMainBatch]
  where
    blocks = fmap fst blunds
    inMainBatch =
        SomeBatchOp . getNewestFirst $
        fmap (GS.SetInMainChain False . view headerHashG) blocks
    forwardLinksBatch =
        SomeBatchOp . getNewestFirst $
        fmap (GS.RemoveForwardLink . view prevBlockL) blocks
    isGenesis0 (Left genesisBlk) = genesisBlk ^. epochIndexL == 0
    isGenesis0 (Right _)         = False
