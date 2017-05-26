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
import qualified Data.HashMap.Strict   as HM
import qualified Data.List.NonEmpty    as NE
import qualified Ether
import           Formatting            (build, sformat, (%))
import           Serokell.Util         (Color (Red), colorize)
import           Serokell.Util.Verify  (formatAllErrors, verResToMonadError)
import           System.Wlog           (WithLogger, logWarning)

import           Pos.Binary.Core       ()
import           Pos.Block.BListener   (MonadBListener (..))
import           Pos.Block.Core        (Block, genBlockLeaders, mainBlockLeaderKey)
import           Pos.Block.Pure        (verifyBlocks)
import           Pos.Block.Types       (Blund)
import           Pos.Constants         (lastKnownBlockVersion)
import           Pos.Context           (lrcActionOnEpochReason)
import           Pos.Core              (BlockVersion (..), epochIndexL, headerHash,
                                        headerHashG, prevBlockL)
import           Pos.DB                (SomeBatchOp (..))
import           Pos.DB.Block          (putBlund)
import           Pos.DB.Class          (MonadDB, MonadDBPure)
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
type SlogMode ssc m
     = (MonadSlots m, SscHelpersClass ssc, MonadDBPure m, WithLogger m)

-- Sadly, MonadIO and MonadDB are needed for LRC, but it can be improved.
-- | Set of constraints needed for Slog verification.
type SlogVerifyMode ssc m
     = ( SlogMode ssc m
       , MonadError Text m
       , MonadIO m
       , MonadDB m
       , Ether.MonadReader' LrcContext m
       )

-- | Verify everything from block that is not checked by other components.
-- All blocks must be from the same epoch.
slogVerifyBlocks
    :: forall ssc m.
       SlogVerifyMode ssc m
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
    -- For all issuers of blocks we're processing retrieve their PSK
    -- if any and create a hashmap of these.
    pskCerts <-
        fmap (HM.fromList . catMaybes) $
        forM (rights $ toList $ blocks ^. _Wrapped) $ \b ->
        let issuer = b ^. mainBlockLeaderKey
        in fmap (issuer,) <$> GS.getPSKByIssuer issuer
    verResToMonadError formatAllErrors $
        verifyBlocks curSlot dataMustBeKnown adoptedBVD
        leaders pskCerts blocks

-- | Set of constraints necessary to apply/rollback blocks in Slog.
type SlogApplyMode ssc m
     = ( SlogMode ssc m
       , MonadDB m
       , MonadBListener m
       , MonadMask m
       )

-- | This function does everything that should be done when blocks are
-- applied and is not done in other components.
slogApplyBlocks ::
       forall ssc m. SlogApplyMode ssc m
    => OldestFirst NE (Blund ssc)
    -> m SomeBatchOp
slogApplyBlocks blunds = do
    -- Note: it's important to put blunds first
    mapM_ putBlund blunds
    -- If the program is interrupted at this point (after putting on block),
    -- we will rollback all wallet sets at the next launch.
    onApplyBlocks blunds `catch` logWarn
    let putTip =
            SomeBatchOp $
            GS.PutTip $ headerHash $ NE.last $ getOldestFirst blunds
    sanityCheckDB
    putSlottingData =<< GS.getSlottingData
    return $ SomeBatchOp [putTip, forwardLinksBatch, inMainBatch]
  where
    blocks = fmap fst blunds
    forwardLinks = map (view prevBlockL &&& view headerHashG) $ toList blocks
    forwardLinksBatch =
        SomeBatchOp $ map (uncurry GS.AddForwardLink) forwardLinks
    inMainBatch =
        SomeBatchOp . getOldestFirst $
        fmap (GS.SetInMainChain True . view headerHashG . fst) blunds
    -- ↓ was written by @pva701
    logWarn :: SomeException -> m ()
    logWarn = logWarning . sformat ("onApplyBlocks raised exception: " %build)

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
    -- If program is interrupted after call @onRollbackBlocks@,
    -- we will load all wallet set not rolled yet at the next launch.
    onRollbackBlocks blunds `catch` logWarn
    let putTip = SomeBatchOp $
                 GS.PutTip $
                 headerHash $
                 (NE.last $ getNewestFirst blunds) ^. prevBlockL
    sanityCheckDB
    return $ SomeBatchOp [putTip, forwardLinksBatch, inMainBatch]
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
    -- ↓ was written by @pva701
    logWarn :: SomeException -> m ()
    logWarn = logWarning . sformat ("onRollbackBlocks raised exception: "%build)
