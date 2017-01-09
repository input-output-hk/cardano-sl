{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Workers responsible for Leaders and Richmen computation.

module Pos.Lrc.Worker
       ( lrcOnNewSlotWorker
       , lrcSingleShot
       ) where

import qualified Data.HashMap.Strict      as HM
import qualified Data.List.NonEmpty       as NE
import           Formatting               (build, sformat, (%))
import           Mockable                 (fork)
import           Node                     (SendActions)
import           Serokell.Util.Exceptions ()
import           System.Wlog              (logInfo)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Logic          (applyBlocks, rollbackBlocks, withBlkSemaphore_)
import           Pos.Communication.BiP    (BiP)
import           Pos.Constants            (k)
import           Pos.Context              (updateLrcSync)
import qualified Pos.DB                   as DB
import qualified Pos.DB.GState            as GS
import           Pos.DB.Lrc               (getLeaders, putEpoch, putLeaders)
import           Pos.Lrc.Consumer         (LrcConsumer (..))
import           Pos.Lrc.Consumers        (allLrcConsumers)
import           Pos.Lrc.Eligibility      (findAllRichmenMaybe)
import           Pos.Lrc.FollowTheSatoshi (followTheSatoshiM)
import           Pos.Slotting             (onNewSlot')
import           Pos.Ssc.Class            (SscWorkersClass)
import           Pos.Ssc.Extra            (sscCalculateSeed)
import           Pos.Types                (EpochIndex, EpochOrSlot (..), EpochOrSlot (..),
                                           HeaderHash, HeaderHash, SlotId (..),
                                           crucialSlot, getEpochOrSlot, getEpochOrSlot)
import           Pos.WorkMode             (NewWorkMode)

lrcOnNewSlotWorker
    :: (SscWorkersClass ssc, NewWorkMode ssc m)
    => SendActions BiP m -> m ()
lrcOnNewSlotWorker _ = onNewSlot' True $ lrcOnNewSlotImpl

lrcOnNewSlotImpl
    :: (SscWorkersClass ssc, NewWorkMode ssc m)
    => SlotId -> m ()
lrcOnNewSlotImpl SlotId {..} = when (siSlot < k) $ lrcSingleShot siEpoch

-- | Run leaders and richmen computation for given epoch. Behavior
-- when there are not enough blocks in db is currently unspecified.
lrcSingleShot
    :: (SscWorkersClass ssc, NewWorkMode ssc m)
    => EpochIndex -> m ()
lrcSingleShot epoch = lrcSingleShotImpl epoch allLrcConsumers

lrcSingleShotImpl
    :: NewWorkMode ssc m
    => EpochIndex -> [LrcConsumer m] -> m ()
lrcSingleShotImpl epoch consumers = do
    expectedRichmenComp <- filterM (flip lcIfNeedCompute epoch) consumers
    needComputeLeaders <- isNothing <$> getLeaders epoch
    let needComputeRichmen = not . null $ expectedRichmenComp
    when needComputeRichmen $ logInfo "Need to compute richmen"
    when needComputeLeaders $ logInfo "Need to compute leaders"
    when (needComputeLeaders || needComputeLeaders) $ do
        logInfo $ "LRC computation is starting"
        withBlkSemaphore_ $ lrcDo epoch consumers
        putEpoch epoch
        updateLrcSync epoch
        logInfo $ "LRC computation has finished"

lrcDo
    :: NewWorkMode ssc m
    => EpochIndex -> [LrcConsumer m] -> HeaderHash ssc -> m (HeaderHash ssc)
lrcDo epoch consumers tip = tip <$ do
    blockUndoList <- DB.loadBlocksFromTipWhile whileMoreOrEq5k
    when (null blockUndoList) $
        panic "No block has been generated during last k slots"
    let blockUndos = NE.fromList blockUndoList
    rollbackBlocks blockUndos
    richmenComputationDo epoch consumers
    leadersComputationDo epoch
    applyBlocks (NE.reverse blockUndos)
  where
    whileMoreOrEq5k b _ = getEpochOrSlot b > crucial
    crucial = EpochOrSlot $ Right $ crucialSlot epoch

leadersComputationDo :: NewWorkMode ssc m => EpochIndex -> m ()
leadersComputationDo epochId =
    unlessM (isJust <$> getLeaders epochId) $ do
        mbSeed <- sscCalculateSeed epochId
        totalStake <- GS.getTotalFtsStake
        leaders <-
            case mbSeed of
                Left e ->
                    panic $ sformat ("SSC couldn't compute seed: " %build) e
                Right seed ->
                    GS.iterateByTx (followTheSatoshiM seed totalStake) snd
        putLeaders epochId leaders

richmenComputationDo :: forall ssc m . NewWorkMode ssc m
    => EpochIndex -> [LrcConsumer m] -> m ()
richmenComputationDo epochIdx consumers = unless (null consumers) $ do
    -- [CSL-93] Use eligibility threshold here
    total <- GS.getTotalFtsStake
    let minThreshold = safeThreshold total (not . lcConsiderDelegated)
    let minThresholdD = safeThreshold total lcConsiderDelegated
    (richmen, richmenD) <- GS.iterateByStake
                               (findAllRichmenMaybe @ssc minThreshold minThresholdD)
                               identity
    let callCallback cons = void $ fork $
            if lcConsiderDelegated cons
            then lcComputedCallback cons epochIdx total
                   (HM.filter (>= lcThreshold cons total) richmenD)
            else lcComputedCallback cons epochIdx total
                   (HM.filter (>= lcThreshold cons total) richmen)
    mapM_ callCallback consumers
  where
    safeThreshold total f =
        safeMinimum
        $ map (flip lcThreshold total)
        $ filter f consumers
    safeMinimum a = if null a then Nothing else Just $ minimum a
