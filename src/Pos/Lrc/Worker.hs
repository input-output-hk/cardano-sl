{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Workers responsible for Leaders and Richmen computation.

module Pos.Lrc.Worker
       ( lrcOnNewSlotWorker
       , lrcSingleShot
       ) where

import           Control.TimeWarp.Timed   (fork_)
import qualified Data.HashMap.Strict      as HM
import qualified Data.List.NonEmpty       as NE
import           Formatting               (build, sformat, (%))
import           Serokell.Util.Exceptions ()
import           System.Wlog              (logInfo)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Logic          (applyBlocks, rollbackBlocks, withBlkSemaphore_)
import           Pos.Constants            (k)
import           Pos.Context              (isLeadersComputed, readLeadersEager,
                                           writeLeaders)
import qualified Pos.DB                   as DB
import qualified Pos.DB.GState            as GS
import           Pos.Lrc.Consumers        (allLrcConsumers)
import           Pos.Lrc.Eligibility      (findAllRichmenMaybe)
import           Pos.Lrc.FollowTheSatoshi (followTheSatoshiM)
import           Pos.Lrc.Types            (LrcConsumer (..))
import           Pos.Slotting             (onNewSlot)
import           Pos.Ssc.Class            (SscWorkersClass)
import           Pos.Ssc.Extra            (sscCalculateSeed)
import           Pos.Types                (EpochIndex, EpochOrSlot (..), EpochOrSlot (..),
                                           HeaderHash, HeaderHash, SlotId (..),
                                           crucialSlot, getEpochOrSlot, getEpochOrSlot)
import           Pos.WorkMode             (WorkMode)

lrcOnNewSlotWorker :: (SscWorkersClass ssc, WorkMode ssc m) => m ()
lrcOnNewSlotWorker = onNewSlot True $ lrcOnNewSlotImpl allLrcConsumers

lrcOnNewSlotImpl :: WorkMode ssc m => [LrcConsumer m] -> SlotId -> m ()
lrcOnNewSlotImpl consumers SlotId{..}
    | siSlot < k = do
        expectedRichmenComp <- filterM (flip lcIfNeedCompute siEpoch) consumers
        needComputeLeaders <- not <$> isLeadersComputed siEpoch
        let needComputeRichmen = not . null $ expectedRichmenComp
        when needComputeRichmen $ logInfo "Need to compute richmen"
        when needComputeLeaders $ logInfo "Need to compute leaders"

        when (needComputeLeaders || needComputeLeaders) $ do
            logInfo $ "LRC computation is starting"
            lrcSingleShot siEpoch expectedRichmenComp
            logInfo $ "LRC computation has finished"
    | otherwise = pass

-- | Run leaders and richmen computation for given epoch. Behavior
-- when there are not enough blocks in db is currently unspecified.
lrcSingleShot
    :: WorkMode ssc m
    => EpochIndex -> [LrcConsumer m] -> m ()
lrcSingleShot epoch consumers = withBlkSemaphore_ $ lrcDo epoch consumers

lrcDo
    :: WorkMode ssc m
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

leadersComputationDo :: WorkMode ssc m => EpochIndex -> m ()
leadersComputationDo epochId = do
    unlessM (isLeadersComputed epochId) $ do
        mbSeed <- sscCalculateSeed epochId
        totalStake <- GS.getTotalFtsStake
        leaders <-
            case mbSeed of
                Left e     -> panic $ sformat ("SSC couldn't compute seed: "%build) e
                Right seed -> GS.iterateByTx (followTheSatoshiM seed totalStake) snd
        writeLeaders (epochId, leaders)
    (epoch, leaders) <- readLeadersEager
    DB.putLeaders (epoch, leaders)

richmenComputationDo :: forall ssc m . WorkMode ssc m
    => EpochIndex -> [LrcConsumer m] -> m ()
richmenComputationDo epochIdx consumers = unless (null consumers) $ do
    -- [CSL-93] Use eligibility threshold here
    total <- GS.getTotalFtsStake
    let minThreshold = safeThreshold total (not . lcConsiderDelegated)
    let minThresholdD = safeThreshold total lcConsiderDelegated
    (richmen, richmenD) <- GS.iterateByStake
                               (findAllRichmenMaybe @ssc minThreshold minThresholdD)
                               identity
    let callCallback cons = fork_ $
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
