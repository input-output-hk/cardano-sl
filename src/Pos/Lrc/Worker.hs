{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Workers responsible for Leaders and Richmen computation.

module Pos.Lrc.Worker
       ( lrcOnNewSlotWorker
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
import           Pos.Context              (isLeadersComputed, readLeadersEager,
                                           writeLeaders)
import qualified Pos.DB                   as DB
import qualified Pos.DB.GState            as GS
import           Pos.Lrc.Consumers        (allLrcConsumers)
import           Pos.Lrc.Eligibility      (findAllRichmenMaybe)
import           Pos.Lrc.FollowTheSatoshi (followTheSatoshiM)
import           Pos.Lrc.Types            (LrcConsumer (..))
import           Pos.Slotting             (onNewSlot')
import           Pos.Ssc.Class            (SscWorkersClass)
import           Pos.Ssc.Extra            (sscCalculateSeed)
import           Pos.Types                (EpochOrSlot (..), EpochOrSlot (..), HeaderHash,
                                           HeaderHash, SlotId (..), SlotId (..),
                                           crucialSlot, getEpochOrSlot, getEpochOrSlot)
import           Pos.WorkMode             (NewWorkMode)

lrcOnNewSlotWorker :: (SscWorkersClass ssc, NewWorkMode ssc m) => SendActions BiP m -> m ()
lrcOnNewSlotWorker = const $ onNewSlot' True $ lrcOnNewSlotImpl allLrcConsumers

lrcOnNewSlotImpl :: NewWorkMode ssc m => [LrcConsumer m] -> SlotId -> m ()
lrcOnNewSlotImpl consumers slotId@SlotId{..}
    | siSlot < k = do
        expectedRichmenComp <- filterM (flip lcIfNeedCompute slotId) consumers
        needComputeLeaders <- not <$> isLeadersComputed siEpoch
        let needComputeRichmen = not . null $ expectedRichmenComp
        when needComputeRichmen $ logInfo "Need to compute richmen"
        when needComputeLeaders $ logInfo "Need to compute leaders"

        when (needComputeLeaders || needComputeLeaders) $ do
            logInfo $ "LRC computation is starting"
            withBlkSemaphore_ $ lrcDo slotId expectedRichmenComp
            logInfo $ "LRC computation has finished"
    | otherwise = lrcConsumersClear consumers

lrcDo :: NewWorkMode ssc m
      => SlotId -> [LrcConsumer m] -> HeaderHash ssc -> m (HeaderHash ssc)
lrcDo slotId consumers tip = tip <$ do
    blockUndoList <- DB.loadBlocksFromTipWhile whileMoreOrEq5k
    when (null blockUndoList) $
        panic "No one block hasn't been generated during last k slots"
    let blockUndos = NE.fromList blockUndoList
    rollbackBlocks blockUndos
    richmenComputationDo slotId consumers
    leadersComputationDo slotId
    applyBlocks (NE.reverse blockUndos)
  where
    whileMoreOrEq5k b _ = getEpochOrSlot b > crucial
    crucial = EpochOrSlot $ Right $ crucialSlot slotId

leadersComputationDo :: NewWorkMode ssc m => SlotId -> m ()
leadersComputationDo SlotId {siEpoch = epochId} = do
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

richmenComputationDo :: forall ssc m . NewWorkMode ssc m
    => SlotId -> [LrcConsumer m] -> m ()
richmenComputationDo slotId consumers = unless (null consumers) $ do
    -- [CSL-93] Use eligibility threshold here
    total <- GS.getTotalFtsStake
    let minThreshold = safeThreshold total (not . lcConsiderDelegated)
    let minThresholdD = safeThreshold total lcConsiderDelegated
    (richmen, richmenD) <- GS.iterateByStake
                               (findAllRichmenMaybe @ssc minThreshold minThresholdD)
                               identity
    let callCallback cons = void $ fork $
            if lcConsiderDelegated cons
            then lcComputedCallback cons slotId total
                   (HM.filter (>= lcThreshold cons total) richmenD)
            else lcComputedCallback cons slotId total
                   (HM.filter (>= lcThreshold cons total) richmen)
    mapM_ callCallback consumers
  where
    safeThreshold total f =
        safeMinimum
        $ map (flip lcThreshold total)
        $ filter f consumers
    safeMinimum a = if null a then Nothing else Just $ minimum a

lrcConsumersClear :: NewWorkMode ssc m => [LrcConsumer m] -> m ()
lrcConsumersClear = mapM_ lcClearCallback
-- dangerous ^, one thread
