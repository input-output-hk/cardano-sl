{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Workers responsible for Leaders and Richmen computation.

module Pos.Worker.Lrc
       ( lrcOnNewSlotWorker
       ) where

import           Control.TimeWarp.Timed   (fork_)
import qualified Data.HashMap.Strict      as HM
import qualified Data.List.NonEmpty       as NE
import           Formatting               (build, sformat, (%))
import           Serokell.Util.Exceptions ()
import           System.Wlog              (logDebug, logInfo)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Logic          (applyBlocks, rollbackBlocks, withBlkSemaphore_)
import           Pos.Constants            (k)
import           Pos.Context              (getNodeContext, isLeadersComputed,
                                           ncSscLeaders, readLeaders, writeLeaders)
import           Pos.DB                   (getTotalFtsStake, loadBlocksFromTipWhile,
                                           mapUtxoIterator, putLeaders)
import           Pos.FollowTheSatoshi     (followTheSatoshiM)
import           Pos.Richmen              (allLrcConsumers, findRichmenStake)
import           Pos.Slotting             (onNewSlot)
import           Pos.Ssc.Class            (SscWorkersClass)
import           Pos.Ssc.Extra            (sscCalculateSeed)
import           Pos.Types                (Coin, EpochOrSlot (..), EpochOrSlot (..),
                                           HeaderHash, HeaderHash, LrcConsumer (..),
                                           SlotId (..), SlotId (..), StakeholderId, TxIn,
                                           TxIn, TxOutAux, TxOutAux, crucialSlot,
                                           getEpochOrSlot, getEpochOrSlot)
import           Pos.Util                 (clearMVar)
import           Pos.WorkMode             (WorkMode)

lrcOnNewSlotWorker :: (SscWorkersClass ssc, WorkMode ssc m) => m ()
lrcOnNewSlotWorker = onNewSlot True $ lrcOnNewSlotImpl allLrcConsumers

lrcOnNewSlotImpl :: WorkMode ssc m => [LrcConsumer m] -> SlotId -> m ()
lrcOnNewSlotImpl consumers slotId
    | siSlot slotId < k = do
        expectedRichmenComp <- filterM (flip lcIfNeedCompute slotId) consumers
        needComputeLeaders <- not <$> isLeadersComputed
        let needComputeRichmen = not . null $ expectedRichmenComp
        when needComputeRichmen $ logInfo "Need to compute richmen"
        when needComputeLeaders $ logInfo "Need to compute leaders"

        when (needComputeLeaders || needComputeLeaders) $ do
            logInfo $ "LRC computation is starting"
            withBlkSemaphore_ $ lrcDo slotId expectedRichmenComp
            logInfo $ "LRC computation has finished"
    | otherwise = do
        nc <- getNodeContext
        lrcConsumersClear consumers
        clearMVar $ ncSscLeaders nc

lrcDo :: WorkMode ssc m
      => SlotId -> [LrcConsumer m] -> HeaderHash ssc -> m (HeaderHash ssc)
lrcDo slotId consumers tip = tip <$ do
    blockUndoList <- loadBlocksFromTipWhile whileMoreOrEq5k
    when (null blockUndoList) $
        panic "No one block hasn't been generated during last k slots"
    let blockUndos = NE.fromList blockUndoList
    rollbackBlocks blockUndos
    richmenComputationDo slotId consumers
    leadersComputationDo slotId
    applyBlocks blockUndos
  where
    whileMoreOrEq5k b _ = getEpochOrSlot b >= crucial
    crucial = EpochOrSlot $ Right $ crucialSlot slotId

richmenComputationDo :: WorkMode ssc m
    => SlotId -> [LrcConsumer m] -> m ()
richmenComputationDo slotId consumers = unless (null consumers) $ do
    -- [CSL-93] Use eligibility threshold here
    total <- getTotalFtsStake
    let minThreshold = safeThreshold total (not . lcConsiderDelegated)
    let minThresholdD = safeThreshold total lcConsiderDelegated
    (richmen, richmenD) <-
        mapUtxoIterator @(StakeholderId, Coin)
            (findRichmenStake minThreshold minThresholdD)
            identity
    logDebug $ "CONSUMERS " <> show (length consumers)
    logDebug $ "RICHMEN = " <> show richmen
    let callCallback cons = fork_ $
            if lcConsiderDelegated cons then
                lcComputedCallback cons
                                   slotId
                                   total
                                   (HM.filter (>= lcThreshold cons total) richmenD)
            else
                lcComputedCallback cons
                                   slotId
                                   total
                                   (HM.filter (>= lcThreshold cons total) richmen)
    mapM_ callCallback consumers
  where
    safeThreshold total f =
        safeMinimum
        $ map (flip lcThreshold total)
        $ filter f consumers
    safeMinimum a = if null a then Nothing else Just $ minimum a

leadersComputationDo :: WorkMode ssc m => SlotId -> m ()
leadersComputationDo SlotId {siEpoch = epochId} = do
    unlessM isLeadersComputed $ do
        mbSeed <- sscCalculateSeed epochId
        totalStake <- getTotalFtsStake
        leaders <-
            case mbSeed of
                Left e     -> panic $ sformat ("SSC couldn't compute seed: "%build) e
                Right seed -> mapUtxoIterator @(TxIn, TxOutAux) @TxOutAux
                              (followTheSatoshiM seed totalStake) snd
        writeLeaders leaders
    leaders <- readLeaders
    putLeaders epochId leaders

lrcConsumersClear :: WorkMode ssc m => [LrcConsumer m] -> m ()
lrcConsumersClear = mapM_ lcClearCallback
-- dangerous ^, one thread
