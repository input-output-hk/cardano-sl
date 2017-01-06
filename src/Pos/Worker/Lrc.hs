{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Workers responsible for Leaders and Richmen computation.

module Pos.Worker.Lrc
       ( lrcOnNewSlot
       ) where

import qualified Data.List.NonEmpty       as NE
import           Formatting               (build, sformat, (%))
import           Serokell.Util.Exceptions ()
import           System.Wlog              (logDebug)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Block.Logic          (applyBlocks, rollbackBlocks, withBlkSemaphore_)
import           Pos.Constants            (k)
import           Pos.Context              (getNodeContext, isLrcCompleted, ncSscLeaders,
                                           readLeaders)
import           Pos.DB                   (getTotalFtsStake, loadBlocksFromTipWhile,
                                           mapUtxoIterator, putSlotLeaders)
import           Pos.Eligibility          (findRichmenStake)
import           Pos.FollowTheSatoshi     (followTheSatoshiM)
import           Pos.Ssc.Extra            (sscCalculateSeed)
import           Pos.Types                (EpochOrSlot (..), HeaderHash, SlotId (..),
                                           TxIn, TxOutAux, crucialSlot, getEpochOrSlot,
                                           mkCoin, Coin, EpochOrSlot (..), HeaderHash,
                                           SlotId (..), StakeholderId, TxIn, TxOutAux,
                                           getEpochOrSlot, mkCoin)
import           Pos.WorkMode             (WorkMode)
import Pos.Richmen (LrcConsumer (..), allLrcComsumers)

lrcOnNewSlot :: WorkMode ssc m => SlotId -> m ()
lrcOnNewSlot slotId
    | siSlot slotId < k = do
        nc <- getNodeContext
        lrcCompl <- isLrcCompleted
        unless lrcCompl $
            withBlkSemaphore_ $ lrcDo slotId
    | otherwise = do
        nc <- getNodeContext
        let clearMVar = liftIO . void . tryTakeMVar
        lrcClear
        clearMVar $ ncSscLeaders nc

lrcDo
    :: WorkMode ssc m
    => SlotId -> HeaderHash ssc -> m (HeaderHash ssc)
lrcDo SlotId {siEpoch = epochId} tip = tip <$ do
    logDebug $ "It's time to compute leaders and parts"
    blockUndoList <- loadBlocksFromTipWhile whileMoreOrEq5k
    when (null blockUndoList) $
        panic "No one block hasn't been generated during last k slots"
    let blockUndos = NE.fromList blockUndoList
    rollbackBlocks blockUndos
    totalStake <- notImplemented
    -- [CSL-93] Use eligibility threshold here
    (richmen, richmenD) <-
        mapUtxoIterator @(StakeholderId, Coin)
            (findRichmenStake (Just . mkCoin $ 0) (Just . mkCoin $ 0))
            identity
    callCallback cons = fork_ $
        if lcConsiderDelegated cons then lcComputedCallback totalStake richmenD
        else lcComputedCallback totalStake richmen
    mapM_ callCallback allLrcComsumers
    nc <- getNodeContext
    let leadersMVar = ncSscLeaders nc
    whenM (liftIO $ isEmptyMVar leadersMVar) $ do
        mbSeed <- sscCalculateSeed epochId
        totalStake <- getTotalFtsStake
        leaders <-
            case mbSeed of
                Left e     -> panic $ sformat ("SSC couldn't compute seed: "%build) e
                Right seed -> mapUtxoIterator @(TxIn, TxOutAux) @TxOutAux
                              (followTheSatoshiM seed totalStake) snd
        liftIO $ putMVar leadersMVar leaders
    leaders <- readLeaders
    putSlotLeaders epochId leaders
    applyBlocks blockUndos
  where
    whileMoreOrEq5k b _ = getEpochOrSlot b >= crucial
    crucial = EpochOrSlot $ Right $ crucialSlot slotId

lrcClear
    :: WorkMode ssc m
    => lrcLeaders -> m ()
lrcClear = mapM_ lcClearCallback allLrcComsumers
