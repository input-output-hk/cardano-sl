{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Security.Workers
       ( SecurityWorkersClass (..)
       ) where

import           Control.Lens                      ((^.))
import           Control.Monad.Trans.State         (StateT (..), get, put)
import qualified Data.HashMap.Strict               as HM
import           Data.Tagged                       (Tagged (..))
import           System.Wlog                       (logWarning)
import           Universum                         hiding (modify)

import           Pos.Constants                     (k)
import           Pos.Context                       (getNodeContext, ncPublicKey)
import           Pos.Slotting                      (onNewSlot)
import           Pos.Ssc.Class.Types               (Ssc (..))
import           Pos.Ssc.GodTossing.Types.Instance ()
import           Pos.Ssc.GodTossing.Types.Type     (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types    (GtPayload (..), SscBi)
import           Pos.Ssc.NistBeacon                (SscNistBeacon)
import           Pos.State                         (getBlockByDepth, getHeadBlock)
import           Pos.Types                         (EpochIndex, MainBlock, SlotId (..),
                                                    blockMpc,
                                                    flattenSlotId, gbHeader, headerSlot,
                                                    gcdEpoch, gbhConsensus,
                                                    makePubKeyAddress)
import           Pos.WorkMode                      (WorkMode)

class Ssc ssc => SecurityWorkersClass ssc where
    securityWorkers :: WorkMode ssc m => Tagged ssc [m ()]

instance SscBi => SecurityWorkersClass SscGodTossing where
    securityWorkers = Tagged [ checkForReceivedBlocksWorker
                             , checkForIgnoredCommitmentsWorker
                             ]

instance SecurityWorkersClass SscNistBeacon where
    securityWorkers = Tagged [ checkForReceivedBlocksWorker
                             ]

-- TODO(voit): Move this to constants
blocksNotReceivedThreshold :: Integral a => a
blocksNotReceivedThreshold = 10

-- TODO(voit): Move this to constants
commitmentsAreIgnoredThreshold :: Integral a => a
commitmentsAreIgnoredThreshold = 5

reportAboutEclipsed :: WorkMode ssc m => m ()
reportAboutEclipsed = logWarning "We're doomed, we're eclipsed!"

checkForReceivedBlocksWorker :: WorkMode ssc m => m ()
checkForReceivedBlocksWorker = onNewSlot True $ \slotId -> do
    headBlock <- getHeadBlock
    case headBlock of
        Left genesis -> compareSlots slotId $ SlotId (genesis ^. gbHeader . gbhConsensus . gcdEpoch) 0
        Right blk    -> compareSlots slotId (blk ^. gbHeader . headerSlot)
  where
    compareSlots slotId blockGeneratedId = do
        let fSlotId = flattenSlotId slotId
        let fBlockGeneratedSlotId = flattenSlotId blockGeneratedId
        when (fSlotId - fBlockGeneratedSlotId > blocksNotReceivedThreshold)
            reportAboutEclipsed

checkForIgnoredCommitmentsWorker :: forall m. WorkMode SscGodTossing m => m ()
checkForIgnoredCommitmentsWorker = do
    _ <- runStateT (onNewSlot True checkForIgnoredCommitmentsWorkerImpl) 0
    return ()

checkForIgnoredCommitmentsWorkerImpl
    :: forall m. WorkMode SscGodTossing m
    => SlotId -> StateT EpochIndex m ()
checkForIgnoredCommitmentsWorkerImpl slotId = do
    checkCommitmentsInPreviousBlocks slotId
    lastCommitment <- get
    when (siEpoch slotId - lastCommitment > commitmentsAreIgnoredThreshold) $ do
        lift reportAboutEclipsed

checkCommitmentsInPreviousBlocks
    :: forall m. WorkMode SscGodTossing m
    => SlotId -> StateT EpochIndex m ()
checkCommitmentsInPreviousBlocks slotId = do
    forM_ [0 .. k - 1] $ \depth -> do
        headBlock <- getBlockByDepth depth
        case headBlock of
            Just (Right blk) -> checkCommitmentsInBlock slotId blk
            _                -> return ()

checkCommitmentsInBlock
    :: forall m. WorkMode SscGodTossing m
    => SlotId -> MainBlock SscGodTossing -> StateT EpochIndex m ()
checkCommitmentsInBlock slotId block = do
    ourAddr <- makePubKeyAddress . ncPublicKey <$> getNodeContext
    let commitmentInBlockchain = isCommitmentInPayload ourAddr (block ^. blockMpc)
    when commitmentInBlockchain $
        put $ siEpoch slotId
  where
    isCommitmentInPayload addr (CommitmentsPayload commitments _) = HM.member addr commitments
    isCommitmentInPayload _ _ = False
