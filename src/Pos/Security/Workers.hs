{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Security.Workers
       ( SecurityWorkersClass (..)
       ) where

import           Control.Lens                      ((^.))
import           Control.Monad.Trans.State         (StateT (..), get, put)
import qualified Data.HashMap.Strict               as HM
import           Data.Tagged                       (Tagged (..))
import           Formatting                        (build, int, sformat, (%))
import           System.Wlog                       (logWarning)
import           Universum                         hiding (modify)

import           Pos.Constants                     (k)
import           Pos.Slotting                      (onNewSlot)
import           Pos.Ssc.Class.Types               (Ssc (..))
import           Pos.Ssc.GodTossing.Types.Instance ()
import           Pos.Ssc.GodTossing.Types.Type     (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types    (GtPayload (..))
import           Pos.Ssc.NistBeacon                (SscNistBeacon)
import           Pos.State                         (getHeadBlock, getBlockByDepth)
import           Pos.Types                         (Address, EpochIndex, SlotId (..),
                                                    MainBlock, blockMpc,
                                                    flattenSlotId, flattenSlotId,
                                                    gbHeader, headerSlot, makePubKeyAddress,
                                                    slotIdF)
import           Pos.WorkMode                      (WorkMode, getNodeContext, ncPublicKey)

class Ssc ssc => SecurityWorkersClass ssc where
  securityWorkers :: WorkMode ssc m => Tagged ssc [m ()]

instance SecurityWorkersClass SscGodTossing where
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
    Left _    -> return ()
    Right blk -> do
      let fSlotId = flattenSlotId slotId
      let fBlockGeneratedSlotId = flattenSlotId (blk ^. gbHeader . headerSlot)
      when (fSlotId - fBlockGeneratedSlotId > blocksNotReceivedThreshold)
        reportAboutEclipsed

checkForIgnoredCommitmentsWorker :: forall m. WorkMode SscGodTossing m => m ()
checkForIgnoredCommitmentsWorker =
  runStateT (onNewSlot True checkForIgnoredCommitmentsWorkerImpl) 0 >> return ()

checkForIgnoredCommitmentsWorkerImpl ::
  forall m. WorkMode SscGodTossing m
  => SlotId -> StateT EpochIndex m ()
checkForIgnoredCommitmentsWorkerImpl slotId = do
  commitmentIgnored <- not <$> isCommitmentIncludedInAnyPreviousBlock slotId
  lastCommitment <- get
  when (and [commitmentIgnored, siEpoch slotId - lastCommitment > commitmentsAreIgnoredThreshold]) $ do
    lift reportAboutEclipsed

isCommitmentIncludedInAnyPreviousBlock ::
  forall m. WorkMode SscGodTossing m
  => SlotId -> StateT EpochIndex m Bool
isCommitmentIncludedInAnyPreviousBlock slotId = do
  fmap or $ forM [0 .. k - 1] $ \depth -> do
    headBlock <- getBlockByDepth depth
    case headBlock of
      Just (Right blk) -> logWarning (sformat ("Malicious: processing depth: "%int) depth) >> processBlock slotId blk
      _ -> return False

isCommitmentInPayload  :: forall m. WorkMode SscGodTossing m => Address -> GtPayload -> m Bool
isCommitmentInPayload addr p@(CommitmentsPayload commitments _) = do
  logWarning $ sformat ("Malicious commitment: "%build) p
  return $ HM.member addr commitments

isCommitmentInPayload _ p = do
  logWarning $ sformat ("Malicious other-type: "%build) p
  return False

processBlock ::
  forall m. WorkMode SscGodTossing m
  => SlotId -> MainBlock SscGodTossing -> StateT EpochIndex m Bool
processBlock slotId block = do
  ourAddr <- makePubKeyAddress . ncPublicKey <$> getNodeContext
  commitmentInBlockchain <- lift $ isCommitmentInPayload ourAddr (block ^. blockMpc)
  when commitmentInBlockchain $
    put $ siEpoch slotId
  epochs <- get
  logWarning $ sformat ("Malicious: slot="%slotIdF%", epochs="%int%", hasCommitment="%build%", ourAddr="%build) slotId epochs commitmentInBlockchain ourAddr
  return commitmentInBlockchain
