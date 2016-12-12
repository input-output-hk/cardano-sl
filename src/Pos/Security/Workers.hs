{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Security.Workers
       ( SecurityWorkersClass (..)
       ) where

import           Control.Lens                      ((^.))
import           Control.Monad.Trans.State         (StateT (..), get, modify, put)
import qualified Data.HashMap.Strict               as HM
import           Data.Tagged                       (Tagged (..))
import           Formatting                        (build, int, sformat, (%))
import           System.Wlog                       (logWarning)
import           Universum                         hiding (modify)

import           Pos.Slotting                      (onNewSlot)
import           Pos.Ssc.Class.Types               (Ssc (..))
import           Pos.Ssc.GodTossing.Functions      (isOpeningId)
import           Pos.Ssc.GodTossing.Types.Instance ()
import           Pos.Ssc.GodTossing.Types.Type     (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types    (GtPayload (..))
import           Pos.Ssc.NistBeacon                (SscNistBeacon)
import           Pos.State                         (getHeadBlock)
import           Pos.Types                         (Address, SlotId, MainBlock, blockMpc,
                                                    flattenSlotId, flattenSlotId,
                                                    gbHeader, headerSlot,
                                                    makePubKeyAddress, slotIdF)
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

type EpochsCount = Word64

checkForIgnoredCommitmentsWorkerImpl ::
  forall m. WorkMode SscGodTossing m
  => SlotId -> StateT EpochsCount m ()
checkForIgnoredCommitmentsWorkerImpl slotId = do
  headBlock <- getHeadBlock
  case headBlock of
    Left _ -> return ()
    Right blk -> processBlock slotId blk

isCommitmentInPayload  :: forall m. WorkMode SscGodTossing m => Address -> GtPayload -> m Bool
isCommitmentInPayload addr p@(CommitmentsPayload commitments _) = do
  logWarning $ sformat ("Malicious commitment: "%build) p
  return $ HM.member addr commitments
isCommitmentInPayload _ p = do
  logWarning $ sformat ("Malicious other-type: "%build) p
  return False

processBlock ::
  forall m. WorkMode SscGodTossing m
  => SlotId -> MainBlock SscGodTossing -> StateT EpochsCount m ()
processBlock slotId block = do
  ourAddr <- makePubKeyAddress . ncPublicKey <$> getNodeContext
  -- let commitmentInBlockchain = isCommitmentInPayload ourAddr (block ^. blockMpc)
  commitmentInBlockchain <- lift $ isCommitmentInPayload ourAddr (block ^. blockMpc)
  when commitmentInBlockchain $
    put 0
  -- TODO(voit): Check if isOpeningId is the correct time to check about commitment
  epochs <- get
  logWarning $ sformat ("Malicious: slot="%slotIdF%", epochs="%int%", hasCommitment="%build%", ourAddr="%build) slotId epochs commitmentInBlockchain ourAddr
  when (and [isOpeningId slotId, not commitmentInBlockchain]) $ do
    modify (+1)
    when (epochs > commitmentsAreIgnoredThreshold) $
      lift reportAboutEclipsed
