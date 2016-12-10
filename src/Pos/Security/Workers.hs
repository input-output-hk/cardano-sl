{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}

module Pos.Security.Workers
       ( SecurityWorkersClass (..)
       ) where

import           Control.Lens                      ((^.))
import           Control.Monad.Trans.State         (StateT (..), get, put, modify)
import           Data.Tagged                       (Tagged (..))
import           Formatting                        (sformat, (%), int, build)
import           System.Wlog                       (logWarning)
import           Universum                         hiding (modify)

import           Pos.Slotting                      (onNewSlot)
import           Pos.Ssc.Class.Types               (Ssc (..))
import           Pos.Ssc.GodTossing.Functions      (hasCommitment, isOpeningId)
import           Pos.Ssc.GodTossing.Types.Instance ()
import           Pos.Ssc.GodTossing.Types.Type     (SscGodTossing)
import           Pos.Ssc.NistBeacon                (SscNistBeacon)
import           Pos.State                         (getHeadBlock, getGlobalMpcData)
import           Pos.Types                         (SlotId, flattenSlotId, gbHeader,
                                                    headerSlot, makePubKeyAddress, flattenSlotId,
                                                    slotIdF)
import           Pos.WorkMode                      (WorkMode, ncPublicKey, getNodeContext)

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
  ourAddr <- makePubKeyAddress . ncPublicKey <$> getNodeContext
  commitmentInBlockchain <- hasCommitment ourAddr <$> getGlobalMpcData
  when commitmentInBlockchain $
    put 0
  -- TODO(voit): Check if isOpeningId is the correct time to check about commitment
  epochs <- get
  logWarning $ sformat ("Malicious: slot="%slotIdF%", epochs="%int%", hasCommitment="%build) slotId epochs commitmentInBlockchain
  when (and [isOpeningId slotId, not commitmentInBlockchain]) $ do
    modify (+1)
    --epochs <- get
    when (epochs > commitmentsAreIgnoredThreshold) $
      lift reportAboutEclipsed
