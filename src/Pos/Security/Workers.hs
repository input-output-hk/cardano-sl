{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}

module Pos.Security.Workers
       ( SecurityWorkersClass (..)
       ) where

import           Control.Lens                      ((^.))
import           Data.Tagged                       (Tagged (..))
import           System.Wlog                       (logWarning)
import           Universum

import           Pos.Slotting                      (onNewSlot)
import           Pos.Ssc.Class.Types               (Ssc (..))
import           Pos.Ssc.GodTossing.Functions      (hasCommitment)
import           Pos.Ssc.GodTossing.Types.Instance ()
import           Pos.Ssc.GodTossing.Types.Type     (SscGodTossing)
import           Pos.Ssc.NistBeacon                (SscNistBeacon)
import           Pos.State                         (getHeadBlock, getGlobalMpcData)
import           Pos.Types                         (SlotId, flattenSlotId, gbHeader,
                                                    headerSlot, makePubKeyAddress, flattenSlotId)
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

-- TODO(voit): This is just a stub
checkForIgnoredCommitmentsWorker :: forall m. WorkMode SscGodTossing m => m ()
checkForIgnoredCommitmentsWorker = onNewSlot True $ \slotId -> do
  ourAddr <- makePubKeyAddress . ncPublicKey <$> getNodeContext
  commitmentInBlockchain <- hasCommitment ourAddr <$> getGlobalMpcData
  when (not commitmentInBlockchain) $
    reportAboutEclipsed
