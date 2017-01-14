{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Security.Workers
       ( SecurityWorkersClass (..)
       ) where

import           Control.Concurrent.STM            (TVar, newTVar, readTVar, writeTVar)
import           Control.Lens                      ((^.))
import           Control.Monad.Trans.Reader        (ReaderT (..), ask)
import qualified Data.HashMap.Strict               as HM
import           Data.Tagged                       (Tagged (..))
import           Formatting                        (sformat, (%))
import           Node                              (SendActions)
import           System.Wlog                       (logError, logWarning)
import           Universum                         hiding (ask)

import           Pos.Block.Network.Retrieval       (mkHeadersRequest, requestHeaders)
import           Pos.Communication.BiP             (BiP)
import           Pos.Constants                     (blkSecurityParam,
                                                    mdNoBlocksSlotThreshold,
                                                    mdNoCommitmentsEpochThreshold)
import           Pos.Context                       (getNodeContext, ncPublicKey)
import           Pos.Crypto                        (PublicKey, shortHashF)
import           Pos.DB                            (getBlockHeader, getTipBlockHeader,
                                                    loadBlundsFromTipByDepth)
import           Pos.DHT.Model                     (converseToNeighbors)
import           Pos.Security.Class                (SecurityWorkersClass (..))
import           Pos.Slotting                      (onNewSlot)
import           Pos.Ssc.GodTossing.Types.Instance ()
import           Pos.Ssc.GodTossing.Types.Type     (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types    (GtPayload (..), SscBi)
import           Pos.Ssc.NistBeacon                (SscNistBeacon)
import           Pos.Types                         (BlockHeader, EpochIndex, MainBlock,
                                                    SlotId (..), blockMpc,
                                                    flattenEpochOrSlot, flattenSlotId,
                                                    genesisHash, headerLeaderKey,
                                                    prevBlockL)
import           Pos.Types.Address                 (addressHash)
import           Pos.WorkMode                      (WorkMode)

instance SscBi => SecurityWorkersClass SscGodTossing where
    securityWorkers = Tagged [ checkForReceivedBlocksWorker
                             , checkForIgnoredCommitmentsWorker
                             ]

instance SecurityWorkersClass SscNistBeacon where
    securityWorkers = Tagged [ checkForReceivedBlocksWorker
                             ]

reportAboutEclipsed :: WorkMode ssc m => m ()
reportAboutEclipsed = logWarning "We're doomed, we're eclipsed!"

checkForReceivedBlocksWorker :: WorkMode ssc m => SendActions BiP m -> m ()
checkForReceivedBlocksWorker sendActions = onNewSlot True $ \slotId -> do
    ourPk <- ncPublicKey <$> getNodeContext
    whenM (getTipBlockHeader >>= iterateWhileOurs ourPk slotId) onEclipse
  where
    onEclipse = do
        reportAboutEclipsed
        mghM <- mkHeadersRequest Nothing
        whenJust mghM $ \mgh ->
            converseToNeighbors sendActions (requestHeaders mgh)
    condition slotId h = flattenSlotId slotId - flattenEpochOrSlot h > mdNoBlocksSlotThreshold
    isNotOurs _ (Left _)       = False
    isNotOurs ourPk (Right mb) = mb ^. headerLeaderKey /= ourPk
    iterateWhileOurs :: WorkMode ssc m => PublicKey -> SlotId -> BlockHeader ssc -> m Bool
    iterateWhileOurs ourPk slotId h
        | condition slotId h = return True
        | isNotOurs ourPk h = return False
        | h ^. prevBlockL == genesisHash = return False
        | otherwise = maybe onLoadFailure (iterateWhileOurs ourPk slotId)
                            =<< getBlockHeader (h ^. prevBlockL)
      where
        onLoadFailure :: WorkMode ssc m => m Bool
        onLoadFailure = False <$ logError (sformat
                                    ("no block corresponding to hash "%shortHashF) (h ^. prevBlockL))

checkForIgnoredCommitmentsWorker :: forall m. WorkMode SscGodTossing m => SendActions BiP m -> m ()
checkForIgnoredCommitmentsWorker  __sendActions= do
    epochIdx <- atomically (newTVar 0)
    _ <- runReaderT (onNewSlot True checkForIgnoredCommitmentsWorkerImpl) epochIdx
    return ()

checkForIgnoredCommitmentsWorkerImpl
    :: forall m. WorkMode SscGodTossing m
    => SlotId -> ReaderT (TVar EpochIndex) m ()
checkForIgnoredCommitmentsWorkerImpl slotId = do
    checkCommitmentsInPreviousBlocks slotId
    tvar <- ask
    lastCommitment <- lift $ atomically $ readTVar tvar
    when (siEpoch slotId - lastCommitment > mdNoCommitmentsEpochThreshold) $ do
        lift reportAboutEclipsed

checkCommitmentsInPreviousBlocks
    :: forall m. WorkMode SscGodTossing m
    => SlotId -> ReaderT (TVar EpochIndex) m ()
checkCommitmentsInPreviousBlocks slotId = do
    kBlocks <- map fst <$> loadBlundsFromTipByDepth blkSecurityParam
    forM_ kBlocks $ \case
        Right blk -> checkCommitmentsInBlock slotId blk
        _         -> return ()

checkCommitmentsInBlock
    :: forall m. WorkMode SscGodTossing m
    => SlotId -> MainBlock SscGodTossing -> ReaderT (TVar EpochIndex) m ()
checkCommitmentsInBlock slotId block = do
    ourId <- addressHash . ncPublicKey <$> getNodeContext
    let commitmentInBlockchain = isCommitmentInPayload ourId (block ^. blockMpc)
    when commitmentInBlockchain $ do
        tvar <- ask
        lift $ atomically $ writeTVar tvar $ siEpoch slotId
  where
    isCommitmentInPayload addr (CommitmentsPayload commitments _) = HM.member addr commitments
    isCommitmentInPayload _ _ = False
