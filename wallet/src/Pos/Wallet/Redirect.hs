{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This module contains various default definitions
-- of some monads used by wallet.

module Pos.Wallet.Redirect
       ( MonadBlockchainInfo(..)
       , networkChainDifficultyWebWallet
       , localChainDifficultyWebWallet
       , connectedPeersWebWallet
       , blockchainSlotDurationWebWallet
       , MonadUpdates(..)
       , waitForUpdateWebWallet
       , applyLastUpdateWebWallet
       , MonadTxpLocal (..)
       , txpProcessTxWebWallet
       , txpNormalizeWebWallet
       ) where

import           Universum

import qualified Control.Concurrent.STM            as STM
import           Control.Exception.Safe            (onException)
import           Control.Lens                      (views)
import qualified Data.HashMap.Strict               as HM
import           Data.Time.Units                   (Millisecond)
import           Ether.Internal                    (HasLens (..))
import           System.Wlog                       (WithLogger, logWarning)

import           Pos.Block.Core                    (BlockHeader)
import qualified Pos.Context                       as PC
import           Pos.Core                          (ChainDifficulty, HasConfiguration,
                                                    Timestamp, difficultyL,
                                                    getCurrentTimestamp)
import           Pos.Crypto                        (WithHash (..))
import           Pos.DB.Block                      (MonadBlockDB)
import           Pos.DB.DB                         (getTipHeader)
import qualified Pos.GState                        as GS
import           Pos.Shutdown                      (HasShutdownContext, triggerShutdown)
import           Pos.Slotting                      (MonadSlots (..),
                                                    getNextEpochSlotDuration)
import           Pos.StateLock                     (Priority (..), withStateLock)
import           Pos.Txp                           (MonadTxpLocal (..), ToilVerFailure,
                                                    Tx, TxAux (..), TxId, TxUndo,
                                                    TxpNormalizeMempoolMode,
                                                    TxpProcessTransactionMode, askTxpMem,
                                                    getLocalTxsNUndo, txNormalize,
                                                    txProcessTransactionNoLock, txpTip)
import           Pos.Update.Context                (UpdateContext (ucDownloadedUpdate))
import           Pos.Update.Poll.Types             (ConfirmedProposalState)
import           Pos.Wallet.WalletMode             (MonadBlockchainInfo (..),
                                                    MonadUpdates (..))
import           Pos.Wallet.Web.Account            (AccountMode, getSKById)
import           Pos.Wallet.Web.ClientTypes        (CId, Wal)
import           Pos.Wallet.Web.Methods.History    (addHistoryTxMeta)
import qualified Pos.Wallet.Web.State              as WS
import           Pos.Wallet.Web.State.Memory.Logic (TxpMempoolToModifierEnv,
                                                    buildStorageModifier,
                                                    updateStorageModifierOnTx)
import           Pos.Wallet.Web.State.Memory.Types (ExtStorageModifier (..),
                                                    ExtStorageModifierVar,
                                                    HasExtStorageModifier,
                                                    StorageModifier (..))
import           Pos.Wallet.Web.Tracking           (BlocksStorageModifierVar,
                                                    HasBlocksStorageModifier,
                                                    THEntryExtra, buildTHEntryExtra,
                                                    eskToWalletDecrCredentials,
                                                    isTxEntryInteresting)

----------------------------------------------------------------------------
-- BlockchainInfo
----------------------------------------------------------------------------

getLastKnownHeader
  :: (PC.MonadLastKnownHeader ctx m, MonadIO m)
  => m (Maybe BlockHeader)
getLastKnownHeader =
    atomically . readTVar =<< view (lensOf @PC.LastKnownHeaderTag)

type BlockchainInfoEnv ctx m =
    ( MonadBlockDB m
    , PC.MonadLastKnownHeader ctx m
    , PC.MonadProgressHeader ctx m
    , MonadReader ctx m
    , HasLens PC.ConnectedPeers ctx PC.ConnectedPeers
    , MonadIO m
    , MonadSlots ctx m
    , HasConfiguration
    )

networkChainDifficultyWebWallet
    :: forall ctx m. BlockchainInfoEnv ctx m
    => m (Maybe ChainDifficulty)
networkChainDifficultyWebWallet = getLastKnownHeader >>= \case
    Just lh -> do
        thDiff <- view difficultyL <$> getTipHeader
        let lhDiff = lh ^. difficultyL
        return . Just $ max thDiff lhDiff
    Nothing -> pure Nothing

localChainDifficultyWebWallet
    :: forall ctx m. BlockchainInfoEnv ctx m
    => m ChainDifficulty
localChainDifficultyWebWallet = do
    -- Workaround: Make local chain difficulty monotonic
    prevMaxDifficulty <- fromMaybe 0 <$> GS.getMaxSeenDifficultyMaybe
    currDifficulty <- view difficultyL <$> getTipHeader
    return $ max prevMaxDifficulty currDifficulty

connectedPeersWebWallet
    :: forall ctx m. BlockchainInfoEnv ctx m
    => m Word
connectedPeersWebWallet = fromIntegral . length <$> do
    PC.ConnectedPeers cp <- view (lensOf @PC.ConnectedPeers)
    atomically (readTVar cp)

blockchainSlotDurationWebWallet
    :: forall ctx m. BlockchainInfoEnv ctx m
    => m Millisecond
blockchainSlotDurationWebWallet = getNextEpochSlotDuration

----------------------------------------------------------------------------
-- Updates
----------------------------------------------------------------------------

type UpdatesEnv ctx m =
    ( MonadIO m
    , WithLogger m
    , HasShutdownContext ctx
    , MonadReader ctx m
    , HasLens UpdateContext ctx UpdateContext
    )

waitForUpdateWebWallet :: UpdatesEnv ctx m => m ConfirmedProposalState
waitForUpdateWebWallet =
    takeMVar =<< views (lensOf @UpdateContext) ucDownloadedUpdate

applyLastUpdateWebWallet :: UpdatesEnv ctx m => m ()
applyLastUpdateWebWallet = triggerShutdown

----------------------------------------------------------------------------
-- Txp Local
----------------------------------------------------------------------------

txpProcessTxWebWallet
    :: forall ctx m .
    ( TxpProcessTransactionMode ctx m
    , AccountMode m
    , WS.MonadWalletDB ctx m
    , MonadBlockDB m
    , HasExtStorageModifier ctx
    )
    => (TxId, TxAux) -> m (Either ToilVerFailure ())
txpProcessTxWebWallet tx@(txId, txAux) =
    withStateLock LowPriority "txpProcessTxWebWallet" $ \_ ->
        txProcessTransactionNoLock tx >>= traverse (const walletLogic)
  where
    walletLogic :: m ()
    walletLogic = do
        (_, txUndos) <- getLocalTxsNUndo
        case HM.lookup txId txUndos of
            Nothing ->
                logWarning "Node processed a tx but corresponding tx undo not found"
            Just txUndo -> do
                updateStorageModifierOnTx (txId, txAux, txUndo)
                addTxToWallets txUndo

    addTxToWallets :: TxUndo -> m ()
    addTxToWallets txUndo = do
        ts <- getCurrentTimestamp
        let txWithUndo = (WithHash (taTx txAux) txId, txUndo)
        thees <- mapM (toThee txWithUndo ts) =<< WS.getWalletIds
        let interestingThees = mapMaybe (\ (id, t) -> (id,) <$> isTxEntryInteresting t) thees
        mapM_ (uncurry addHistoryTxMeta) interestingThees

    toThee :: (WithHash Tx, TxUndo) -> Timestamp -> CId Wal -> m (CId Wal, THEntryExtra)
    toThee txWithUndo ts wId = do
        wdc <- eskToWalletDecrCredentials <$> getSKById wId
        pure (wId, buildTHEntryExtra wdc txWithUndo (Nothing, Just ts))

txpNormalizeWebWallet
    :: ( TxpNormalizeMempoolMode ctx m
       , TxpMempoolToModifierEnv ctx m
       , AccountMode m
       , HasExtStorageModifier ctx
       , HasBlocksStorageModifier ctx
       , WS.MonadWalletDB ctx m
       , MonadMask m
       ) => m ()
txpNormalizeWebWallet = do
    txNormalize
    memTip <- atomically . readTVar =<< (txpTip <$> askTxpMem)
    memStorageModifierVar <- view (lensOf @ExtStorageModifierVar)
    -- Take value to show that
    -- the current thread are going to update wallet-db and modifier
    void $ atomically $ STM.tryTakeTMVar memStorageModifierVar
    updateWalletDBAndBuildModifier memTip
      `onException` atomically (STM.tryPutTMVar memStorageModifierVar (ExtStorageModifier memTip mempty))
  where
    updateWalletDBAndBuildModifier memTip = do
        -- Update wallet-db
        blocksStorageModifierVar <- view (lensOf @BlocksStorageModifierVar)
        blocksStorageModifier <- atomically $ getStorageModifier <$> readTVar blocksStorageModifierVar
        WS.applyModifierToWallets memTip (HM.toList blocksStorageModifier)

        -- Prepare data to update wallet modifier
        !memStorageMod <- buildStorageModifier
        memStorageModifierVar <- view (lensOf @ExtStorageModifierVar)

        atomically $ do
            -- Update blocks storage modifier and wallet modifier
            writeTVar blocksStorageModifierVar mempty
            STM.putTMVar memStorageModifierVar memStorageMod
