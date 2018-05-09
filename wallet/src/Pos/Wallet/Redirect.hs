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

import           Control.Lens (views)
import qualified Data.HashMap.Strict as HM
import           Data.Time.Units (Millisecond)
import           System.Wlog (WithLogger, logWarning)

import           Pos.Block.Types (LastKnownHeaderTag, MonadLastKnownHeader)
import qualified Pos.Context as PC
import           Pos.Core (ChainDifficulty, HasConfiguration, Timestamp, Tx, TxAux (..), TxId,
                           TxUndo, difficultyL, getCurrentTimestamp)
import           Pos.Core.Block (BlockHeader)
import           Pos.Crypto (WithHash (..))
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead)
import qualified Pos.DB.GState.Common as GS
import           Pos.Shutdown (HasShutdownContext, triggerShutdown)
import           Pos.Slotting (MonadSlots (..), getNextEpochSlotDuration)
import           Pos.Txp (MempoolExt, MonadTxpLocal (..), ToilVerFailure, TxpLocalWorkMode,
                          TxpProcessTransactionMode, getLocalUndos, txNormalize,
                          txProcessTransaction, withTxpLocalData)
import           Pos.Update.Context (UpdateContext (ucDownloadedUpdate))
import           Pos.Update.Poll.Types (ConfirmedProposalState)
import           Pos.Util.Util (HasLens (..))
import           Pos.Wallet.WalletMode (MonadBlockchainInfo (..), MonadUpdates (..))
import           Pos.Wallet.Web.Account (AccountMode, getSKById)
import           Pos.Wallet.Web.ClientTypes (CId, Wal)
import           Pos.Wallet.Web.Methods.History (addHistoryTxMeta)
import qualified Pos.Wallet.Web.State as WS
import           Pos.Wallet.Web.Tracking (THEntryExtra, buildTHEntryExtra,
                                          eskToWalletDecrCredentials, isTxEntryInteresting)

----------------------------------------------------------------------------
-- BlockchainInfo
----------------------------------------------------------------------------

getLastKnownHeader
  :: (MonadLastKnownHeader ctx m, MonadIO m)
  => m (Maybe BlockHeader)
getLastKnownHeader =
    atomically . readTVar =<< view (lensOf @LastKnownHeaderTag)

type BlockchainInfoEnv ctx m =
    ( MonadDBRead m
    , MonadLastKnownHeader ctx m
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
        thDiff <- view difficultyL <$> DB.getTipHeader
        let lhDiff = lh ^. difficultyL
        return . Just $ max thDiff lhDiff
    Nothing -> pure Nothing

localChainDifficultyWebWallet
    :: forall ctx m. BlockchainInfoEnv ctx m
    => m ChainDifficulty
localChainDifficultyWebWallet = do
    -- Workaround: Make local chain difficulty monotonic
    prevMaxDifficulty <- fromMaybe 0 <$> GS.getMaxSeenDifficultyMaybe
    currDifficulty <- view difficultyL <$> DB.getTipHeader
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
    , AccountMode ctx m
    , WS.WalletDbReader ctx m
    , HasConfiguration
    )
    => (TxId, TxAux) -> m (Either ToilVerFailure ())
txpProcessTxWebWallet tx@(txId, txAux) = do
    db <- WS.askWalletDB
    txProcessTransaction tx >>= traverse (const $ addTxToWallets db)
  where
    addTxToWallets :: WS.WalletDB -> m ()
    addTxToWallets db = do
        txUndos <- withTxpLocalData getLocalUndos
        case HM.lookup txId txUndos of
            Nothing ->
                logWarning "Node processed a tx but corresponding tx undo not found"
            Just txUndo -> do
                ws <- WS.getWalletSnapshot db
                ts <- getCurrentTimestamp
                let txWithUndo = (WithHash (taTx txAux) txId, txUndo)
                thees <- mapM (toThee txWithUndo ts) $ WS.getWalletAddresses ws
                let interestingThees = mapMaybe (\ (id, t) -> (id,) <$> isTxEntryInteresting t) thees
                mapM_ (uncurry $ addHistoryTxMeta db) interestingThees

    toThee :: (WithHash Tx, TxUndo) -> Timestamp -> CId Wal -> m (CId Wal, THEntryExtra)
    toThee txWithUndo ts wId = do
        wdc <- eskToWalletDecrCredentials <$> getSKById wId
        pure (wId, buildTHEntryExtra wdc txWithUndo (Nothing, Just ts))

txpNormalizeWebWallet :: (TxpLocalWorkMode ctx m, MempoolExt m ~ ()) => m ()
txpNormalizeWebWallet = txNormalize
