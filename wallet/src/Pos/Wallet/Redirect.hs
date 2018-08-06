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

import           Universum hiding (id)

import           Control.Lens (views)
import qualified Data.HashMap.Strict as HM
import           Data.Time.Units (Millisecond)

import           Pos.Chain.Block (LastKnownHeaderTag, MonadLastKnownHeader)
import           Pos.Chain.Txp (ToilVerFailure, TxpConfiguration)
import           Pos.Chain.Update (ConfirmedProposalState)
import qualified Pos.Context as PC
import           Pos.Core (ChainDifficulty, HasConfiguration, Timestamp,
                     difficultyL, getCurrentTimestamp)
import           Pos.Core.Block (BlockHeader)
import           Pos.Core.Txp (Tx, TxAux (..), TxId, TxUndo)
import           Pos.Crypto (ProtocolMagic, WithHash (..))
import qualified Pos.DB.BlockIndex as DB
import           Pos.DB.Class (MonadDBRead)
import qualified Pos.DB.GState.Common as GS
import           Pos.DB.Txp (MempoolExt, MonadTxpLocal (..), TxpLocalWorkMode,
                     TxpProcessTransactionMode, getLocalUndos, txNormalize,
                     txProcessTransaction, withTxpLocalData)
import           Pos.DB.Update (UpdateContext (ucDownloadedUpdate))
import           Pos.Infra.Shutdown (HasShutdownContext, triggerShutdown)
import           Pos.Infra.Slotting (MonadSlots (..), getNextEpochSlotDuration)
import           Pos.Util.Trace (noTrace)
import           Pos.Util.Trace.Named (TraceNamed, logWarning)
import           Pos.Util.Util (HasLens (..))
import           Pos.Wallet.WalletMode (MonadBlockchainInfo (..),
                     MonadUpdates (..))
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
    readTVarIO =<< view (lensOf @LastKnownHeaderTag)

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
    readTVarIO cp

blockchainSlotDurationWebWallet
    :: forall ctx m. BlockchainInfoEnv ctx m
    => m Millisecond
blockchainSlotDurationWebWallet = getNextEpochSlotDuration

----------------------------------------------------------------------------
-- Updates
----------------------------------------------------------------------------

type UpdatesEnv ctx m =
    ( MonadIO m
    , HasShutdownContext ctx
    , MonadReader ctx m
    , HasLens UpdateContext ctx UpdateContext
    )

waitForUpdateWebWallet :: UpdatesEnv ctx m => m ConfirmedProposalState
waitForUpdateWebWallet =
    takeMVar =<< views (lensOf @UpdateContext) ucDownloadedUpdate

applyLastUpdateWebWallet :: UpdatesEnv ctx m => TraceNamed m -> m ()
applyLastUpdateWebWallet logTrace = triggerShutdown logTrace

----------------------------------------------------------------------------
-- Txp Local
----------------------------------------------------------------------------

txpProcessTxWebWallet
    :: forall ctx m .
    ( TxpProcessTransactionMode ctx m
    , AccountMode ctx m
    , WS.WalletDbReader ctx m
    )
    => TraceNamed m
    -> ProtocolMagic
    -> TxpConfiguration
    -> (TxId, TxAux)
    -> m (Either ToilVerFailure ())
txpProcessTxWebWallet logTrace pm txpConfig tx@(txId, txAux) = do
    db <- WS.askWalletDB
    txProcessTransaction logTrace noTrace pm txpConfig tx >>= traverse (const $ addTxToWallets db)
  where
    addTxToWallets :: WS.WalletDB -> m ()
    addTxToWallets db = do
        txUndos <- withTxpLocalData getLocalUndos
        case HM.lookup txId txUndos of
            Nothing ->
                logWarning logTrace "Node processed a tx but corresponding tx undo not found"
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

txpNormalizeWebWallet
    :: ( TxpLocalWorkMode ctx m
       , MempoolExt m ~ ()
       )
    => TraceNamed m
    -> ProtocolMagic
    -> TxpConfiguration
    -> m ()
txpNormalizeWebWallet = txNormalize
