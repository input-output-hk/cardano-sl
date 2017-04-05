{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'WalletMode' constraint. Like `WorkMode`, but for wallet.

module Pos.Wallet.WalletMode
       ( MonadBalances (..)
       , MonadTxHistory (..)
       , MonadBlockchainInfo (..)
       , MonadUpdates (..)
       , WalletMode
       , WalletRealMode
       ) where

import           Control.Concurrent.STM      (TMVar, tryReadTMVar)
import           Control.Monad.Trans         (MonadTrans)
import           Control.Monad.Trans.Maybe   (MaybeT (..))
import           Data.Tagged                 (Tagged (..))
import           Data.Time.Units             (Millisecond)
import           Mockable                    (Production)
import           System.Wlog                 (LoggerNameBox, WithLogger)
import           Universum

import           Pos.Client.Txp.Balances     (MonadBalances (..))
import           Pos.Client.Txp.History      (MonadTxHistory (..), deriveAddrHistory)
import           Pos.Communication           (TxMode)
import           Pos.Communication.PeerState (PeerStateHolder, WithPeerState)
import           Pos.Constants               (blkSecurityParam)
import qualified Pos.Context                 as PC
import           Pos.DB                      (MonadDB)
import qualified Pos.DB.Block                as DB
import           Pos.DB.Error                (DBError (..))
import qualified Pos.DB.GState               as GS
import           Pos.Delegation              (DelegationT (..))
import           Pos.Shutdown                (triggerShutdown)
import           Pos.Slotting                (MonadSlots (..), NtpSlotting,
                                              SlottingHolder, getLastKnownSlotDuration)
import           Pos.Ssc.Class               (Ssc, SscHelpersClass)
import           Pos.Ssc.Extra               (SscHolder (..))
import           Pos.Txp                     (TxpHolder (..), filterUtxoByAddr,
                                              runUtxoStateT)
import           Pos.Types                   (BlockHeader, ChainDifficulty, difficultyL,
                                              flattenEpochOrSlot, flattenSlotId)
import           Pos.Update                  (ConfirmedProposalState (..))
import           Pos.Update.Context          (UpdateContext (ucUpdateSemaphore))
import           Pos.Util                    (maybeThrow)
import           Pos.Util.Context            (askContext)
import           Pos.Wallet.Context          (ContextHolder, WithWalletContext)
import           Pos.Wallet.KeyStorage       (KeyStorage, MonadKeys)
import           Pos.Wallet.State            (WalletDB)
import qualified Pos.Wallet.State            as WS
import           Pos.Wallet.Web.State        (WalletWebDB (..))
import           Pos.WorkMode                (RawRealMode)

deriving instance MonadBalances m => MonadBalances (WalletWebDB m)

instance MonadIO m => MonadBalances (WalletDB m) where
    getOwnUtxo addr = filterUtxoByAddr addr <$> WS.getUtxo

deriving instance MonadTxHistory m => MonadTxHistory (WalletWebDB m)

-- | Get tx history for Address
instance MonadIO m => MonadTxHistory (WalletDB m) where
    getTxHistory = Tagged $ \addr _ -> do
        chain <- WS.getBestChain
        utxo <- WS.getOldestUtxo
        _ <- fmap (fst . fromMaybe (error "deriveAddrHistory: Nothing")) $
            runMaybeT $ flip runUtxoStateT utxo $
            deriveAddrHistory addr chain
        pure $ error "getTxHistory is not implemented for light wallet"
    saveTx _ = pure ()

class Monad m => MonadBlockchainInfo m where
    networkChainDifficulty :: m (Maybe ChainDifficulty)
    localChainDifficulty :: m ChainDifficulty
    blockchainSlotDuration :: m Millisecond
    connectedPeers :: m Word

    default networkChainDifficulty
        :: (MonadTrans t, MonadBlockchainInfo m', t m' ~ m) => m (Maybe ChainDifficulty)
    networkChainDifficulty = lift networkChainDifficulty

    default localChainDifficulty
        :: (MonadTrans t, MonadBlockchainInfo m', t m' ~ m) => m ChainDifficulty
    localChainDifficulty = lift localChainDifficulty

    default blockchainSlotDuration
        :: (MonadTrans t, MonadBlockchainInfo m', t m' ~ m) => m Millisecond
    blockchainSlotDuration = lift blockchainSlotDuration

    default connectedPeers
        :: (MonadTrans t, MonadBlockchainInfo m', t m' ~ m) => m Word
    connectedPeers = lift connectedPeers

instance MonadBlockchainInfo m => MonadBlockchainInfo (ReaderT r m)
instance MonadBlockchainInfo m => MonadBlockchainInfo (StateT s m)

deriving instance MonadBlockchainInfo m => MonadBlockchainInfo (WalletWebDB m)

-- | Stub instance for lite-wallet
instance MonadBlockchainInfo WalletRealMode where
    networkChainDifficulty = error "notImplemented"
    localChainDifficulty = error "notImplemented"
    blockchainSlotDuration = error "notImplemented"
    connectedPeers = error "notImplemented"

-- | Helpers for avoiding copy-paste
topHeader :: (SscHelpersClass ssc, MonadDB m) => m (BlockHeader ssc)
topHeader = maybeThrow (DBMalformed "No block with tip hash!") =<<
            DB.getBlockHeader =<< GS.getTip

getContextTVar
    :: (Ssc ssc, MonadIO m, PC.WithNodeContext ssc m)
    => (PC.NodeContext ssc -> TVar a)
    -> m a
getContextTVar getter =
    PC.getNodeContext >>=
    atomically . readTVar . getter

getContextTMVar
    :: (Ssc ssc, MonadIO m, PC.WithNodeContext ssc m)
    => (PC.NodeContext ssc -> TMVar a)
    -> m (Maybe a)
getContextTMVar getter =
    PC.getNodeContext >>=
    atomically . tryReadTMVar . getter

downloadHeader
    :: (Ssc ssc, MonadIO m, PC.WithNodeContext ssc m)
    => m (Maybe (BlockHeader ssc))
downloadHeader = getContextTMVar PC.ncProgressHeader

-- | Instance for full-node's ContextHolder
instance forall ssc . SscHelpersClass ssc =>
         MonadBlockchainInfo (RawRealMode ssc) where
    networkChainDifficulty = getContextTVar PC.ncLastKnownHeader >>= \case
        Just lh -> do
            thDiff <- view difficultyL <$> topHeader @ssc
            let lhDiff = lh ^. difficultyL
            return . Just $ max thDiff lhDiff
        Nothing -> runMaybeT $ do
            cSlot <- flattenSlotId <$> MaybeT getCurrentSlot
            th <- lift (topHeader @ssc)
            let hSlot = flattenEpochOrSlot th
            when (hSlot <= cSlot - blkSecurityParam) $
                fail "Local tip is outdated"
            return $ th ^. difficultyL

    localChainDifficulty = downloadHeader >>= \case
        Just dh -> return $ dh ^. difficultyL
        Nothing -> view difficultyL <$> topHeader @ssc

    connectedPeers = fromIntegral . length <$> getContextTVar PC.ncConnectedPeers
    blockchainSlotDuration = getLastKnownSlotDuration

-- | Abstraction over getting update proposals
class Monad m => MonadUpdates m where
    waitForUpdate :: m ConfirmedProposalState
    applyLastUpdate :: m ()

    default waitForUpdate :: (MonadTrans t, MonadUpdates m', t m' ~ m)
                          => m ConfirmedProposalState
    waitForUpdate = lift waitForUpdate

    default applyLastUpdate :: (MonadTrans t, MonadUpdates m', t m' ~ m)
                            => m ()
    applyLastUpdate = lift applyLastUpdate

instance MonadUpdates m => MonadUpdates (ReaderT r m)
instance MonadUpdates m => MonadUpdates (StateT s m)
instance MonadUpdates m => MonadUpdates (KeyStorage m)
instance MonadUpdates m => MonadUpdates (PeerStateHolder m)
instance MonadUpdates m => MonadUpdates (NtpSlotting m)
instance MonadUpdates m => MonadUpdates (SlottingHolder m)

deriving instance MonadUpdates m => MonadUpdates (TxpHolder __ m)
deriving instance MonadUpdates m => MonadUpdates (SscHolder ssc m)
deriving instance MonadUpdates m => MonadUpdates (DelegationT m)
deriving instance MonadUpdates m => MonadUpdates (WalletWebDB m)

-- | Dummy instance for lite-wallet
instance MonadIO m => MonadUpdates (WalletDB m) where
    waitForUpdate = error "notImplemented"
    applyLastUpdate = pure ()

-- | Instance for full node
instance (Ssc ssc, MonadIO m, WithLogger m) =>
         MonadUpdates (PC.ContextHolder ssc m) where
    waitForUpdate = liftIO . takeMVar =<<
                        askContext @UpdateContext ucUpdateSemaphore
    applyLastUpdate = triggerShutdown

---------------------------------------------------------------
-- Composite restrictions
---------------------------------------------------------------

type WalletMode ssc m
    = ( TxMode ssc m
      , MonadKeys m
      , MonadBlockchainInfo m
      , MonadUpdates m
      , WithWalletContext m
      , WithPeerState m
      )

---------------------------------------------------------------
-- Implementations of 'WalletMode'
---------------------------------------------------------------

type WalletRealMode = PeerStateHolder
                      (KeyStorage
                       (WalletDB
                        (ContextHolder
                         (LoggerNameBox
                          Production
                           ))))
