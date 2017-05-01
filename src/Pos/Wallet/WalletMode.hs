{-# LANGUAGE CPP                 #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | 'WalletMode' constraint. Like `WorkMode`, but for wallet.

module Pos.Wallet.WalletMode
       ( MonadBalances (..)
       , MonadTxHistory (..)
       , MonadBlockchainInfo (..)
       , MonadUpdates (..)
       , WalletMode
       , WalletRealMode
       , WalletStaticPeersMode
       , BIRRContext
       , runBIRRContext
       , UPDContext
       , runUPDContext
       ) where

import           Universum

import           Control.Concurrent.STM      (TMVar, tryReadTMVar)
import           Control.Monad.Trans         (MonadTrans)
import           Control.Monad.Trans.Maybe   (MaybeT (..))
import           Data.Coerce                 (coerce)
import           Data.Tagged                 (Tagged (..))
import           Data.Time.Units             (Millisecond)
import qualified Ether
import           Mockable                    (Production)
import           Pos.Reporting.MemState      (ReportingContextT)
import           System.Wlog                 (LoggerNameBox, WithLogger)

import           Pos.Client.Txp.Balances     (MonadBalances (..), getBalanceFromUtxo)
import           Pos.Client.Txp.History      (MonadTxHistory (..), deriveAddrHistory)
import           Pos.Communication           (TxMode)
import           Pos.Communication.PeerState (PeerStateHolder, WithPeerState)
import           Pos.Constants               (blkSecurityParam)
import qualified Pos.Context                 as PC
import           Pos.DB                      (MonadDB)
import qualified Pos.DB.Block                as DB
import           Pos.DB.Error                (DBError (..))
import qualified Pos.DB.GState               as GS
import           Pos.Discovery               (DiscoveryConstT, DiscoveryKademliaT,
                                              MonadDiscovery)
import           Pos.Shutdown                (MonadShutdownMem, triggerShutdown)
import           Pos.Slotting                (MonadSlots (..), getLastKnownSlotDuration)
import           Pos.Ssc.Class               (Ssc, SscHelpersClass)
import           Pos.Txp                     (filterUtxoByAddr, runUtxoStateT)
import           Pos.Types                   (BlockHeader, ChainDifficulty, difficultyL,
                                              flattenEpochOrSlot, flattenSlotId)
import           Pos.Update                  (ConfirmedProposalState (..))
import           Pos.Update.Context          (UpdateContext (ucUpdateSemaphore))
import           Pos.Util                    (maybeThrow)
import           Pos.Wallet.KeyStorage       (KeyStorage, MonadKeys)
import           Pos.Wallet.State            (WalletDB)
import qualified Pos.Wallet.State            as WS

instance MonadIO m => MonadBalances (WalletDB m) where
    getOwnUtxo addr = filterUtxoByAddr addr <$> WS.getUtxo
    getBalance = getBalanceFromUtxo

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

instance {-# OVERLAPPABLE #-}
    (MonadBlockchainInfo m, MonadTrans t, Monad (t m)) =>
        MonadBlockchainInfo (t m)

-- | Stub instance for lite-wallet
instance MonadBlockchainInfo RawWalletMode where
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

data BIRRContextTag

type BIRRContext = Ether.TaggedTrans BIRRContextTag Ether.IdentityT

runBIRRContext :: BIRRContext m a -> m a
runBIRRContext = coerce

-- | Instance for full-node's ContextHolder
instance
    ( SscHelpersClass ssc
    , t ~ Ether.IdentityT
    , PC.WithNodeContext ssc m
    , MonadIO m
    , MonadDB m
    , MonadSlots m
    ) => MonadBlockchainInfo (Ether.TaggedTrans BIRRContextTag t m)
  where
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

instance {-# OVERLAPPABLE #-}
    (MonadUpdates m, MonadTrans t, Monad (t m)) =>
        MonadUpdates (t m)

-- | Dummy instance for lite-wallet
instance MonadIO m => MonadUpdates (WalletDB m) where
    waitForUpdate = error "notImplemented"
    applyLastUpdate = pure ()

data UPDContextTag

type UPDContext = Ether.TaggedTrans UPDContextTag Ether.IdentityT

runUPDContext :: UPDContext m a -> m a
runUPDContext = coerce

-- | Instance for full node
instance
    ( Ssc ssc
    , MonadIO m
    , WithLogger m
    , PC.WithNodeContext ssc m
    , t ~ Ether.IdentityT
    , MonadShutdownMem m
    , Ether.MonadReader' UpdateContext m
    ) => MonadUpdates (Ether.TaggedTrans UPDContextTag t m)
  where
    waitForUpdate = takeMVar =<< Ether.asks' ucUpdateSemaphore
    applyLastUpdate = triggerShutdown

---------------------------------------------------------------
-- Composite restrictions
---------------------------------------------------------------

type WalletMode ssc m
    = ( TxMode ssc m
      , MonadKeys m
      , MonadBlockchainInfo m
      , MonadUpdates m
      , WithPeerState m
      , MonadDiscovery m
      )

---------------------------------------------------------------
-- Implementations of 'WalletMode'
---------------------------------------------------------------

type RawWalletMode =
    PeerStateHolder (
    KeyStorage (
    WalletDB (
    ReportingContextT (
    LoggerNameBox (
    Production
    )))))

type WalletRealMode = DiscoveryKademliaT RawWalletMode
type WalletStaticPeersMode = DiscoveryConstT RawWalletMode
