{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

-- | 'WalletMode' constraint. Like `WorkMode`, but for wallet.

module Pos.Wallet.WalletMode
       ( MonadBalances (..)
       , MonadTxHistory (..)
       , TxHistoryAnswer (..)
       , MonadBlockchainInfo (..)
       , MonadUpdates (..)
       , TxMode
       , WalletMode
       , WalletRealMode
       ) where

import           Control.Concurrent.STM      (TMVar, tryReadTMVar)
import           Control.Monad.Loops         (unfoldrM)
import           Control.Monad.Trans         (MonadTrans)
import           Control.Monad.Trans.Maybe   (MaybeT (..))
import qualified Data.HashMap.Strict         as HM
import qualified Data.Map                    as M
import           Data.Tagged                 (Tagged (..))
import           Data.Time.Units             (Millisecond)
import           Mockable                    (MonadMockable, Production)
import           System.Wlog                 (LoggerNameBox, WithLogger)
import           Universum

import           Pos.Communication.PeerState (PeerStateHolder, WithPeerState)
import           Pos.Constants               (blkSecurityParam)
import qualified Pos.Context                 as PC
import           Pos.Core.Coin               (unsafeIntegerToCoin)
import           Pos.Crypto                  (WithHash (..))
import           Pos.DB                      (MonadDB)
import qualified Pos.DB.Block                as DB
import           Pos.DB.Error                (DBError (..))
import qualified Pos.DB.GState               as GS
import           Pos.DB.Limits               (MonadDBLimits)
import           Pos.Delegation              (DelegationT (..))
import           Pos.DHT.Model               (MonadDHT, getKnownPeers)
import           Pos.DHT.Real                (KademliaDHT (..))
import           Pos.Shutdown                (triggerShutdown)
import           Pos.Slotting                (MonadSlots (..), NtpSlotting,
                                              SlottingHolder, getLastKnownSlotDuration)
import           Pos.Ssc.Class               (Ssc, SscHelpersClass)
import           Pos.Ssc.Extra               (SscHolder (..))
import           Pos.Txp                     (TxAux, TxId, TxOutAux (..), TxpHolder (..),
                                              Utxo, addrBelongsTo, evalUtxoStateT,
                                              filterUtxoByAddr, getMemPool,
                                              getUtxoModifier, runUtxoStateT, txOutValue,
                                              txProcessTransaction, _mpLocalTxs)
import           Pos.Types                   (Address, BlockHeader, ChainDifficulty, Coin,
                                              HeaderHash, difficultyL, flattenEpochOrSlot,
                                              flattenSlotId, prevBlockL, prevBlockL,
                                              sumCoins, sumCoins)
import           Pos.Update                  (ConfirmedProposalState (..), USHolder (..))
import           Pos.Util                    (maybeThrow)
import qualified Pos.Util.Modifier           as MM
import           Pos.WorkMode                (MinWorkMode, RawRealMode)

import           Pos.Wallet.Context          (ContextHolder, WithWalletContext)
import           Pos.Wallet.KeyStorage       (KeyStorage, MonadKeys)
import           Pos.Wallet.State            (WalletDB)
import qualified Pos.Wallet.State            as WS
import           Pos.Wallet.Tx.Pure          (TxHistoryEntry, deriveAddrHistory,
                                              deriveAddrHistoryPartial, getRelatedTxs)
import           Pos.Wallet.Web.State        (WalletWebDB (..))

-- | A class which have the methods to get state of address' balance
class Monad m => MonadBalances m where
    getOwnUtxo :: Address -> m Utxo
    getBalance :: Address -> m Coin
    getBalance addr = unsafeIntegerToCoin . sumCoins .
                      map (txOutValue . toaOut) . toList <$> getOwnUtxo addr
    -- TODO: add a function to get amount of stake (it's different from
    -- balance because of distributions)

    default getOwnUtxo :: (MonadTrans t, MonadBalances m', t m' ~ m) => Address -> m Utxo
    getOwnUtxo = lift . getOwnUtxo

instance MonadBalances m => MonadBalances (ReaderT r m)
instance MonadBalances m => MonadBalances (StateT s m)
instance MonadBalances m => MonadBalances (KademliaDHT m)
instance MonadBalances m => MonadBalances (KeyStorage m)
instance MonadBalances m => MonadBalances (PeerStateHolder m)
instance MonadBalances m => MonadBalances (NtpSlotting m)
instance MonadBalances m => MonadBalances (SlottingHolder m)

deriving instance MonadBalances m => MonadBalances (PC.ContextHolder ssc m)
deriving instance MonadBalances m => MonadBalances (SscHolder ssc m)
deriving instance MonadBalances m => MonadBalances (DelegationT m)
deriving instance MonadBalances m => MonadBalances (USHolder m)
deriving instance MonadBalances m => MonadBalances (WalletWebDB m)

-- | Instances of 'MonadBalances' for wallet's and node's DBs
instance MonadIO m => MonadBalances (WalletDB m) where
    getOwnUtxo addr = WS.getUtxo >>= return . filterUtxoByAddr addr

instance (MonadDB m, MonadMask m) => MonadBalances (TxpHolder m) where
    getOwnUtxo addr = do
        utxo <- GS.getFilteredUtxo addr
        updates <- getUtxoModifier
        let toDel = MM.deletions updates
            toAdd = HM.filter (`addrBelongsTo` addr) $ MM.insertionsMap updates
            utxo' = foldr M.delete utxo toDel
        return $ HM.foldrWithKey M.insert utxo' toAdd

data TxHistoryAnswer = TxHistoryAnswer
    { taLastCachedHash :: HeaderHash
    , taCachedNum      :: Int
    , taCachedUtxo     :: Utxo
    , taHistory        :: [TxHistoryEntry]
    } deriving (Show)

-- | A class which have methods to get transaction history
class Monad m => MonadTxHistory m where
    getTxHistory
        :: SscHelpersClass ssc
        => Tagged ssc (Address -> Maybe (HeaderHash, Utxo) -> m TxHistoryAnswer)
    saveTx :: (TxId, TxAux) -> m ()

    default getTxHistory
        :: (SscHelpersClass ssc, MonadTrans t, MonadTxHistory m', t m' ~ m)
        => Tagged ssc (Address -> Maybe (HeaderHash, Utxo) -> m TxHistoryAnswer)
    getTxHistory = fmap (fmap lift) <$> getTxHistory

    default saveTx :: (MonadTrans t, MonadTxHistory m', t m' ~ m) => (TxId, TxAux) -> m ()
    saveTx = lift . saveTx

instance MonadTxHistory m => MonadTxHistory (ReaderT r m)
instance MonadTxHistory m => MonadTxHistory (StateT s m)
instance MonadTxHistory m => MonadTxHistory (KademliaDHT m)
instance MonadTxHistory m => MonadTxHistory (KeyStorage m)
instance MonadTxHistory m => MonadTxHistory (PeerStateHolder m)
instance MonadTxHistory m => MonadTxHistory (NtpSlotting m)
instance MonadTxHistory m => MonadTxHistory (SlottingHolder m)

deriving instance MonadTxHistory m => MonadTxHistory (PC.ContextHolder ssc m)
deriving instance MonadTxHistory m => MonadTxHistory (SscHolder ssc m)
deriving instance MonadTxHistory m => MonadTxHistory (DelegationT m)
deriving instance MonadTxHistory m => MonadTxHistory (USHolder m)
deriving instance MonadTxHistory m => MonadTxHistory (WalletWebDB m)

-- | Instances of 'MonadTxHistory' for wallet's and node's DBs

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

instance ( MonadDB m
         , MonadThrow m
         , WithLogger m
#ifdef WITH_EXPLORER
         , MonadSlots m
#endif
         , PC.WithNodeContext s m) =>
         MonadTxHistory (TxpHolder m) where
    getTxHistory :: forall ssc. SscHelpersClass ssc
                 => Tagged ssc (Address -> Maybe (HeaderHash, Utxo) -> TxpHolder m TxHistoryAnswer)
    getTxHistory = Tagged $ \addr mInit -> do
        tip <- GS.getTip

        let getGenUtxo = filterUtxoByAddr addr . PC.ncGenesisUtxo <$> PC.getNodeContext
        (bot, genUtxo) <- maybe ((,) <$> GS.getBot <*> getGenUtxo) pure mInit

        -- Getting list of all hashes in main blockchain (excluding bottom block - it's genesis anyway)
        hashList <- flip unfoldrM tip $ \h ->
            if h == bot
            then return Nothing
            else do
                header <- DB.getBlockHeader @ssc h >>=
                    maybeThrow (DBMalformed "Best blockchain is non-continuous")
                let prev = header ^. prevBlockL
                return $ Just (h, prev)

        -- Determine last block which txs should be cached
        let cachedHashes = drop blkSecurityParam hashList
            nonCachedHashes = take blkSecurityParam hashList

        let blockFetcher h txs = do
                blk <- lift . lift $ DB.getBlock @ssc h >>=
                       maybeThrow (DBMalformed "A block mysteriously disappeared!")
                deriveAddrHistoryPartial txs addr [blk]
            localFetcher blkTxs = do
                let mp (txid, (tx, txw, txd)) = (WithHash tx txid, txw, txd)
                ltxs <- HM.toList . _mpLocalTxs <$> lift (lift getMemPool)
                txs <- getRelatedTxs addr $ map mp ltxs
                return $ txs ++ blkTxs

        mres <- runMaybeT $ do
            (cachedTxs, cachedUtxo) <- runUtxoStateT
                (foldrM blockFetcher [] cachedHashes) genUtxo

            result <- evalUtxoStateT
                (foldrM blockFetcher cachedTxs nonCachedHashes >>= localFetcher)
                cachedUtxo

            let lastCachedHash = maybe bot identity $ head cachedHashes
            return $ TxHistoryAnswer lastCachedHash (length cachedTxs) cachedUtxo result

        maybe (error "deriveAddrHistory: Nothing") pure mres

    saveTx txw = () <$ runExceptT (txProcessTransaction txw)

--deriving instance MonadTxHistory m => MonadTxHistory (Modern.TxpLDHolder m)

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

    connectedPeers = fromIntegral . length <$> getKnownPeers
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
instance MonadUpdates m => MonadUpdates (KademliaDHT m)
instance MonadUpdates m => MonadUpdates (KeyStorage m)
instance MonadUpdates m => MonadUpdates (PeerStateHolder m)
instance MonadUpdates m => MonadUpdates (NtpSlotting m)
instance MonadUpdates m => MonadUpdates (SlottingHolder m)

deriving instance MonadUpdates m => MonadUpdates (TxpHolder m)
deriving instance MonadUpdates m => MonadUpdates (SscHolder ssc m)
deriving instance MonadUpdates m => MonadUpdates (DelegationT m)
deriving instance MonadUpdates m => MonadUpdates (USHolder m)
deriving instance MonadUpdates m => MonadUpdates (WalletWebDB m)

-- | Dummy instance for lite-wallet
instance MonadIO m => MonadUpdates (WalletDB m) where
    waitForUpdate = error "notImplemented"
    applyLastUpdate = pure ()

-- | Instance for full node
instance (Ssc ssc, MonadIO m, WithLogger m) =>
         MonadUpdates (PC.ContextHolder ssc m) where
    waitForUpdate = liftIO . takeMVar . PC.ncUpdateSemaphore =<< PC.getNodeContext
    applyLastUpdate = triggerShutdown

---------------------------------------------------------------
-- Composite restrictions
---------------------------------------------------------------

type TxMode ssc m
    = ( MinWorkMode m
      , MonadBalances m
      , MonadTxHistory m
      , MonadMockable m
      , MonadMask m
      , MonadDBLimits m
      )

type WalletMode ssc m
    = ( TxMode ssc m
      , MonadKeys m
      , MonadBlockchainInfo m
      , MonadUpdates m
      , WithWalletContext m
      , MonadDHT m
      , WithPeerState m
      )

---------------------------------------------------------------
-- Implementations of 'WalletMode'
---------------------------------------------------------------

type WalletRealMode = PeerStateHolder (KademliaDHT
                      (KeyStorage
                       (WalletDB
                        (ContextHolder
                         (LoggerNameBox
                          Production
                           )))))
