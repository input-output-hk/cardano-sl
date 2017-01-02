{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'WalletMode' constraint. Like `WorkMode`, but for wallet.

module Pos.Wallet.WalletMode
       ( MonadBalances (..)
       , MonadTxHistory (..)
       , TxMode
       , WalletMode
       , WalletRealMode
       , SState
       ) where

import           Control.Monad                 (fail)
import           Control.Monad.Loops           (unfoldrM)
import           Control.Monad.Trans           (MonadTrans)
import           Control.Monad.Trans.Maybe     (MaybeT (..))
import           Control.TimeWarp.Rpc          (Dialog, Transfer)
import qualified Data.HashMap.Strict           as HM
import qualified Data.Map                      as M
import           Universum

import           Pos.Communication.Types.State (MutSocketState)
import qualified Pos.Context                   as PC
import           Pos.DB                        (MonadDB)
import qualified Pos.DB                        as DB
import           Pos.DHT.Model                 (DHTPacking)
import           Pos.DHT.Real                  (KademliaDHT)
import           Pos.Ssc.Class.Types           (Ssc)
import           Pos.Ssc.Extra                 (SscHolder (..))
import           Pos.Ssc.GodTossing            (SscGodTossing)
import           Pos.Txp.Class                 (getUtxoView)
import qualified Pos.Txp.Holder                as Modern
import           Pos.Txp.Logic                 (processTx)
import           Pos.Txp.Types                 (UtxoView (..))
import           Pos.Types                     (Address, Coin, Tx, TxAux, TxId, Utxo,
                                                runUtxoStateT, toPair, txOutValue)
import           Pos.Types.Utxo.Functions      (belongsTo, filterUtxoByAddr)
import           Pos.WorkMode                  (MinWorkMode)

import           Pos.Wallet.Context            (ContextHolder, WithWalletContext)
import           Pos.Wallet.KeyStorage         (KeyStorage, MonadKeys)
import           Pos.Wallet.State              (WalletDB)
import qualified Pos.Wallet.State              as WS
import           Pos.Wallet.Tx.Pure            (deriveAddrHistory)

-- | A class which have the methods to get state of address' balance
class Monad m => MonadBalances m where
    getOwnUtxo :: Address -> m Utxo
    getBalance :: Address -> m Coin
    getBalance addr = sum . fmap (txOutValue . fst) <$> getOwnUtxo addr
    -- TODO: add a function to get amount of stake (it's different from
    -- balance because of distributions)

    default getOwnUtxo :: (MonadTrans t, MonadBalances m', t m' ~ m) => Address -> m Utxo
    getOwnUtxo = lift . getOwnUtxo

instance MonadBalances m => MonadBalances (ReaderT r m)
instance MonadBalances m => MonadBalances (StateT s m)
instance MonadBalances m => MonadBalances (KademliaDHT m)
instance MonadBalances m => MonadBalances (KeyStorage m)

deriving instance MonadBalances m => MonadBalances (PC.ContextHolder ssc m)
deriving instance MonadBalances m => MonadBalances (SscHolder ssc m)

-- | Instances of 'MonadBalances' for wallet's and node's DBs
instance MonadIO m => MonadBalances (WalletDB m) where
    getOwnUtxo addr = WS.getUtxo >>= return . filterUtxoByAddr addr

instance (MonadDB ssc m, MonadMask m) => MonadBalances (Modern.TxpLDHolder ssc m) where
    getOwnUtxo addr = do
        utxo <- DB.getFilteredUtxo addr
        updates <- getUtxoView
        let toDel = delUtxo updates
            toAdd = HM.filter (`belongsTo` addr) $ addUtxo updates
            utxo' = foldr (M.delete . toPair) utxo toDel
        return $ HM.foldrWithKey (M.insert . toPair) utxo' toAdd

--deriving instance MonadBalances m => MonadBalances (Modern.TxpLDHolder m)

-- | A class which have methods to get transaction history
class Monad m => MonadTxHistory m where
    getTxHistory :: Address -> m [(TxId, Tx, Bool)]
    saveTx :: (TxId, TxAux) -> m ()

    default getTxHistory :: (MonadTrans t, MonadTxHistory m', t m' ~ m) => Address -> m [(TxId, Tx, Bool)]
    getTxHistory = lift . getTxHistory

    default saveTx :: (MonadTrans t, MonadTxHistory m', t m' ~ m) => (TxId, TxAux) -> m ()
    saveTx = lift . saveTx

instance MonadTxHistory m => MonadTxHistory (ReaderT r m)
instance MonadTxHistory m => MonadTxHistory (StateT s m)
instance MonadTxHistory m => MonadTxHistory (KademliaDHT m)
instance MonadTxHistory m => MonadTxHistory (KeyStorage m)

deriving instance MonadTxHistory m => MonadTxHistory (PC.ContextHolder ssc m)
deriving instance MonadTxHistory m => MonadTxHistory (SscHolder ssc m)

-- | Instances of 'MonadTxHistory' for wallet's and node's DBs

-- | Get tx history for Address
instance MonadIO m => MonadTxHistory (WalletDB m) where
    getTxHistory addr = do
        chain <- WS.getBestChain
        utxo <- WS.getOldestUtxo
        return $ fst . fromMaybe (fail "deriveAddrHistory: Nothing") $
            flip runUtxoStateT utxo $
            deriveAddrHistory addr chain
    saveTx _ = pure ()

-- TODO: make a working instance
instance (Ssc ssc, MonadDB ssc m, MonadThrow m) => MonadTxHistory (Modern.TxpLDHolder ssc m) where
    getTxHistory addr = do
        bot <- DB.getBot
        genUtxo <- filterUtxoByAddr addr <$> DB.getGenUtxo

        -- It's genesis hash at the very bottom already, so we don't look for txs there
        txss <- flip unfoldrM (bot, genUtxo) $ \(h, utxo) -> runMaybeT $ do
            next <- MaybeT $ DB.getNextHash h
            blk <- MaybeT $ DB.getBlock next
            MaybeT $ return $ do
                (txs, utxo') <- flip runUtxoStateT utxo $
                    deriveAddrHistory addr [blk]
                return (txs, (next, utxo'))
        return $ foldr (++) [] txss

    saveTx txw = () <$ processTx txw

--deriving instance MonadTxHistory m => MonadTxHistory (Modern.TxpLDHolder m)

type TxMode ssc m
    = ( MinWorkMode (MutSocketState ssc) m
      , MonadBalances m
      , MonadTxHistory m
      )

type WalletMode ssc m
    = ( TxMode ssc m
      , MonadKeys m
      , WithWalletContext m
      )

---------------------------------------------------------------
-- Implementations of 'WalletMode'
---------------------------------------------------------------
type SState = MutSocketState SscGodTossing

type WalletRealMode = KademliaDHT
                      (KeyStorage
                       (WalletDB
                        (ContextHolder
                         (Dialog DHTPacking (Transfer SState)))))
