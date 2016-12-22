{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

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
import           Control.Monad.Trans           (MonadTrans)
import           Control.TimeWarp.Rpc          (Dialog, Transfer)
import qualified Data.Map                      as M
import           Universum

import           Pos.Communication.Types.State (MutSocketState)
import qualified Pos.Context                   as PC
import           Pos.Crypto                    (WithHash)
import           Pos.DB                        (DBHolder)
import qualified Pos.DB                        as DB
import           Pos.DHT.Model                 (DHTPacking)
import           Pos.DHT.Real                  (KademliaDHT)
import qualified Pos.Modern.Txp.Holder         as Modern
import           Pos.Ssc.Extra                 (SscHolder (..), SscLDImpl (..))
import           Pos.Ssc.GodTossing            (SscGodTossing)
import           Pos.Types                     (Address, Coin, Tx, Utxo, evalUtxoStateT,
                                                txOutValue)
import           Pos.Types.Utxo.Functions      (filterUtxoByAddr)
import           Pos.WorkMode                  (TxLDImpl (..))
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
    getBalance addr = getOwnUtxo addr >>= return . sum . M.map txOutValue

    default getOwnUtxo :: MonadTrans t => Address -> t m Utxo
    getOwnUtxo = lift . getOwnUtxo

instance MonadBalances m => MonadBalances (ReaderT r m)
instance MonadBalances m => MonadBalances (StateT s m)
instance MonadBalances m => MonadBalances (KademliaDHT m)
instance MonadBalances m => MonadBalances (KeyStorage m)

deriving instance MonadBalances m => MonadBalances (PC.ContextHolder ssc m)
deriving instance MonadBalances m => MonadBalances (TxLDImpl m)
deriving instance MonadBalances m => MonadBalances (SscLDImpl ssc m)
deriving instance MonadBalances m => MonadBalances (Modern.TxpLDHolder ssc m)
deriving instance MonadBalances m => MonadBalances (SscHolder ssc m)

-- | Instances of 'MonadBalances' for wallet's and node's DBs
instance MonadIO m => MonadBalances (WalletDB m) where
    getOwnUtxo addr = WS.getUtxo >>= return . filterUtxoByAddr addr

instance (MonadThrow m, MonadIO m, MonadMask m) => MonadBalances (DBHolder ssc m) where
    getOwnUtxo = DB.getFilteredUtxo

-- | A class which have methods to get transaction history
class Monad m => MonadTxHistory m where
    getTxHistory :: Address -> m [WithHash Tx]
    default getTxHistory :: MonadTrans t => Address -> t m [WithHash Tx]
    getTxHistory = lift . getTxHistory

instance MonadTxHistory m => MonadTxHistory (ReaderT r m)
instance MonadTxHistory m => MonadTxHistory (StateT s m)
instance MonadTxHistory m => MonadTxHistory (KademliaDHT m)
instance MonadTxHistory m => MonadTxHistory (KeyStorage m)

deriving instance MonadTxHistory m => MonadTxHistory (PC.ContextHolder ssc m)
deriving instance MonadTxHistory m => MonadTxHistory (TxLDImpl m)
deriving instance MonadTxHistory m => MonadTxHistory (SscLDImpl ssc m)
deriving instance MonadTxHistory m => MonadTxHistory (Modern.TxpLDHolder ssc m)
deriving instance MonadTxHistory m => MonadTxHistory (SscHolder ssc m)

-- | Instances of 'MonadTxHistory' for wallet's and node's DBs

-- | Get tx history for Address
instance MonadIO m => MonadTxHistory (WalletDB m) where
    getTxHistory addr = do
        chain <- WS.getBestChain
        utxo <- WS.getOldestUtxo
        return $ fromMaybe (fail "deriveAddrHistory: Nothing") $
            flip evalUtxoStateT utxo $
            deriveAddrHistory addr chain

-- TODO: make a working instance
instance MonadIO m => MonadTxHistory (DBHolder ssc m) where
    getTxHistory _ = pure []

type TxMode ssc m
    = ( MinWorkMode (MutSocketState ssc) m
      , MonadBalances m
      )

type WalletMode ssc m
    = ( TxMode ssc m
      , MonadTxHistory m
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
