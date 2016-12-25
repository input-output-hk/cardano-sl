{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
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

import           Control.Lens                  (over, _1)
import           Control.Monad                 (fail)
import           Control.Monad.Trans           (MonadTrans)
import           Control.TimeWarp.Rpc          (Dialog, Transfer)
#ifdef MODERN
import qualified Data.HashMap.Strict           as HM
#endif
import           Universum

import           Pos.Communication.Types.State (MutSocketState)
import qualified Pos.Context                   as PC
import           Pos.Crypto                    (withHash)
import           Pos.DHT.Model                 (DHTPacking)
import           Pos.DHT.Real                  (KademliaDHT)
import           Pos.Ssc.Extra                 (SscHolder (..), SscLDImpl (..))
import           Pos.Ssc.GodTossing            (SscGodTossing)
import qualified Pos.Txp.Holder                as Modern
#ifdef MODERN
import           Pos.DB                        (MonadDB)
import qualified Pos.DB                        as DB
import           Pos.Txp.Class                 (MonadTxpLD (..), getUtxoView)
import           Pos.Txp.Logic                 (processTx)
import           Pos.Txp.Types                 (MemPool (..), UtxoView (..))
#else
import qualified Data.List.NonEmpty            as NE
import           Pos.Ssc.Class                 (SscConstraint)
import qualified Pos.State                     as St
import           Pos.Txp.Listeners             (processTx)
import           Pos.Txp.LocalData             (MonadTxLD (..), _txLocalTxs)
import           Pos.Types                     (execUtxoStateT)
import           Pos.Wallet.Tx.Pure            (getRelatedTxs)
#endif
import           Pos.Types                     (Address, Coin, Tx, TxAux, TxId, Utxo,
                                                evalUtxoStateT, txOutValue)
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
    getBalance addr = sum . fmap (txOutValue . fst) <$> getOwnUtxo addr
    -- TODO: add a function to get amount of stake (it's different from
    -- balance because of distributions)

    default getOwnUtxo :: MonadTrans t => Address -> t m Utxo
    getOwnUtxo = lift . getOwnUtxo

instance MonadBalances m => MonadBalances (ReaderT r m)
instance MonadBalances m => MonadBalances (StateT s m)
instance MonadBalances m => MonadBalances (KademliaDHT m)
instance MonadBalances m => MonadBalances (KeyStorage m)

deriving instance MonadBalances m => MonadBalances (PC.ContextHolder ssc m)
deriving instance MonadBalances m => MonadBalances (SscLDImpl ssc m)
deriving instance MonadBalances m => MonadBalances (SscHolder ssc m)

-- | Instances of 'MonadBalances' for wallet's and node's DBs
instance MonadIO m => MonadBalances (WalletDB m) where
    getOwnUtxo addr = WS.getUtxo >>= return . filterUtxoByAddr addr

#ifdef MODERN
instance (MonadDB ssc m, MonadMask m) => MonadBalances (Modern.TxpLDHolder ssc m) where
    getOwnUtxo addr = do
        utxo <- DB.getFilteredUtxo addr
        updates <- getUtxoView
        let toDel = delUtxo updates
            toAdd = HM.filter (`belongsTo` addr) $ addUtxo updates
            utxo' = foldr (M.delete . toPair) utxo toDel
        return $ HM.foldrWithKey (M.insert . toPair) utxo' toAdd

deriving instance MonadBalances m => MonadBalances (TxLDImpl m)
#else
instance (SscConstraint ssc, St.MonadDB ssc m, MonadIO m) => MonadBalances (TxLDImpl m) where
    getOwnUtxo addr = do
        utxo <- St.getUtxo
        localTxs <- _txLocalTxs <$> getTxLocalData
        let utxo' = filterUtxoByAddr addr utxo
            wtxs = map (over _1 withHash) . toList $ localTxs
        case execUtxoStateT (getRelatedTxs addr wtxs) utxo' of
            Nothing     -> fail "Inconsistent local txs state!"
            Just utxo'' -> return utxo''

deriving instance MonadBalances m => MonadBalances (Modern.TxpLDHolder ssc m)
#endif

-- | A class which have methods to get transaction history
class Monad m => MonadTxHistory m where
    getTxHistory :: Address -> m [(TxId, Tx, Bool)]
    saveTx :: (TxId, TxAux) -> m ()

    default getTxHistory :: MonadTrans t => Address -> t m [(TxId, Tx, Bool)]
    getTxHistory = lift . getTxHistory

    default saveTx :: MonadTrans t => (TxId, TxAux) -> t m ()
    saveTx = lift . saveTx

instance MonadTxHistory m => MonadTxHistory (ReaderT r m)
instance MonadTxHistory m => MonadTxHistory (StateT s m)
instance MonadTxHistory m => MonadTxHistory (KademliaDHT m)
instance MonadTxHistory m => MonadTxHistory (KeyStorage m)

deriving instance MonadTxHistory m => MonadTxHistory (PC.ContextHolder ssc m)
deriving instance MonadTxHistory m => MonadTxHistory (SscLDImpl ssc m)
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
    saveTx _ = pure ()

-- TODO: make a working instance
#ifdef MODERN
instance (MonadDB ssc m, MonadThrow m) => MonadTxHistory (Modern.TxpLDHolder ssc m) where
    getTxHistory _ = pure []
    saveTx txw = () <$ processTx txw

deriving instance MonadTxHistory m => MonadTxHistory (TxLDImpl m)
#else
instance (SscConstraint ssc, St.MonadDB ssc m, MonadIO m) => MonadTxHistory (TxLDImpl m) where
    getTxHistory addr = do
        chain <- St.getBestChain
        utxo <- St.getOldestUtxo
        return $ fromMaybe (fail "deriveAddrHistory: Nothing") $
            flip evalUtxoStateT utxo $
            deriveAddrHistory addr $ NE.toList chain
    saveTx txw = () <$ processTx txw

deriving instance MonadTxHistory m => MonadTxHistory (Modern.TxpLDHolder ssc m)
#endif

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
