{-# LANGUAGE TypeFamilies #-}

-- | Extra type classes used for explorer's toil.

module Pos.Explorer.Txp.Toil.Class
       ( MonadTxExtra (..)
       , MonadTxExtraRead (..)
       ) where

import           Universum

import           Control.Lens (at, (%=), (.=))
import           Control.Monad.Trans.Class (MonadTrans)

import           Pos.Core (Address, Coin, TxId)
import           Pos.DB.Class (MonadDBRead)
import           Pos.Explorer.Core (AddrHistory, TxExtra)
import qualified Pos.Explorer.DB as DB
import           Pos.Explorer.Txp.Toil.Types (ExplorerExtra, eeAddrBalances, eeAddrHistories,
                                              eeLocalTxsExtra, eeNewUtxoSum)
import           Pos.Txp.Toil (DBToil, ToilT, tmExtra)
import           Pos.Util (ether)
import qualified Pos.Util.Modifier as MM

class Monad m => MonadTxExtraRead m where
    getTxExtra :: TxId -> m (Maybe TxExtra)
    getAddrHistory :: Address -> m AddrHistory
    getAddrBalance :: Address -> m (Maybe Coin)
    getUtxoSum :: m Integer

instance {-# OVERLAPPABLE #-}
    (MonadTxExtraRead m, MonadTrans t, Monad (t m)) =>
        MonadTxExtraRead (t m)
  where
    getTxExtra = lift . getTxExtra
    getAddrHistory = lift . getAddrHistory
    getAddrBalance = lift . getAddrBalance
    getUtxoSum = lift getUtxoSum


class MonadTxExtraRead m => MonadTxExtra m where
    putTxExtra :: TxId -> TxExtra -> m ()
    delTxExtra :: TxId -> m ()
    updateAddrHistory :: Address -> AddrHistory -> m ()
    putAddrBalance :: Address -> Coin -> m ()
    delAddrBalance :: Address -> m ()
    putUtxoSum :: Integer -> m ()

instance {-# OVERLAPPABLE #-}
    (MonadTxExtra m, MonadTrans t, Monad (t m)) =>
        MonadTxExtra (t m)
  where
    putTxExtra id = lift . putTxExtra id
    delTxExtra = lift . delTxExtra
    updateAddrHistory addr = lift . updateAddrHistory addr
    putAddrBalance addr = lift . putAddrBalance addr
    delAddrBalance = lift . delAddrBalance
    putUtxoSum = lift . putUtxoSum

----------------------------------------------------------------------------
-- ToilT instances
----------------------------------------------------------------------------

instance MonadTxExtraRead m => MonadTxExtraRead (ToilT ExplorerExtra m) where
    getTxExtra id = ether $
        MM.lookupM getTxExtra id =<< use (tmExtra . eeLocalTxsExtra)
    getAddrHistory addr = ether $
        maybe (getAddrHistory addr) pure =<< use (tmExtra . eeAddrHistories . at addr)
    getAddrBalance addr = ether $
        MM.lookupM getAddrBalance addr =<< use (tmExtra . eeAddrBalances)
    getUtxoSum = ether $ do
        valMonad <- getUtxoSum
        valModifier <- use (tmExtra . eeNewUtxoSum)
        pure $ fromMaybe valMonad valModifier

instance MonadTxExtraRead m => MonadTxExtra (ToilT ExplorerExtra m) where
    putTxExtra id extra = ether $
        tmExtra . eeLocalTxsExtra %= MM.insert id extra
    delTxExtra id = ether $
        tmExtra . eeLocalTxsExtra %= MM.delete id
    updateAddrHistory addr hist = ether $
        tmExtra . eeAddrHistories . at addr .= Just hist
    putAddrBalance addr coin = ether $
        tmExtra . eeAddrBalances %= MM.insert addr coin
    delAddrBalance addr = ether $
        tmExtra . eeAddrBalances %= MM.delete addr
    putUtxoSum utxoSum = ether $ do
        tmExtra . eeNewUtxoSum .= Just utxoSum

----------------------------------------------------------------------------
-- DBToil instances
----------------------------------------------------------------------------

instance (MonadDBRead m) => MonadTxExtraRead (DBToil m) where
    getTxExtra = DB.getTxExtra
    getAddrHistory = DB.getAddrHistory
    getAddrBalance = DB.getAddrBalance
    getUtxoSum = DB.getUtxoSum
