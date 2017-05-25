{-# LANGUAGE TypeFamilies #-}

-- | Extra type classes used for explorer's toil.

module Pos.Explorer.Txp.Toil.Class
       ( MonadTxExtra (..)
       , MonadTxExtraRead (..)
       ) where

import           Universum

import           Control.Lens                (at, (%=), (.=))
import           Control.Monad.Trans.Class   (MonadTrans)

import           Pos.Core                    (Address, Coin)
import           Pos.DB.Class                (MonadDBPure)
import           Pos.Explorer.Core           (AddrHistory, TxExtra)
import qualified Pos.Explorer.DB             as DB
import           Pos.Explorer.Txp.Toil.Types (ExplorerExtra, eeAddrBalances,
                                              eeAddrHistories, eeLocalTxsExtra)
import           Pos.Txp.Core                (TxId)
import           Pos.Txp.Toil                (DBTxp, ToilT, tmExtra)
import           Pos.Util                    (ether)
import qualified Pos.Util.Modifier           as MM

class Monad m => MonadTxExtraRead m where
    getTxExtra :: TxId -> m (Maybe TxExtra)
    getAddrHistory :: Address -> m AddrHistory
    getAddrBalance :: Address -> m (Maybe Coin)

    default getTxExtra
        :: (MonadTrans t, MonadTxExtraRead m', t m' ~ m) => TxId -> m (Maybe TxExtra)
    getTxExtra = lift . getTxExtra

    default getAddrHistory
        :: (MonadTrans t, MonadTxExtraRead m', t m' ~ m) => Address -> m AddrHistory
    getAddrHistory = lift . getAddrHistory

    default getAddrBalance
        :: (MonadTrans t, MonadTxExtraRead m', t m' ~ m) => Address -> m (Maybe Coin)
    getAddrBalance = lift . getAddrBalance

instance {-# OVERLAPPABLE #-}
    (MonadTxExtraRead m, MonadTrans t, Monad (t m)) =>
        MonadTxExtraRead (t m)

class MonadTxExtraRead m => MonadTxExtra m where
    putTxExtra :: TxId -> TxExtra -> m ()
    delTxExtra :: TxId -> m ()
    updateAddrHistory :: Address -> AddrHistory -> m ()
    putAddrBalance :: Address -> Coin -> m ()
    delAddrBalance :: Address -> m ()

    default putTxExtra
        :: (MonadTrans t, MonadTxExtra m', t m' ~ m) => TxId -> TxExtra -> m ()
    putTxExtra id = lift . putTxExtra id

    default delTxExtra
        :: (MonadTrans t, MonadTxExtra m', t m' ~ m) => TxId -> m ()
    delTxExtra = lift . delTxExtra

    default updateAddrHistory
        :: (MonadTrans t, MonadTxExtra m', t m' ~ m) => Address -> AddrHistory -> m ()
    updateAddrHistory addr = lift . updateAddrHistory addr

    default putAddrBalance
        :: (MonadTrans t, MonadTxExtra m', t m' ~ m) => Address -> Coin -> m ()
    putAddrBalance addr = lift . putAddrBalance addr

    default delAddrBalance
        :: (MonadTrans t, MonadTxExtra m', t m' ~ m) => Address -> m ()
    delAddrBalance = lift . delAddrBalance

instance {-# OVERLAPPABLE #-}
    (MonadTxExtra m, MonadTrans t, Monad (t m)) =>
        MonadTxExtra (t m)

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

----------------------------------------------------------------------------
-- DBTxp instances
----------------------------------------------------------------------------

instance (MonadDBPure m) => MonadTxExtraRead (DBTxp m) where
    getTxExtra = DB.getTxExtra
    getAddrHistory = DB.getAddrHistory
    getAddrBalance = DB.getAddrBalance
