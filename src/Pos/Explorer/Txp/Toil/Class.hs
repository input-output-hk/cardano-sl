{-# LANGUAGE TypeFamilies #-}

-- | Extra type classes used for explorer's toil.

module Pos.Explorer.Txp.Toil.Class
       ( MonadTxExtra (..)
       , MonadTxExtraRead (..)
       ) where

import           Universum

import           Control.Lens                 (at, (%=), (.=))
import qualified Control.Monad.Ether.Implicit as Ether
import           Control.Monad.Trans.Class    (MonadTrans)

import           Pos.Core                     (Address)
import           Pos.DB.Class                 (MonadDB)
import           Pos.Explorer.Core            (AddrHistory, TxExtra)
import qualified Pos.Explorer.DB              as DB
import           Pos.Explorer.Txp.Toil.Types  (ExplorerExtra, eeAddrHistories,
                                               eeLocalTxsExtra)
import           Pos.Txp.Core                 (TxId)
import           Pos.Txp.Toil                 (DBTxp, ToilT, UtxoReaderT, tmExtra)
import           Pos.Util                     (ether)
import qualified Pos.Util.Modifier            as MM

class Monad m => MonadTxExtraRead m where
    getTxExtra :: TxId -> m (Maybe TxExtra)
    getAddrHistory :: Address -> m AddrHistory

    default getTxExtra
        :: (MonadTrans t, MonadTxExtraRead m', t m' ~ m) => TxId -> m (Maybe TxExtra)
    getTxExtra = lift . getTxExtra

    default getAddrHistory
        :: (MonadTrans t, MonadTxExtraRead m', t m' ~ m) => Address -> m AddrHistory
    getAddrHistory = lift . getAddrHistory

instance MonadTxExtraRead m => MonadTxExtraRead (ReaderT s m)
instance MonadTxExtraRead m => MonadTxExtraRead (StateT s m)
instance MonadTxExtraRead m => MonadTxExtraRead (ExceptT s m)
instance MonadTxExtraRead m => MonadTxExtraRead (Ether.ReaderT s m)

class MonadTxExtraRead m => MonadTxExtra m where
    putTxExtra :: TxId -> TxExtra -> m ()
    delTxExtra :: TxId -> m ()
    updateAddrHistory :: Address -> AddrHistory -> m ()

    default putTxExtra
        :: (MonadTrans t, MonadTxExtra m', t m' ~ m) => TxId -> TxExtra -> m ()
    putTxExtra id = lift . putTxExtra id

    default delTxExtra
        :: (MonadTrans t, MonadTxExtra m', t m' ~ m) => TxId -> m ()
    delTxExtra = lift . delTxExtra

    default updateAddrHistory
        :: (MonadTrans t, MonadTxExtra m', t m' ~ m) => Address -> AddrHistory -> m ()
    updateAddrHistory addr = lift . updateAddrHistory addr

instance MonadTxExtra m => MonadTxExtra (ReaderT s m)
instance MonadTxExtra m => MonadTxExtra (StateT s m)
instance MonadTxExtra m => MonadTxExtra (ExceptT s m)
instance MonadTxExtra m => MonadTxExtra (UtxoReaderT m)

----------------------------------------------------------------------------
-- ToilT instances
----------------------------------------------------------------------------

instance MonadTxExtraRead m => MonadTxExtraRead (ToilT ExplorerExtra m) where
    getTxExtra id = ether $
        MM.lookupM getTxExtra id =<< use (tmExtra . eeLocalTxsExtra)
    getAddrHistory addr = ether $
        maybe (getAddrHistory addr) pure =<< use (tmExtra . eeAddrHistories . at addr)

instance MonadTxExtraRead m => MonadTxExtra (ToilT ExplorerExtra m) where
    putTxExtra id extra = ether $
        tmExtra . eeLocalTxsExtra %= MM.insert id extra
    delTxExtra id = ether $
        tmExtra . eeLocalTxsExtra %= MM.delete id
    updateAddrHistory addr hist = ether $
        tmExtra . eeAddrHistories . at addr .= Just hist

----------------------------------------------------------------------------
-- DBTxp instances
----------------------------------------------------------------------------

instance (Monad m, MonadDB m) => MonadTxExtraRead (DBTxp m) where
    getTxExtra = DB.getTxExtra
    getAddrHistory = DB.getAddrHistory
