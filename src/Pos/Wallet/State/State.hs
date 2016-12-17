{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

module Pos.Wallet.State.State
       ( WalletState
       , MonadWalletDB (..)
       , WalletModeDB
       , openState
       , openMemState
       , closeState

       -- * Getters
       , getBlock

       , getUtxo
       , getTxHistory
       ) where

import           Data.Acid                (EventResult, EventState, QueryEvent,
                                           UpdateEvent)
import           Universum

import           Pos.Types                (Tx, Utxo)
import           Pos.Wallet.State.Acidic  (WalletState, closeState, openMemState,
                                           openState)
import           Pos.Wallet.State.Acidic  as A
import           Pos.Wallet.State.Storage (Block', HeaderHash', Storage)

-- | MonadWalletDB stands for monad which is able to get web wallet state
class Monad m => MonadWalletDB m where
    getWalletState :: m WalletState

-- | Instances for common transformers
instance MonadWalletDB m => MonadWalletDB (ReaderT r m) where
    getWalletState = lift getWalletState

instance MonadWalletDB m => MonadWalletDB (StateT s m) where
    getWalletState = lift getWalletState

-- | Constraint for working with web wallet DB
type WalletModeDB m = (MonadWalletDB m, MonadIO m)

queryDisk
    :: (EventState event ~ Storage, QueryEvent event, WalletModeDB m)
    => event -> m (EventResult event)
queryDisk e = getWalletState >>= flip A.query e

updateDisk
    :: (EventState event ~ Storage, UpdateEvent event, WalletModeDB m)
    => event -> m (EventResult event)
updateDisk e = getWalletState >>= flip A.update e

getBlock :: WalletModeDB m => HeaderHash' -> m (Maybe Block')
getBlock = queryDisk . A.GetBlock

getUtxo :: WalletModeDB m => m Utxo
getUtxo = queryDisk A.GetUtxo

getTxHistory :: WalletModeDB m => m [Tx]
getTxHistory = queryDisk A.GetTxHistory
