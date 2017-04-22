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
       , getBestChain

       , getUtxo
       , getOldestUtxo
       , getTxHistory

       , getSlotDuration
       , getMaxBlockSize
       ) where

import           Data.Acid                   (EventResult, EventState, QueryEvent)
import           Data.Time.Units             (Millisecond)
import           Pos.Communication.PeerState (PeerStateHolder)
import           Serokell.Data.Memory.Units  (Byte)
import           Universum

import           Pos.Txp                     (Tx, Utxo)
import           Pos.Types                   (HeaderHash)

import           Pos.Wallet.State.Acidic     (WalletState, closeState, openMemState,
                                              openState)
import           Pos.Wallet.State.Acidic     as A
import           Pos.Wallet.State.Storage    (Block', Storage)

-- | MonadWalletDB stands for monad which is able to get web wallet state
class Monad m => MonadWalletDB m where
    getWalletState :: m WalletState

-- | Instances for common transformers
instance MonadWalletDB m => MonadWalletDB (ReaderT r m) where
    getWalletState = lift getWalletState

instance MonadWalletDB m => MonadWalletDB (StateT s m) where
    getWalletState = lift getWalletState

instance MonadWalletDB m => MonadWalletDB (PeerStateHolder m) where
    getWalletState = lift getWalletState

-- | Constraint for working with web wallet DB
type WalletModeDB m = (MonadWalletDB m, MonadIO m)

queryDisk
    :: (EventState event ~ Storage, QueryEvent event, WalletModeDB m)
    => event -> m (EventResult event)
queryDisk e = getWalletState >>= flip A.query e

-- updateDisk
--     :: (EventState event ~ Storage, UpdateEvent event, WalletModeDB m)
--     => event -> m (EventResult event)
-- updateDisk e = getWalletState >>= flip A.update e

getBlock :: WalletModeDB m => HeaderHash -> m (Maybe Block')
getBlock = queryDisk . A.GetBlock

getBestChain :: WalletModeDB m => m [Block']
getBestChain = queryDisk A.GetBestChain

getUtxo :: WalletModeDB m => m Utxo
getUtxo = queryDisk A.GetUtxo

getOldestUtxo :: WalletModeDB m => m Utxo
getOldestUtxo = queryDisk A.GetOldestUtxo

getTxHistory :: WalletModeDB m => m [Tx]
getTxHistory = queryDisk A.GetTxHistory

getSlotDuration :: WalletModeDB m => m Millisecond
getSlotDuration = queryDisk A.GetSlotDuration

getMaxBlockSize :: WalletModeDB m => m Byte
getMaxBlockSize = queryDisk A.GetMaxBlockSize
