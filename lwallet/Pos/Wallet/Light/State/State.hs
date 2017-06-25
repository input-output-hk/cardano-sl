{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.Light.State.State
       ( WalletState
       , getWalletState
       , MonadWalletDB
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
       ) where

import           Universum

import           Data.Acid                      (EventResult, EventState, QueryEvent)
import           EtherCompat

import           Pos.Txp                        (Tx, Utxo)
import           Pos.Types                      (HeaderHash)

import           Pos.Wallet.Light.State.Acidic  (WalletState, closeState, openMemState,
                                                 openState)
import           Pos.Wallet.Light.State.Acidic  as A
import           Pos.Wallet.Light.State.Storage (Block', Storage)

-- | MonadWalletDB stands for monad which is able to get web wallet state
type MonadWalletDB ctx m = MonadCtx ctx WalletState WalletState m

getWalletState :: MonadWalletDB ctx m => m WalletState
getWalletState = askCtx @WalletState

-- | Constraint for working with web wallet DB
type WalletModeDB ctx m = (MonadWalletDB ctx m, MonadIO m)

queryDisk
    :: (EventState event ~ Storage, QueryEvent event, WalletModeDB ctx m)
    => event -> m (EventResult event)
queryDisk e = getWalletState >>= flip A.query e

-- updateDisk
--     :: (EventState event ~ Storage, UpdateEvent event, WalletModeDB m)
--     => event -> m (EventResult event)
-- updateDisk e = getWalletState >>= flip A.update e

getBlock :: WalletModeDB ctx m => HeaderHash -> m (Maybe Block')
getBlock = queryDisk . A.GetBlock

getBestChain :: WalletModeDB ctx m => m [Block']
getBestChain = queryDisk A.GetBestChain

getUtxo :: WalletModeDB ctx m => m Utxo
getUtxo = queryDisk A.GetUtxo

getOldestUtxo :: WalletModeDB ctx m => m Utxo
getOldestUtxo = queryDisk A.GetOldestUtxo

getTxHistory :: WalletModeDB ctx m => m [Tx]
getTxHistory = queryDisk A.GetTxHistory
