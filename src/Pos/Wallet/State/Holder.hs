{-# LANGUAGE TypeFamilies         #-}

module Pos.Wallet.State.Holder
       ( WalletDB
       , runWalletDB
       ) where

import qualified Control.Monad.Ether.Implicit as Ether
import           Pos.Wallet.State.State       (WalletState)
import           Universum

-- | Holder for web wallet data
type WalletDB = Ether.ReaderT WalletState

-- | Execute `WalletDB` action with given `WalletState`
runWalletDB :: WalletState -> WalletDB m a -> m a
runWalletDB = flip Ether.runReaderT
