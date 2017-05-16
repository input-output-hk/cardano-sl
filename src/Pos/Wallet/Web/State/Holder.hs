{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.Web.State.Holder
       ( WalletWebDB
       , runWalletWebDB
       , getWalletState
       ) where

import qualified Ether
import           Pos.Wallet.Web.State.State (WalletState, getWalletWebState)
import           Universum

-- | Holder for web wallet data
type WalletWebDB = Ether.ReaderT' WalletState

-- | Execute `WalletWebDB` action with given `WalletState`
runWalletWebDB :: WalletState -> WalletWebDB m a -> m a
runWalletWebDB = flip $ Ether.runReaderT

getWalletState :: Monad m => WalletWebDB m WalletState
getWalletState = getWalletWebState
