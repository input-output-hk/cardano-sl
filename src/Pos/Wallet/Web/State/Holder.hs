{-# LANGUAGE TypeFamilies #-}

module Pos.Wallet.Web.State.Holder
       ( WalletWebDB
       , runWalletWebDB
       , getWalletState
       ) where

import qualified Control.Monad.Ether        as Ether.E
import           Pos.Wallet.Web.State.State (WalletState, WalletWebDBTag,
                                             getWalletWebState)
import           Universum

-- | Holder for web wallet data
type WalletWebDB = Ether.E.ReaderT WalletWebDBTag WalletState

-- | Execute `WalletWebDB` action with given `WalletState`
runWalletWebDB :: WalletState -> WalletWebDB m a -> m a
runWalletWebDB = flip $ Ether.E.runReaderT (Proxy @WalletWebDBTag)

getWalletState :: Monad m => WalletWebDB m WalletState
getWalletState = getWalletWebState
