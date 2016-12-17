{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to WalletContext.

module Pos.Wallet.Context.Class
       ( WithWalletContext (..)
       ) where

import           Universum

import           Pos.DHT.Model              (DHTResponseT)
import           Pos.DHT.Real               (KademliaDHT)
import           Pos.Wallet.Context.Context (WalletContext)

-- | Class for something that has 'NodeContext' inside.
class WithWalletContext m where
    getWalletContext :: m WalletContext

instance (Monad m, WithWalletContext m) =>
         WithWalletContext (KademliaDHT m) where
    getWalletContext = lift getWalletContext

instance (Monad m, WithWalletContext m) =>
         WithWalletContext (ReaderT a m) where
    getWalletContext = lift getWalletContext

instance (Monad m, WithWalletContext m) =>
         WithWalletContext (StateT a m) where
    getWalletContext = lift getWalletContext

instance (Monad m, WithWalletContext m) =>
         WithWalletContext (DHTResponseT s m) where
    getWalletContext = lift getWalletContext

