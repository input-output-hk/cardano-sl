{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to WalletContext.

module Pos.Wallet.Context.Class
       ( WithWalletContext (..)
       ) where

import           Control.Monad.Trans        (MonadTrans)
import           Universum

import qualified Pos.Context                as PC

import           Pos.Wallet.Context.Context (WalletContext (..), fromNodeCtx)

-- | Class for something that has 'NodeContext' inside.
class Monad m => WithWalletContext m where
    getWalletContext :: m WalletContext
    default getWalletContext :: (MonadTrans t, WithWalletContext m', t m' ~ m) => m WalletContext
    getWalletContext = lift getWalletContext

instance {-# OVERLAPPABLE #-}
  (WithWalletContext m, MonadTrans t, Monad (t m)) =>
  WithWalletContext (t m)

instance Monad m => WithWalletContext (PC.ContextHolder ssc m) where
    getWalletContext = fromNodeCtx <$> PC.getNodeContext
