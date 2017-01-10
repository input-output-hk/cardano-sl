{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to WalletContext.

module Pos.Wallet.Context.Class
       ( WithWalletContext (..)
       ) where

import           Control.Monad.Trans        (MonadTrans)
import           Universum

import qualified Pos.Context                as PC
import           Pos.Delegation.Class       (DelegationT (..))
import           Pos.Ssc.Extra              (SscHolder (..))
import qualified Pos.Txp.Holder             as Modern
import           Pos.Update                 (USHolder (..))

import           Pos.Wallet.Context.Context (WalletContext, fromNodeCtx)

-- | Class for something that has 'NodeContext' inside.
class Monad m => WithWalletContext m where
    getWalletContext :: m WalletContext
    default getWalletContext :: (MonadTrans t, WithWalletContext m', t m' ~ m) => m WalletContext
    getWalletContext = lift getWalletContext

instance (Monad m, WithWalletContext m) => WithWalletContext (ReaderT a m)
instance (Monad m, WithWalletContext m) => WithWalletContext (StateT a m)

instance Monad m => WithWalletContext (PC.ContextHolder ssc m) where
    getWalletContext = fromNodeCtx <$> PC.getNodeContext

deriving instance (Monad m, WithWalletContext m) => WithWalletContext (Modern.TxpLDHolder ssc m)
deriving instance (Monad m, WithWalletContext m) => WithWalletContext (SscHolder ssc m)
deriving instance (Monad m, WithWalletContext m) => WithWalletContext (DelegationT m)
deriving instance (Monad m, WithWalletContext m) => WithWalletContext (USHolder m)
