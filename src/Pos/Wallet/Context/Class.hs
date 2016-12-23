{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to WalletContext.

module Pos.Wallet.Context.Class
       ( WithWalletContext (..)
       ) where

import           Control.Monad.Trans        (MonadTrans)
import           Universum

import qualified Pos.Context                as PC
import           Pos.DHT.Model              (DHTResponseT)
import           Pos.DHT.Real               (KademliaDHT)
import           Pos.Ssc.Extra              (SscHolder (..), SscLDImpl (..))
import qualified Pos.Txp.Holder             as Modern
import           Pos.WorkMode               (TxLDImpl (..))

import           Pos.Wallet.Context.Context (WalletContext, fromNodeCtx)

-- | Class for something that has 'NodeContext' inside.
class WithWalletContext m where
    getWalletContext :: m WalletContext
    default getWalletContext :: (MonadTrans t, Monad m) => t m WalletContext
    getWalletContext = lift getWalletContext

instance (Monad m, WithWalletContext m) => WithWalletContext (KademliaDHT m)
instance (Monad m, WithWalletContext m) => WithWalletContext (ReaderT a m)
instance (Monad m, WithWalletContext m) => WithWalletContext (StateT a m)
instance (Monad m, WithWalletContext m) => WithWalletContext (DHTResponseT s m)

instance Monad m => WithWalletContext (PC.ContextHolder ssc m) where
    getWalletContext = fromNodeCtx <$> PC.getNodeContext

deriving instance (Monad m, WithWalletContext m) => WithWalletContext (TxLDImpl m)
deriving instance (Monad m, WithWalletContext m) => WithWalletContext (SscLDImpl ssc m)
deriving instance (Monad m, WithWalletContext m) => WithWalletContext (Modern.TxpLDHolder ssc m)
deriving instance (Monad m, WithWalletContext m) => WithWalletContext (SscHolder ssc m)
