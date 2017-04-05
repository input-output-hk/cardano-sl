{-# LANGUAGE TypeFamilies #-}

-- | Class which provides access to WalletContext.

module Pos.Wallet.Context.Class
       ( WithWalletContext (..)
       ) where

import           Control.Monad.Trans         (MonadTrans)
import           Universum

import           Pos.Communication.PeerState (PeerStateHolder)
import qualified Pos.Context                 as PC
import           Pos.Delegation.Holder       (DelegationT (..))
import           Pos.Slotting                (NtpSlotting, SlottingHolder)
import           Pos.Ssc.Extra               (SscHolder (..))
import           Pos.Txp                     (TxpHolder (..))

import           Pos.Wallet.Context.Context  (WalletContext (..), fromNodeCtx)

-- | Class for something that has 'NodeContext' inside.
class Monad m => WithWalletContext m where
    getWalletContext :: m WalletContext
    default getWalletContext :: (MonadTrans t, WithWalletContext m', t m' ~ m) => m WalletContext
    getWalletContext = lift getWalletContext

instance (Monad m, WithWalletContext m) => WithWalletContext (ReaderT a m)
instance (Monad m, WithWalletContext m) => WithWalletContext (StateT a m)
instance (Monad m, WithWalletContext m) => WithWalletContext (PeerStateHolder m)
instance (Monad m, WithWalletContext m) => WithWalletContext (NtpSlotting m)
instance (Monad m, WithWalletContext m) => WithWalletContext (SlottingHolder m)

instance Monad m => WithWalletContext (PC.ContextHolder ssc m) where
    getWalletContext = fromNodeCtx <$> PC.getNodeContext

instance (Monad m, WithWalletContext m) => WithWalletContext (TxpHolder __ m)
instance (Monad m, WithWalletContext m) => WithWalletContext (SscHolder ssc m)
instance (Monad m, WithWalletContext m) => WithWalletContext (DelegationT m)
