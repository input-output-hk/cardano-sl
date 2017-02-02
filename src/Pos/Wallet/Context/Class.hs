{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to WalletContext.

module Pos.Wallet.Context.Class
       ( WithWalletContext (..)
       ) where

import           Control.Monad.Trans         (MonadTrans)
import           Universum

import           Pos.Communication.PeerState (PeerStateHolder)
import qualified Pos.Context                 as PC
import           Pos.Delegation.Holder       (DelegationT (..))
import           Pos.DHT.Real                (KademliaDHT (..))
import           Pos.Slotting                (DBSlotsData, NtpSlotting)
import           Pos.Ssc.Extra               (SscHolder (..))
import qualified Pos.Txp.Holder              as Modern
import           Pos.Update                  (USHolder (..))

import           Pos.Wallet.Context.Context  (WalletContext (..), fromNodeCtx)
import           Pos.Wallet.Slotting         (WalletDBSlotsData)

-- | Class for something that has 'NodeContext' inside.
class Monad m => WithWalletContext m where
    getWalletContext :: m WalletContext
    default getWalletContext :: (MonadTrans t, WithWalletContext m', t m' ~ m) => m WalletContext
    getWalletContext = lift getWalletContext

instance (Monad m, WithWalletContext m) => WithWalletContext (ReaderT a m)
instance (Monad m, WithWalletContext m) => WithWalletContext (StateT a m)
instance (Monad m, WithWalletContext m) => WithWalletContext (PeerStateHolder m)
instance (Monad m, WithWalletContext m) => WithWalletContext (NtpSlotting m)
instance (Monad m, WithWalletContext m) => WithWalletContext (WalletDBSlotsData m)
instance (Monad m, WithWalletContext m) => WithWalletContext (DBSlotsData m)

instance Monad m => WithWalletContext (PC.ContextHolder ssc m) where
    getWalletContext = fromNodeCtx <$> PC.getNodeContext

instance (Monad m, WithWalletContext m) => WithWalletContext (Modern.TxpLDHolder ssc m)
instance (Monad m, WithWalletContext m) => WithWalletContext (SscHolder ssc m)
instance (Monad m, WithWalletContext m) => WithWalletContext (DelegationT m)
instance (Monad m, WithWalletContext m) => WithWalletContext (USHolder m)
instance (Monad m, WithWalletContext m) => WithWalletContext (KademliaDHT m)
