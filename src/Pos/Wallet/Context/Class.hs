{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to WalletContext.

module Pos.Wallet.Context.Class
       ( WithWalletContext (..)
       , readNtpLastSlot
       , readNtpMargin
       , readNtpTimestamp
       ) where

import qualified Control.Concurrent.STM     as STM
import           Control.Monad.Trans        (MonadTrans)
import           Data.Time.Units            (Microsecond)
import           Universum


import qualified Pos.Context                as PC
import           Pos.Delegation.Class       (DelegationT (..))
import           Pos.DHT.Model              (DHTResponseT)
import           Pos.DHT.Real               (KademliaDHT)
import           Pos.Ssc.Extra              (SscHolder (..))
import qualified Pos.Txp.Holder             as Modern
import           Pos.Types                  (SlotId)

import           Pos.Wallet.Context.Context (WalletContext (..), fromNodeCtx)

-- | Class for something that has 'NodeContext' inside.
class Monad m => WithWalletContext m where
    getWalletContext :: m WalletContext
    default getWalletContext :: (MonadTrans t, WithWalletContext m', t m' ~ m) => m WalletContext
    getWalletContext = lift getWalletContext

instance (Monad m, WithWalletContext m) => WithWalletContext (KademliaDHT m)
instance (Monad m, WithWalletContext m) => WithWalletContext (ReaderT a m)
instance (Monad m, WithWalletContext m) => WithWalletContext (StateT a m)
instance (Monad m, WithWalletContext m) => WithWalletContext (DHTResponseT s m)

instance Monad m => WithWalletContext (PC.ContextHolder ssc m) where
    getWalletContext = fromNodeCtx <$> PC.getNodeContext

deriving instance (Monad m, WithWalletContext m) => WithWalletContext (Modern.TxpLDHolder ssc m)
deriving instance (Monad m, WithWalletContext m) => WithWalletContext (SscHolder ssc m)
deriving instance (Monad m, WithWalletContext m) => WithWalletContext (DelegationT m)

readNtpLastSlot :: (MonadIO m, WithWalletContext m) => m SlotId
readNtpLastSlot = getWalletContext >>= atomically . STM.readTVar . wcNtpLastSlot

readNtpMargin :: (MonadIO m, WithWalletContext m) => m Microsecond
readNtpMargin = getWalletContext >>= fmap fst . atomically . STM.readTVar . wcNtpData

readNtpTimestamp :: (MonadIO m, WithWalletContext m) => m Microsecond
readNtpTimestamp = getWalletContext >>= fmap snd . atomically . STM.readTVar . wcNtpData
