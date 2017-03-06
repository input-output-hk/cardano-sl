{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to part of database which contains
-- message length limits.

module Pos.DB.Limits
       ( MonadDBLimits (..)
       ) where

import           Control.Monad.Except       (ExceptT)
import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.State        (StateT)
import           Control.Monad.Trans        (MonadTrans)
import           Serokell.Data.Memory.Units (Byte)
import           Universum

-- | Weakened `MonadDB` which keeps limits on messages size.
class Monad m => MonadDBLimits m where
    getMaxBlockSize :: m Byte
    getMaxHeaderSize :: m Byte
    getMaxTxSize :: m Byte
    getMaxProposalSize :: m Byte

    default getMaxBlockSize
        :: (MonadTrans t, MonadDBLimits m', t m' ~ m)
        => m Byte
    getMaxBlockSize = lift getMaxBlockSize

    default getMaxHeaderSize
        :: (MonadTrans t, MonadDBLimits m', t m' ~ m)
        => m Byte
    getMaxHeaderSize = lift getMaxHeaderSize

    default getMaxTxSize
        :: (MonadTrans t, MonadDBLimits m', t m' ~ m)
        => m Byte
    getMaxTxSize = lift getMaxTxSize

    default getMaxProposalSize
        :: (MonadTrans t, MonadDBLimits m', t m' ~ m)
        => m Byte
    getMaxProposalSize = lift getMaxProposalSize

instance MonadDBLimits m => MonadDBLimits (ReaderT r m)
instance MonadDBLimits m => MonadDBLimits (StateT r m)
instance MonadDBLimits m => MonadDBLimits (ExceptT r m)
