{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to part of database which contains
-- message length limits.

module Pos.DB.Limits
       ( MonadDBLimits (..)
       ) where

import           Universum

import           Pos.DB.Holder (DBHolder)
import           Pos.DB.Limits (MonadDBLimits (..))
import qualified Pos.Update.DB as DB

instance (MonadIO m, MonadThrow m) => MonadDBLimits (DBHolder m) where
    getMaxBlockSize = DB.getMaxBlockSize
