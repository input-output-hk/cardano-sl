{-# LANGUAGE RankNTypes #-}

module Pos.KnownPeers (
    MonadFormatPeers(..)
  ) where

import           Control.Monad.Trans.Class
import           Formatting (Format)
import           Universum

-- | For debugging: return formatted list of peers, if available
class MonadFormatPeers m where
    formatKnownPeers :: (forall a . Format r a -> a) -> m (Maybe r)

instance {-# OVERLAPPABLE #-}
    ( Monad m, MonadTrans f, MonadFormatPeers m ) =>
        MonadFormatPeers (f m) where
    formatKnownPeers k = lift (formatKnownPeers k)
