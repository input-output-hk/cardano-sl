-- | Slotting functionality.

module Pos.Slotting
       ( Timestamp (..)
       , MonadSlots (..)
       , getCurrentSlot
       ) where

import           Universum

import           Pos.Types (SlotId)

-- | TODO
newtype Timestamp =
    Timestamp ()
    deriving (Show, Monoid)

-- | Type class providing information about time when system started
-- functioning.
class Monad m => MonadSlots m where
    getSystemStartTime :: m Timestamp
    getCurrentTime :: m Timestamp

getCurrentSlot :: MonadSlots m => m SlotId
getCurrentSlot = notImplemented
