{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

-- | Slotting utilities.

module Pos.Sinbin.Slotting.Util
       (
         -- * Helpers using 'MonadSlots[Data]'
         getCurrentSlotFlat

         -- * Worker which ticks when slot starts and its parameters
       , OnNewSlotParams (..)
       , defaultOnNewSlotParams
       , ActionTerminationPolicy (..)
       ) where

import           Universum

import           Pos.Core (FlatSlotId, HasProtocolConstants, flattenSlotId)
import           Pos.Sinbin.Slotting.Class (MonadSlots (..))


-- | Get flat id of current slot based on MonadSlots.
getCurrentSlotFlat :: (MonadSlots ctx m, HasProtocolConstants) => m (Maybe FlatSlotId)
getCurrentSlotFlat = fmap flattenSlotId <$> getCurrentSlot


-- | Parameters for `onNewSlot`.
data OnNewSlotParams = OnNewSlotParams
    { onspStartImmediately  :: !Bool
    -- ^ Whether first action should be executed ASAP (i. e. basically
    -- when the program starts), or only when new slot starts.
    --
    -- For example, if the program is started in the middle of a slot
    -- and this parameter in 'False', we will wait for half of slot
    -- and only then will do something.
    , onspTerminationPolicy :: !ActionTerminationPolicy
    -- ^ What should be done if given action doesn't finish before new
    -- slot starts. See the description of 'ActionTerminationPolicy'.
    }

-- | Default parameters which were used by almost all code before this
-- data type was introduced.
defaultOnNewSlotParams :: OnNewSlotParams
defaultOnNewSlotParams =
    OnNewSlotParams
    { onspStartImmediately = True
    , onspTerminationPolicy = NoTerminationPolicy
    }

-- | This policy specifies what should be done if the action passed to
-- `onNewSlot` doesn't finish when current slot finishes.
--
-- We don't want to run given action more than once in parallel for
-- variety of reasons:
-- 1. If action hangs for some reason, there can be infinitely growing pool
-- of hanging actions with probably bad consequences (e. g. leaking memory).
-- 2. Thread management will be quite complicated if we want to fork
-- threads inside `onNewSlot`.
-- 3. If more than one action is launched, they may use same resources
-- concurrently, so the code must account for it.
data ActionTerminationPolicy
    = NoTerminationPolicy
    -- ^ Even if action keeps running after current slot finishes,
    -- we'll just wait and start action again only after the previous
    -- one finishes.
    | NewSlotTerminationPolicy !Text
    -- ^ If new slot starts, running action will be cancelled. Name of
    -- the action should be passed for logging.
