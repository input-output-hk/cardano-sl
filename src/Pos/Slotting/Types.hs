{-# LANGUAGE TemplateHaskell #-}

-- | Types used in Slotting.

module Pos.Slotting.Types
       ( SlottingState (..)
       , ssNtpLastSlot
       , ssNtpData
       ) where

import           Control.Lens    (makeLenses)
import           Data.Time.Units (Microsecond)
-- import           Universum

import           Pos.Types       (SlotId)

-- | Data needed for the slotting algorithm to work.
data SlottingState = SlottingState
    {
    -- | Slot which was returned from getCurrentSlot in last time
      _ssNtpLastSlot :: !SlotId
    -- | Data for the NTP Worker. First element: margin (difference
    -- between global time and local time) which we got from NTP
    -- server in last time.  Second element: time (local) for which we
    -- got margin in last time.
    , _ssNtpData     :: !(Microsecond, Microsecond)
    }

makeLenses ''SlottingState
