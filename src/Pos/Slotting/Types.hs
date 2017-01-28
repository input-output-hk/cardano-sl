{-# LANGUAGE TemplateHaskell #-}

-- | Datatypes related to slotting.
-- Module is added to deal with cyclic imports.

module Pos.Slotting.Types
       ( SlottingState (..)
       , ssSlotDurationL
       , ssNtpLastSlotL
       , ssNtpDataL
       ) where

import           Control.Lens    (makeLensesFor)
import           Data.Time.Units (Microsecond, Millisecond, convertUnit)
import           Universum

import           Pos.Types       (SlotId (..))

-- | Data needed for the slotting algorithm to work.
data SlottingState = SlottingState
    {
    -- | Current slot duration. (It can be changed by update proposals.)
      ssSlotDuration :: !Millisecond
    -- | Slot which was returned from getCurrentSlot in last time
    , ssNtpLastSlot  :: !SlotId
    -- | Data for the NTP Worker. First element: margin (difference between
    -- global time and local time) which we got from NTP server in last time.
    -- Second element: time (local) for which we got margin in last time.
    , ssNtpData      :: !(Microsecond, Microsecond)
    }

flip makeLensesFor ''SlottingState [
    ("ssSlotDuration", "ssSlotDurationL"),
    ("ssNtpLastSlot", "ssNtpLastSlotL"),
    ("ssNtpData", "ssNtpDataL") ]
