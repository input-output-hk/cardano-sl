module Pos.Slotting.Constants
       ( ntpMaxError
       , ntpPollDelay
       , ntpResponseTimeout
       ) where

import           Data.Time.Units   (Microsecond)
import           Universum

ntpPollDelay :: Microsecond
ntpPollDelay = undefined

ntpMaxError :: Microsecond
ntpMaxError = undefined

ntpResponseTimeout :: Microsecond
ntpResponseTimeout = undefined

