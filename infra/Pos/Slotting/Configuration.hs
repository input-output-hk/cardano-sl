module Pos.Slotting.Configuration
       ( ntpMaxError
       , ntpPollDelay
       , ntpResponseTimeout
       ) where

import           Data.Time.Units (Microsecond)
import           Serokell.Util (mcs)
import           Universum

import           Pos.Infra.Configuration (HasNtpConfiguration, ntpcMaxError, ntpcPollDelay,
                                          ntpcResponseTimeout, ntpConfiguration)

----------------------------------------------------------------------------
-- NTP
----------------------------------------------------------------------------

-- | Inaccuracy in call threadDelay (actually this error is much less than 1 sec)
ntpMaxError :: HasNtpConfiguration => Microsecond
ntpMaxError = mcs . ntpcMaxError $ ntpConfiguration

-- | After making request to NTP servers, how long to wait for their response
ntpResponseTimeout :: HasNtpConfiguration => Microsecond
ntpResponseTimeout = mcs . ntpcResponseTimeout $ ntpConfiguration

-- | How often send request to NTP server
ntpPollDelay :: HasNtpConfiguration => Microsecond
ntpPollDelay = mcs . ntpcPollDelay $ ntpConfiguration
