module Pos.Slotting.Configuration
       ( ntpMaxError
       , ntpPollDelay
       , ntpResponseTimeout
       ) where

import           Universum

import           Data.Time.Units (Microsecond)

import           Pos.Infra.Configuration (HasInfraConfiguration, ccNtpMaxError, ccNtpPollDelay,
                                          ccNtpResponseTimeout, infraConfiguration)
import           Pos.Util.Util (mcs)

----------------------------------------------------------------------------
-- NTP
----------------------------------------------------------------------------

-- | Inaccuracy in call threadDelay (actually this error is much less than 1 sec)
ntpMaxError :: HasInfraConfiguration => Microsecond
ntpMaxError = mcs . ccNtpMaxError $ infraConfiguration

-- | After making request to NTP servers, how long to wait for their response
ntpResponseTimeout :: HasInfraConfiguration => Microsecond
ntpResponseTimeout = mcs . ccNtpResponseTimeout $ infraConfiguration

-- | How often send request to NTP server
ntpPollDelay :: HasInfraConfiguration => Microsecond
ntpPollDelay = mcs . ccNtpPollDelay $ infraConfiguration
