module Pos.Slotting.Constants
       ( ntpMaxError
       , ntpPollDelay
       , ntpResponseTimeout
       ) where

import           Data.Time.Units     (Microsecond)
import           Serokell.Util       (mcs)
import           Universum

import           Pos.Infra.Constants (ccNtpMaxError, ccNtpPollDelay, ccNtpResponseTimeout,
                                      infraConstants)

----------------------------------------------------------------------------
-- NTP
----------------------------------------------------------------------------

-- | Inaccuracy in call threadDelay (actually it is error much less than 1 sec)
ntpMaxError :: Microsecond
ntpMaxError = mcs . ccNtpMaxError $ infraConstants

-- | After making request to NTP servers, how long to wait for their response
ntpResponseTimeout :: Microsecond
ntpResponseTimeout = mcs . ccNtpResponseTimeout $ infraConstants

-- | How often send request to NTP server
ntpPollDelay :: Microsecond
ntpPollDelay = mcs . ccNtpPollDelay $ infraConstants
