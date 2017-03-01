module Pos.Slotting.Constants
       ( ntpMaxError
       , ntpPollDelay
       , ntpResponseTimeout
       ) where

import           Data.Time.Units            (Microsecond)
import           Serokell.Util              (mcs, sec)
import           Universum

import           Pos.Infra.Constants.Parser (infraConstants)
import           Pos.Infra.Constants.Type   (ccNtpPollDelay, ccNtpResponseTimeout)

----------------------------------------------------------------------------
-- NTP
----------------------------------------------------------------------------

-- | Inaccuracy in call threadDelay (actually it is error much less than 1 sec)
ntpMaxError :: Microsecond
ntpMaxError = sec 1

-- | After making request to NTP servers, how long to wait for their response
ntpResponseTimeout :: Microsecond
ntpResponseTimeout = mcs . ccNtpResponseTimeout $ infraConstants

-- | How often send request to NTP server
ntpPollDelay :: Microsecond
ntpPollDelay = mcs . ccNtpPollDelay $ infraConstants
