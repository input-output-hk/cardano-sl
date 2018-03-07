{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.NtpCheck
    ( getNtpStatusOnce
    , ntpSettings
    , withNtpCheck
    , NtpStatus(..)
    , NtpCheckMonad
    ) where

import           Universum

import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (Microsecond)
import           Mockable (CurrentTime, Delay, Mockable, Mockables, withAsync)
import           NTP.Client (NtpClientSettings (..), NtpMonad, NtpStatus (..), spawnNtpClient)
import           Serokell.Util (sec)

import           Pos.Infra.Configuration (HasNtpConfiguration, NtpConfiguration, ntpConfiguration)
import qualified Pos.Infra.Configuration as Infra
import           Pos.Util.Util (median)

type NtpCheckMonad m =
    ( NtpMonad m
    , Mockable CurrentTime m
    , HasNtpConfiguration
    )

withNtpCheck :: forall m a. NtpCheckMonad m => NtpClientSettings -> m a -> m a
withNtpCheck settings action = withAsync (spawnNtpClient settings) (const action)

ntpSettings :: NtpConfiguration -> MVar NtpStatus -> NtpClientSettings
ntpSettings ntpConfig ntpStatus = NtpClientSettings
    { ntpServers         = Infra.ntpcServers ntpConfig
    , ntpLogName         = "ntp-check"
    , ntpResponseTimeout = sec 5
    , ntpPollDelay       = timeDifferenceWarnInterval ntpConfig
    , ntpMeanSelection   = median . NE.fromList
    , ntpTimeDifferenceWarnInterval  = timeDifferenceWarnInterval ntpConfig
    , ntpTimeDifferenceWarnThreshold = timeDifferenceWarnThreshold ntpConfig
    , ..
    }

timeDifferenceWarnInterval :: NtpConfiguration -> Microsecond
timeDifferenceWarnInterval = fromIntegral . Infra.ntpcTimeDifferenceWarnInterval

timeDifferenceWarnThreshold :: NtpConfiguration -> Microsecond
timeDifferenceWarnThreshold = fromIntegral . Infra.nptcTimeDifferenceWarnThreshold

-- | Create NTP client, let it work till the first response from servers,
-- then shutdown and return result.
getNtpStatusOnce :: ( NtpCheckMonad m , Mockables m [ CurrentTime, Delay] )
    => m NtpStatus
getNtpStatusOnce = do
    ntpStatus <- newEmptyMVar
    let initNtp = spawnNtpClient $ ntpSettings ntpConfiguration ntpStatus
    withAsync initNtp $ \_ ->
        readMVar ntpStatus
