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
import           Mockable (CurrentTime, Delay, Mockable, Mockables, currentTime, withAsync)
import           NTP.Client (NtpClientSettings (..), NtpMonad, spawnNtpClient)
import           Serokell.Util (sec)

import           Pos.Core.Slotting (Timestamp (..), diffTimestamp)
import           Pos.Infra.Configuration (HasInfraConfiguration, infraConfiguration)
import qualified Pos.Infra.Configuration as Infra
import           Pos.Util.Util (median)

type NtpCheckMonad m =
    ( NtpMonad m
    , Mockable CurrentTime m
    , HasInfraConfiguration
    )

withNtpCheck :: forall m a. NtpCheckMonad m => NtpClientSettings m -> m a -> m a
withNtpCheck settings action = withAsync (spawnNtpClient settings) (const action)

ntpSettings :: NtpCheckMonad m => (NtpStatus -> m ()) -> NtpClientSettings m
ntpSettings onStatus = NtpClientSettings
    { ntpServers         = Infra.ntpServers
    , ntpHandler         = ntpCheckHandler onStatus
    , ntpLogName         = "ntp-check"
    , ntpResponseTimeout = sec 5
    , ntpPollDelay       = timeDifferenceWarnInterval
    , ntpMeanSelection   = median . NE.fromList
    }

data NtpStatus = NtpSyncOk | NtpDesync Microsecond
    deriving (Eq, Show)

ntpCheckHandler :: NtpCheckMonad m => (NtpStatus -> m a) -> (Microsecond, Microsecond) -> m a
ntpCheckHandler cont (newMargin, transmitTime) = do
    let ntpTime = Timestamp $ transmitTime + newMargin
    localTime <- Timestamp <$> currentTime
    let timeDiff = diffTimestamp ntpTime localTime
    let ntpStatus
            | timeDiff <= timeDifferenceWarnThreshold = NtpSyncOk
            | otherwise = NtpDesync timeDiff
    cont ntpStatus

timeDifferenceWarnInterval :: HasInfraConfiguration => Microsecond
timeDifferenceWarnInterval = fromIntegral (Infra.ccTimeDifferenceWarnInterval infraConfiguration)

timeDifferenceWarnThreshold :: HasInfraConfiguration => Microsecond
timeDifferenceWarnThreshold = fromIntegral (Infra.ccTimeDifferenceWarnThreshold infraConfiguration)

-- | Create NTP client, let it work till the first response from servers,
-- then shutdown and return result.
getNtpStatusOnce :: ( NtpCheckMonad m , Mockables m [ CurrentTime, Delay] )
    => m NtpStatus
getNtpStatusOnce = do
    status <- newEmptyMVar
    let onStatusHandler = putMVar status
    let initNtp = spawnNtpClient $ ntpSettings onStatusHandler
    withAsync initNtp $ \_ ->
        readMVar status
