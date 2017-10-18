{-# LANGUAGE ScopedTypeVariables #-}

module Pos.NtpCheck
    ( withNtpCheck
    , NtpStatus(..)
    ) where

import           Universum                   hiding (bracket)

import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.List.NonEmpty          as NE
import           Data.Time.Units             (Microsecond)
import           Mockable                    (Bracket, CurrentTime, Fork, Mockable,
                                              bracket, currentTime)
import           NTP.Client                  (NtpClientSettings (..), pressNtpStopButton,
                                              startNtpClient)
import           Pos.Core.Timestamp          (Timestamp (..), diffTimestamp)
import           Pos.Infra.Configuration     (HasInfraConfiguration, infraConfiguration)
import qualified Pos.Infra.Configuration     as Infra
import           Pos.Util.Util               (median)
import           Serokell.Util               (sec)
import           System.Wlog                 (WithLogger)

type NtpCheckMonad m =
    ( MonadIO m
    , MonadMask m
    , MonadBaseControl IO m
    , Mockable Fork m
    , Mockable Bracket m
    , Mockable CurrentTime m
    , WithLogger m
    , HasInfraConfiguration
    )

withNtpCheck :: forall m a. NtpCheckMonad m => (NtpStatus -> m ()) -> m a -> m a
withNtpCheck onStatus action = bracket (startNtpClient ntpSettings) pressNtpStopButton (\_ -> action)
  where
    ntpSettings :: NtpClientSettings m
    ntpSettings = NtpClientSettings
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
