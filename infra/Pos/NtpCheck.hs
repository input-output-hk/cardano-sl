{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.NtpCheck
    ( mkNtpDiffVar
    , ntpSettings
    , withNtpCheck
    , NtpStatus(..)
    , NtpCheckMonad
    ) where

import           Universum                   hiding (bracket)

import           Control.Monad.Trans.Control (MonadBaseControl)

import qualified Data.List.NonEmpty          as NE
import           Data.Time.Units             (Microsecond)
import           Mockable                    (Bracket, CurrentTime, Delay, Fork, Mockable,
                                              Mockables, bracket, currentTime)
import           NTP.Client                  (NtpClientSettings (..), ntpSingleShot,
                                              pressNtpStopButton, startNtpClient)
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

withNtpCheck :: forall m a. NtpCheckMonad m => NtpClientSettings m -> m a -> m a
withNtpCheck settings action =
    bracket (startNtpClient settings) pressNtpStopButton (const action)

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

type NtpDiffVar = TVar Microsecond

-- Helper to get difference in `Microsecond`
mkNtpDiffVar :: ( NtpCheckMonad m , Mockables m [ CurrentTime, Delay] )
    => m NtpDiffVar
mkNtpDiffVar = do
    let noDiff = 0
    -- ^ no difference in microseconds, which is our initial value here
    diff <- newTVarIO noDiff
    let onStatusHandler = atomically . \case
            NtpSyncOk -> writeTVar diff noDiff
            -- ^ We will return no difference here, because we already
            -- considered `timeDifferenceWarnThreshold` with `NtpSyncOk`
            NtpDesync diff' -> writeTVar diff diff'
    _ <- ntpSingleShot $ ntpSettings onStatusHandler
    pure diff
