{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Ntp.Check
    ( getNtpStatusOnce
    , ntpSettings
    , withNtpCheck
    , NtpStatus(..)
    , NtpCheckMonad
    ) where

import           Universum

import           Control.Concurrent.STM (retry)
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (Microsecond)
import           Mockable (CurrentTime, Delay, Mockable, Mockables, withAsync)
import           NTP.Client (NtpClientSettings (..), NtpMonad, NtpStatus (..), spawnNtpClient)
import           Serokell.Util (sec)

import           Pos.Ntp.Configuration (NtpConfiguration)
import qualified Pos.Ntp.Configuration as Ntp
import           Pos.Util.Util (median)

type NtpCheckMonad m =
    ( NtpMonad m
    , Mockable CurrentTime m
    )

withNtpCheck :: forall m a. NtpCheckMonad m => NtpClientSettings -> m a -> m a
withNtpCheck settings action = withAsync (spawnNtpClient settings) (const action)

ntpSettings :: NtpConfiguration -> TVar (Maybe NtpStatus) -> NtpClientSettings
ntpSettings ntpConfig ntpStatus = NtpClientSettings
    { ntpServers         = Ntp.ntpcServers ntpConfig
    , ntpLogName         = "ntp-check"
    , ntpResponseTimeout = sec 5
    , ntpPollDelay       = timeDifferenceWarnInterval ntpConfig
    , ntpMeanSelection   = median . NE.fromList
    , ntpTimeDifferenceWarnInterval  = timeDifferenceWarnInterval ntpConfig
    , ntpTimeDifferenceWarnThreshold = timeDifferenceWarnThreshold ntpConfig
    , ..
    }

timeDifferenceWarnInterval :: NtpConfiguration -> Microsecond
timeDifferenceWarnInterval = fromIntegral . Ntp.ntpcTimeDifferenceWarnInterval

timeDifferenceWarnThreshold :: NtpConfiguration -> Microsecond
timeDifferenceWarnThreshold = fromIntegral . Ntp.nptcTimeDifferenceWarnThreshold

-- | Create NTP client, let it work till the first response from servers,
-- then shutdown and return result.
getNtpStatusOnce :: ( NtpCheckMonad m , Mockables m [ CurrentTime, Delay] )
    => NtpConfiguration
    -> m NtpStatus
getNtpStatusOnce ntpConfig = do
    ntpStatus <- atomically $ newTVar Nothing
    let initNtp = spawnNtpClient $ ntpSettings ntpConfig ntpStatus
    withAsync initNtp $ \_ ->
        atomically $ do
            readTVar ntpStatus >>= \case
                Nothing -> retry
                Just st -> return st
