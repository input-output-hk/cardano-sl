{-# LANGUAGE Rank2Types #-}
module Pos.Ntp.Configuration
       ( NtpConfiguration (..)
       , ntpClientSettings
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import           Data.List.NonEmpty as NE
import           Data.Time.Units (fromMicroseconds)
import           Ntp.Client (NtpClientSettings (..))
import           Serokell.Aeson.Options (defaultOptions)
import           Pos.Util.Util (median)

data NtpConfiguration = NtpConfiguration
    {
      ntpcResponseTimeout             :: !Integer
      -- ^ how long to await for responses from ntp servers (in microseconds)
    , ntpcPollDelay                   :: !Integer
      -- ^ how long to wait between to send requests to the servers (in
      -- microseconds)
    , ntpcTimeDifferenceWarnInterval  :: !Integer
      -- ^ NTP checking interval (in microseconds)
    , ntpcTimeDifferenceWarnThreshold :: !Integer
      -- ^ Maximum tolerable difference between NTP time and local time (in
      -- microseconds)
    , ntpcServers                     :: [String]
      -- ^ List of ntp servers
    } deriving (Show, Generic)

instance FromJSON NtpConfiguration where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON NtpConfiguration where
    toJSON = genericToJSON defaultOptions

ntpClientSettings :: NtpConfiguration -> NtpClientSettings
ntpClientSettings NtpConfiguration {..} = NtpClientSettings
    { ntpServers         = ntpcServers
    , ntpResponseTimeout = fromMicroseconds $ ntpcResponseTimeout
    , ntpPollDelay       = fromMicroseconds $ ntpcPollDelay
    , ntpMeanSelection   = median . NE.fromList
    , ntpTimeDifferenceWarnInterval  = fromMicroseconds $ ntpcTimeDifferenceWarnInterval
    , ntpTimeDifferenceWarnThreshold = fromMicroseconds $ ntpcTimeDifferenceWarnThreshold
    }
