{-# LANGUAGE Rank2Types #-}
module Pos.Infra.Ntp.Configuration
       ( NtpConfiguration (..)
       , ntpClientSettings
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import           Data.List.NonEmpty as NE
import           Data.Time.Units (fromMicroseconds)
import           Ntp.Client (NtpClientSettings (..))
import           Serokell.Aeson.Options (defaultOptions)

data NtpConfiguration = NtpConfiguration
    {
      ntpcServers         :: [String]
      -- ^ List of DNS names of ntp servers
    , ntpcResponseTimeout :: !Integer
      -- ^ how long to await for responses from ntp servers (in microseconds)
    , ntpcPollDelay       :: !Integer
      -- ^ how long to wait between sending requests to the ntp servers (in
      -- microseconds)
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
    , ntpSelection       = minimum . NE.map abs
    -- ^ Take minmum of received offsets.
    }
