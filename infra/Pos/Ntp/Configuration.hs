{-# LANGUAGE Rank2Types #-}
module Pos.Ntp.Configuration
       ( NtpConfiguration (..)
       , HasNtpConfiguration
       , ntpConfiguration
       , withNtpConfiguration
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import           Data.Reflection (Given, give, given)
import           Serokell.Aeson.Options (defaultOptions)

data NtpConfiguration = NtpConfiguration
    {
    --------------------------------------------------------------------------
    -- -- NTP checking
    --------------------------------------------------------------------------
      ntpcTimeDifferenceWarnInterval  :: !Integer
      -- ^ NTP checking interval, microseconds
    , nptcTimeDifferenceWarnThreshold :: !Integer
      -- ^ Maximum tolerable difference between NTP time
      -- and local time, microseconds
    , ntpcServers                     :: [String]
      -- ^ List of ntp servers
    } deriving (Show, Generic)

instance FromJSON NtpConfiguration where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON NtpConfiguration where
    toJSON = genericToJSON defaultOptions

type HasNtpConfiguration = Given NtpConfiguration

withNtpConfiguration :: NtpConfiguration -> (HasNtpConfiguration => r) -> r
withNtpConfiguration = give

ntpConfiguration :: HasNtpConfiguration => NtpConfiguration
ntpConfiguration = given
