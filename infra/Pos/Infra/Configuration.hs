{-# LANGUAGE Rank2Types #-}
module Pos.Infra.Configuration
       ( NtpConfiguration (..)
       , HasNtpConfiguration
       , ntpConfiguration
       , withNtpConfiguration
       , ntpServers
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import           Data.Reflection (Given, give, given)
import           Serokell.Aeson.Options (defaultOptions)

data NtpConfiguration = NtpConfiguration
    {
    --------------------------------------------------------------------------
    -- -- NTP slotting
    --------------------------------------------------------------------------
      ntpcResponseTimeout          :: !Int
      -- ^ How often request to NTP server and response collection
    , ntpcPollDelay                :: !Int
      -- ^ How often send request to NTP server
    , ntpcMaxError                 :: !Int
    -- ^ Max NTP error (max difference between local and global time, which is trusted)

    --------------------------------------------------------------------------
    -- -- NTP checking
    --------------------------------------------------------------------------
    , ntpcTimeDifferenceWarnInterval  :: !Integer
      -- ^ NTP checking interval, microseconds
    , nptcTimeDifferenceWarnThreshold :: !Integer
      -- ^ Maximum tolerable difference between NTP time
      -- and local time, microseconds
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

ntpServers :: [String]
ntpServers =
    [ "0.pool.ntp.org"
    , "2.pool.ntp.org"
    , "3.pool.ntp.org"
    ]
    -- need only 3 servers :shrug:
