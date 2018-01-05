{-# LANGUAGE Rank2Types #-}
module Pos.Infra.Configuration
       ( InfraConfiguration (..)
       , HasInfraConfiguration
       , infraConfiguration
       , withInfraConfiguration
       , ntpServers
       ) where

import           Data.Aeson (FromJSON (..), genericParseJSON)
import           Data.Reflection (Given, give, given)
import           Serokell.Aeson.Options (defaultOptions)
import           Universum


data InfraConfiguration = InfraConfiguration
    {
    --------------------------------------------------------------------------
    -- -- NTP slotting
    --------------------------------------------------------------------------
      ccNtpResponseTimeout          :: !Int
      -- ^ How often request to NTP server and response collection
    , ccNtpPollDelay                :: !Int
      -- ^ How often send request to NTP server
    , ccNtpMaxError                 :: !Int
    -- ^ Max NTP error (max difference between local and global time, which is trusted)

    , ccNetworkWaitLogInterval      :: !Int
      -- ^ Network wait logging interval in seconds
      --   (logging that some recv/send takes significant amount of time)

    --------------------------------------------------------------------------
    -- -- NTP checking
    --------------------------------------------------------------------------
    , ccTimeDifferenceWarnInterval  :: !Integer
      -- ^ NTP checking interval, microseconds
    , ccTimeDifferenceWarnThreshold :: !Integer
      -- ^ Maximum tolerable difference between NTP time
      -- and local time, microseconds
    } deriving (Show, Generic)

instance FromJSON InfraConfiguration where
    parseJSON = genericParseJSON defaultOptions

type HasInfraConfiguration = Given InfraConfiguration

withInfraConfiguration :: InfraConfiguration -> (HasInfraConfiguration => r) -> r
withInfraConfiguration = give

infraConfiguration :: HasInfraConfiguration => InfraConfiguration
infraConfiguration = given

ntpServers :: [String]
ntpServers =
    [ "time.windows.com"
    , "clock.isc.org"
    , "ntp5.stratum2.ru" ]
