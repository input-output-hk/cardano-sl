{-# LANGUAGE DeriveLift #-}

module Pos.Infra.Constants.Type
       ( InfraConstants (..)
       ) where

import           Data.Aeson                 (FromJSON (..), genericParseJSON)
import           Data.Tagged                (Tagged (..))
import           Language.Haskell.TH.Syntax (Lift)
import           Serokell.Aeson.Options     (defaultOptions)
import           Serokell.Data.Memory.Units (Byte)
import           Universum

import           Pos.Util.Config            (IsConfig (..))
import           Pos.Util.Util              ()

data InfraConstants = InfraConstants
    { ccNtpResponseTimeout       :: !Int
      -- ^ How often request to NTP server and response collection
    , ccNtpPollDelay             :: !Int
      -- ^ How often send request to NTP server

    , ccNeighboursSendThreshold  :: !Int
      -- ^ Broadcasting threshold
    , ccKademliaDumpInterval     :: !Int
      -- ^ Interval for dumping Kademlia state in slots
    , ccEnhancedMessageBroadcast :: !Word
      -- ^ True if we should enable enhanced bessage broadcast

    , ccNetworkReceiveTimeout    :: !Int
      -- ^ Network timeout on `recv` in milliseconds

    ----------------------------------------------------------------------------
    -- -- Relay
    ----------------------------------------------------------------------------
    , ccMaxReqSize               :: !Byte
      -- ^ Maximum `ReqMsg` size in bytes
    , ccMaxInvSize               :: !Byte
      -- ^ Maximum `InvMsg` size in bytes
    } deriving (Show, Lift, Generic)

instance FromJSON InfraConstants where
    parseJSON = genericParseJSON defaultOptions

instance IsConfig InfraConstants where
    configPrefix = Tagged Nothing
