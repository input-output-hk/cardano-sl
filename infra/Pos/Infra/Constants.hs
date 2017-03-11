{-# LANGUAGE TemplateHaskell #-}

module Pos.Infra.Constants
       ( InfraConstants (..)
       , infraConstants
       ) where

import           Data.Aeson                 (FromJSON (..), genericParseJSON)
import           Data.Tagged                (Tagged (..))
import           Serokell.Aeson.Options     (defaultOptions)
import           Serokell.Data.Memory.Units (Byte)
import           Universum

import           Pos.Util.Config            (IsConfig (..), configParser,
                                             parseFromCslConfig)
import           Pos.Util.Util              ()

infraConstants :: InfraConstants
infraConstants = case parseFromCslConfig configParser of
    Left err -> panic (toText ("Couldn't parse infra config: " ++ err))
    Right x  -> x

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

    --------------------------------------------------------------------------
    -- -- Relay
    --------------------------------------------------------------------------
    , ccMaxReqSize               :: !Byte
      -- ^ Maximum `ReqMsg` size in bytes
    , ccMaxInvSize               :: !Byte
      -- ^ Maximum `InvMsg` size in bytes
    } deriving (Show, Generic)

instance FromJSON InfraConstants where
    parseJSON = genericParseJSON defaultOptions

instance IsConfig InfraConstants where
    configPrefix = Tagged Nothing
