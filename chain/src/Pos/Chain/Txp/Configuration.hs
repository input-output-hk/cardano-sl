{-# LANGUAGE RankNTypes #-}

-- | Configuration for the txp package.

module Pos.Chain.Txp.Configuration
       ( TxpConfiguration(..)
       , RequiresNetworkMagic (..)
       , memPoolLimitTx
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object,
                     withText, (.!=), (.:), (.:?), (.=))
import           Data.Aeson.Types (typeMismatch)

import           Pos.Core (Address)
import           Pos.Util.Util (toAesonError)

data RequiresNetworkMagic
    = NMMustBeNothing
    | NMMustBeJust
    deriving (Eq, Generic, Show)

-- N.B @RequiresNetworkMagic@'s ToJSON & FromJSON instances do not round-trip.
-- They should only be used from TxpConfiguration's instances.
instance ToJSON RequiresNetworkMagic where
    toJSON NMMustBeNothing = String "NMMustBeNothing"
    toJSON NMMustBeJust    = String "NMMustBeJust"

instance FromJSON RequiresNetworkMagic where
    parseJSON = withText "requiresNetworkMagic" $ toAesonError . \case
        "NMMustBeNothing" -> Right NMMustBeNothing
        "NMMustBeJust"    -> Right NMMustBeJust
        other   -> Left ("invalid value " <> show other <>
                         ", acceptable values are NMMustBeNothing | NMMustBeJust")

-- | Delegation configruation part.
data TxpConfiguration = TxpConfiguration
    { -- | Limit on the number of transactions that can be stored in
      -- the mem pool.
      ccMemPoolLimitTx       :: !Int

      -- | Set of source address which are asset-locked. Transactions which
      -- use these addresses as transaction inputs will be silently dropped.
    , tcAssetLockedSrcAddrs  :: !(Set Address)

      -- | Field indicating presence/absense of ProtocolMagic in Addresses
      -- for this cluster.
    , tcRequiresNetworkMagic :: !RequiresNetworkMagic
    } deriving (Eq, Show, Generic)

instance ToJSON TxpConfiguration where
    toJSON tc = object
        [ "memPoolLimitTx"       .= (ccMemPoolLimitTx tc)
        , "assetLockedSrcAddrs"  .= (tcAssetLockedSrcAddrs tc)
        , "requiresNetworkMagic" .= (tcRequiresNetworkMagic tc)
        ]

instance FromJSON TxpConfiguration where
    parseJSON (Object o) = do
        mplt <- o .:  "memPoolLimitTx"
        asla <- o .:  "assetLockedSrcAddrs"
        rnm  <- o .:? "requiresNetworkMagic" .!= NMMustBeJust
        pure (TxpConfiguration mplt asla rnm)
    parseJSON invalid = typeMismatch "TxpConfiguration" invalid

----------------------------------------------------------------------------
-- Constants
----------------------------------------------------------------------------

-- | Limint on the number of transactions that can be stored in
-- the mem pool.
memPoolLimitTx :: Integral i => TxpConfiguration -> i
memPoolLimitTx txpConfig = fromIntegral . ccMemPoolLimitTx $ txpConfig
