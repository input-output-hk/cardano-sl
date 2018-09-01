{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
-- TODO mhueschen : i don't think this should be necessary

module Pos.Crypto.Configuration
       ( ProtocolMagic (..)
       , ProtocolMagicId (..)
       , RequiresNetworkMagic (..)
       , getProtocolMagic
       ) where

import           Universum

import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import           Data.Aeson.Types (typeMismatch)
import           Data.Function (on)
import           Text.JSON.Canonical (FromJSON (..), JSValue (..), ReportSchemaErrors, ToJSON (..),
                                      expected)

import           Pos.Util.Util (toAesonError)


--------------------------------------------------------------------------------
-- RequiresNetworkMagic
--------------------------------------------------------------------------------

-- | Bool-isomorphic flag indicating whether we're on testnet
-- or mainnet/staging.
data RequiresNetworkMagic
    = NMMustBeNothing
    | NMMustBeJust
    deriving (Show, Eq, Generic)

-- TODO mhueschen : grok NFData
instance NFData RequiresNetworkMagic

-- Aeson JSON instances
-- N.B @RequiresNetworkMagic@'s ToJSON & FromJSON instances do not round-trip.
-- They should only be used from a parent instance which handles the
-- `requiresNetworkMagic` key.
instance A.ToJSON RequiresNetworkMagic where
    toJSON NMMustBeNothing = A.String "NMMustBeNothing"
    toJSON NMMustBeJust    = A.String "NMMustBeJust"

instance A.FromJSON RequiresNetworkMagic where
    parseJSON = A.withText "requiresNetworkMagic" $ toAesonError . \case
        "NMMustBeNothing" -> Right NMMustBeNothing
        "NMMustBeJust"    -> Right NMMustBeJust
        other   -> Left ("invalid value " <> show other <>
                         ", acceptable values are NMMustBeNothing | NMMustBeJust")

-- Canonical JSON instances
instance Monad m => ToJSON m RequiresNetworkMagic where
    toJSON NMMustBeNothing = pure (JSString "NMMustBeNothing")
    toJSON NMMustBeJust    = pure (JSString "NMMustBeJust")

instance ReportSchemaErrors m => FromJSON m RequiresNetworkMagic where
    fromJSON = \case
        (JSString "NMMustBeNothing") -> pure NMMustBeNothing
        (JSString "NMMustBeJust")    -> pure NMMustBeJust
        other ->
            expected "NMMustBeNothing | NMMustBeJust" (Just (show other))


--------------------------------------------------------------------------------
-- ProtocolMagic
--------------------------------------------------------------------------------

newtype ProtocolMagicId = ProtocolMagicId
    { unProtocolMagicId :: Int32
    } deriving (Show, Eq, NFData)

-- | Magic number which should differ for different clusters. It's
-- defined here, because it's used for signing. It also used for other
-- things (e. g. it's part of a serialized block).
--
-- As part of CO-353 I am adding
-- `getRequiresNetworkMagic` in order to pipe configuration to functions
-- which must generate & verify Addresses (which now must be aware of
-- `NetworkMagic`).
data ProtocolMagic = ProtocolMagic
    { getProtocolMagicId      :: !ProtocolMagicId
    , getRequiresNetworkMagic :: !RequiresNetworkMagic
    } deriving (Show, Generic)

-- mhueschen: For backwards-compatibility reasons, I redefine this function
-- in terms of the two record accessors.
getProtocolMagic :: ProtocolMagic -> Int32
getProtocolMagic = unProtocolMagicId . getProtocolMagicId

-- Since ProtocolMagic is widely used and we want to maintain backwards
-- compatibility of equality checks between the Int32 identifier, we only
-- compare Eq on `getProtocolMagicId`.
instance Eq ProtocolMagic where
    (==) = (==) `on` getProtocolMagicId

instance NFData ProtocolMagic

instance A.ToJSON ProtocolMagic where
    toJSON (ProtocolMagic (ProtocolMagicId ident) rnm) =
        A.object ["pm" .= ident, "requiresNetworkMagic" .= rnm]

-- Here we default to `NMMustBeJust` (what testnets use) if only
-- a ProtocolMagic identifier is provided.
instance A.FromJSON ProtocolMagic where
    parseJSON v@(A.Number _) = ProtocolMagic
        <$> (ProtocolMagicId <$> A.parseJSON v)
        <*> pure NMMustBeJust
    parseJSON (A.Object o) = ProtocolMagic
        <$> (ProtocolMagicId <$> o .: "pm")
        <*> o .: "requiresNetworkMagic"
    parseJSON invalid = typeMismatch "Coord" invalid

{-
We need to handle the old format (YAML example):

```
protocolMagic: 12345678
```

and the new format

```
protocolMagic:
    pm: 12345678
    requiresNetworkMagic: NMMustBeNothing
```
-}
