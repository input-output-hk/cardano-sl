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
import           Text.JSON.Canonical (FromJSON (..), JSValue (..), ReportSchemaErrors, ToJSON (..),
                                      expected)

import           Pos.Util.Util (toAesonError)


--------------------------------------------------------------------------------
-- RequiresNetworkMagic
--------------------------------------------------------------------------------

-- | Bool-isomorphic flag indicating whether we're on testnet
-- or mainnet/staging.
data RequiresNetworkMagic
    = RequiresNoMagic
    | RequiresMagic
    deriving (Show, Eq, Generic)

-- TODO mhueschen : grok NFData
instance NFData RequiresNetworkMagic

-- Aeson JSON instances
-- N.B @RequiresNetworkMagic@'s ToJSON & FromJSON instances do not round-trip.
-- They should only be used from a parent instance which handles the
-- `requiresNetworkMagic` key.
instance A.ToJSON RequiresNetworkMagic where
    toJSON RequiresNoMagic = A.String "RequiresNoMagic"
    toJSON RequiresMagic    = A.String "RequiresMagic"

instance A.FromJSON RequiresNetworkMagic where
    parseJSON = A.withText "requiresNetworkMagic" $ toAesonError . \case
        "RequiresNoMagic" -> Right RequiresNoMagic
        "RequiresMagic"    -> Right RequiresMagic
        other   -> Left ("invalid value " <> show other <>
                         ", acceptable values are RequiresNoMagic | RequiresMagic")

-- Canonical JSON instances
instance Monad m => ToJSON m RequiresNetworkMagic where
    toJSON RequiresNoMagic = pure (JSString "RequiresNoMagic")
    toJSON RequiresMagic    = pure (JSString "RequiresMagic")

instance ReportSchemaErrors m => FromJSON m RequiresNetworkMagic where
    fromJSON = \case
        (JSString "RequiresNoMagic") -> pure RequiresNoMagic
        (JSString "RequiresMagic")    -> pure RequiresMagic
        other ->
            expected "RequiresNoMagic | RequiresMagic" (Just (show other))


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
    } deriving (Eq, Show, Generic)

-- mhueschen: For backwards-compatibility reasons, I redefine this function
-- in terms of the two record accessors.
getProtocolMagic :: ProtocolMagic -> Int32
getProtocolMagic = unProtocolMagicId . getProtocolMagicId

instance NFData ProtocolMagic

instance A.ToJSON ProtocolMagic where
    toJSON (ProtocolMagic (ProtocolMagicId ident) rnm) =
        A.object ["pm" .= ident, "requiresNetworkMagic" .= rnm]

-- Here we default to `RequiresMagic` (what testnets use) if only
-- a ProtocolMagic identifier is provided.
instance A.FromJSON ProtocolMagic where
    parseJSON v@(A.Number _) = ProtocolMagic
        <$> (ProtocolMagicId <$> A.parseJSON v)
        <*> pure RequiresMagic
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
    requiresNetworkMagic: RequiresNoMagic
```
-}
