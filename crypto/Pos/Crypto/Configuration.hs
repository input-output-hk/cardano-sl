{-# LANGUAGE OverloadedStrings #-}

module Pos.Crypto.Configuration
       ( ProtocolMagic (..)
       , ProtocolMagicId (..)
       , RequiresNetworkMagic (..)
       , getProtocolMagic
       ) where

import           Universum

import           Control.Monad.Except (MonadError)
import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import           Data.Aeson.Types (typeMismatch)
import           Data.List (lookup)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Text.JSON.Canonical (FromJSON (..), JSValue (..), ToJSON (..),
                     expected)

import           Pos.Util.Json.Canonical (SchemaError)
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

instance NFData RequiresNetworkMagic
deriveSafeCopySimple 0 'base ''RequiresNetworkMagic

-- Aeson JSON instances
-- N.B @RequiresNetworkMagic@'s ToJSON & FromJSON instances do not round-trip.
-- They should only be used from a parent instance which handles the
-- `requiresNetworkMagic` key.
instance A.ToJSON RequiresNetworkMagic where
    toJSON RequiresNoMagic = A.String "RequiresNoMagic"
    toJSON RequiresMagic   = A.String "RequiresMagic"

instance A.FromJSON RequiresNetworkMagic where
    parseJSON = A.withText "requiresNetworkMagic" $ toAesonError . \case
        "RequiresNoMagic" -> Right RequiresNoMagic
        "RequiresMagic"   -> Right RequiresMagic
        "NMMustBeNothing" -> Right RequiresNoMagic
        "NMMustBeJust"    -> Right RequiresMagic
        other   -> Left ("invalid value " <> show other <>
                         ", acceptable values are RequiresNoMagic | RequiresMagic")

-- Canonical JSON instances
instance Monad m => ToJSON m RequiresNetworkMagic where
    toJSON RequiresNoMagic = pure (JSString "RequiresNoMagic")
    toJSON RequiresMagic   = pure (JSString "RequiresMagic")

instance MonadError SchemaError m => FromJSON m RequiresNetworkMagic where
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

deriveSafeCopySimple 0 'base ''ProtocolMagicId

-- | Magic number which should differ for different clusters. It's
-- defined here, because it's used for signing. It also used for other
-- things (e. g. it's part of a serialized block).
--
-- mhueschen: As part of CO-353 I am adding `getRequiresNetworkMagic` in
-- order to pipe configuration to functions which must generate & verify
-- Addresses (which now must be aware of `NetworkMagic`).
data ProtocolMagic = ProtocolMagic
    { getProtocolMagicId      :: !ProtocolMagicId
    , getRequiresNetworkMagic :: !RequiresNetworkMagic
    } deriving (Eq, Show, Generic)

-- mhueschen: For backwards-compatibility reasons, I redefine this function
-- in terms of the two record accessors.
getProtocolMagic :: ProtocolMagic -> Int32
getProtocolMagic = unProtocolMagicId . getProtocolMagicId

instance NFData ProtocolMagic
deriveSafeCopySimple 0 'base ''ProtocolMagic

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
    parseJSON invalid = typeMismatch "ProtocolMagic" invalid

-- Canonical JSON instances
instance Monad m => ToJSON m ProtocolMagic where
    -- | We only output the `ProtocolMagicId` such that we don't alter the
    -- resulting hash digest of the genesis block.
    --
    -- In the function, `withCoreConfigurations`, we compare the hash of the
    -- canonical JSON representation of a hardcoded genesis block with an
    -- accompanying hardcoded hash of that same genesis block at its inception
    -- (both of which can be found in lib/configuration.yaml). This allows us
    -- to verify the integrity of the genesis block and ensure that it hasn't
    -- been altered.
    --
    -- As a result of this addition of the `RequiresNetworkMagic` field to
    -- `ProtocolMagic`, we cannot include the newly introduced
    -- `RequiresNetworkMagic` field of `ProtocolMagic` as it would produce
    -- invalid hashes for previously existing genesis blocks.
    --
    -- See the implementation of `withCoreConfigurations` for more detail on
    -- how this works.
    toJSON (ProtocolMagic (ProtocolMagicId ident) _rnm) = toJSON ident

-- Here we default to `RequiresMagic` (what testnets use) if only
-- a ProtocolMagic identifier is provided.
instance MonadError SchemaError m => FromJSON m ProtocolMagic where
    fromJSON = \case
        (JSNum n) -> pure (ProtocolMagic (ProtocolMagicId (fromIntegral n))
                                         RequiresMagic)
        (JSObject dict) -> ProtocolMagic
            <$> (ProtocolMagicId <$> expectLookup "pm: <int>" "pm" dict)
            <*> expectLookup "requiresNetworkMagic: <RequiresNoMagic | \
                             \RequiresMagic>"
                             "requiresNetworkMagic"
                             dict
        other ->
            expected "RequiresNoMagic | RequiresMagic" (Just (show other))

expectLookup :: (MonadError SchemaError m, FromJSON m a)
             => String -> String -> [(String, JSValue)] -> m a
expectLookup msg key dict = case lookup key dict of
    Nothing -> expected msg Nothing
    Just x  -> fromJSON x

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
