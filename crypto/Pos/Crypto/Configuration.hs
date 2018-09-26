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
    = NMMustBeNothing
    | NMMustBeJust
    deriving (Show, Eq, Generic)

instance NFData RequiresNetworkMagic
deriveSafeCopySimple 0 'base ''RequiresNetworkMagic

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

instance MonadError SchemaError m => FromJSON m RequiresNetworkMagic where
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

-- Canonical JSON instances
instance Monad m => ToJSON m ProtocolMagic where
    toJSON (ProtocolMagic (ProtocolMagicId ident) rnm) = do
        (\jsIdent jsRNM -> JSObject
            [ ("pm", jsIdent)
            , ("requiresNetworkMagic", jsRNM) ])
        <$> toJSON ident
        <*> toJSON rnm

-- Here we default to `NMMustBeJust` (what testnets use) if only
-- a ProtocolMagic identifier is provided.
instance MonadError SchemaError m => FromJSON m ProtocolMagic where
    fromJSON = \case
        (JSNum n) -> pure (ProtocolMagic (ProtocolMagicId (fromIntegral n))
                                         NMMustBeJust)
        (JSObject dict) -> ProtocolMagic
            <$> (ProtocolMagicId <$> expectLookup "pm: <int>" "pm" dict)
            <*> expectLookup "requiresNetworkMagic: <NMMustBeNothing | \
                             \NMMustBeJust>"
                             "requiresNetworkMagic"
                             dict
        other ->
            expected "NMMustBeNothing | NMMustBeJust" (Just (show other))

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
    requiresNetworkMagic: NMMustBeNothing
```
-}
