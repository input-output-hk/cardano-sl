{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Pos.Util.UnitsOfMeasure
    ( UnitOfMeasure (..)
    , MeasuredIn(..)
    ) where

import           Control.Lens (at, (?~))
import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object,
                     withObject, (.:), (.=))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as B
import           Formatting ((%))
import qualified Formatting as F
import           Formatting.Buildable (Buildable (..))
import           Pos.Core.Util.LogSafe (BuildableSafeGen (..))
import           Universum

import           Data.Swagger (NamedSchema (..), Referenced (..),
                     SwaggerType (..), ToSchema (..), enum_, properties,
                     required, type_)

-- | A finite sum type representing time units we might want to show to
-- clients. The idea is that whenever we have a quantity represeting some
-- form of time, we should render it together with the relevant unit, to
-- not leave anything to guessing.
data UnitOfMeasure =
      Seconds
    | Milliseconds
    | Microseconds
    -- | % ranging from 0 to 100.
    | Percentage100
    -- | Number of blocks.
    | Blocks
    -- | Number of blocks per second.
    | BlocksPerSecond
    | Bytes
    | Lovelace
    | LovelacePerByte
    deriving (Show, Eq, Ord)

instance Buildable UnitOfMeasure where
    build = \case
        Bytes           -> "bytes"
        LovelacePerByte -> "lovelace/byte"
        Lovelace        -> "lovelace"
        Seconds         -> "seconds"
        Milliseconds    -> "milliseconds"
        Microseconds    -> "microseconds"
        Percentage100   -> "percent"
        Blocks          -> "blocks"
        BlocksPerSecond -> "blocks/second"

instance ToJSON UnitOfMeasure where
    toJSON = String . T.toStrict . B.toLazyText . build

-- | Represent data with a given unit of measure
data MeasuredIn (u :: UnitOfMeasure) a
    = MeasuredIn a
    deriving (Show, Eq, Ord)

instance (Demote u, Buildable a) => BuildableSafeGen (MeasuredIn u a) where
    buildSafeGen _ = build

instance (Demote u, Buildable a) => Buildable (MeasuredIn u a) where
    build (MeasuredIn a) = F.bprint
        (F.build % " " % F.build)
        a
        (demote $ Proxy @u)

instance (Demote u, ToJSON a) => ToJSON (MeasuredIn u a) where
    toJSON (MeasuredIn a) = object
        [ "unit"     .= demote (Proxy @u)
        , "quantity" .= toJSON a
        ]

instance (Demote u, FromJSON a) => FromJSON (MeasuredIn u a) where
    parseJSON = withObject "MeasuredIn" $ \o -> do
        verifyUnit =<< o .: "unit"
        MeasuredIn <$> o .: "quantity"
      where
        unitS = toString $ T.toStrict $ B.toLazyText $ build $ demote $ Proxy @u
        verifyUnit = \case
            u@(String _) | u == toJSON (demote $ Proxy @u) ->
                pure ()
            _ ->
                fail
                $  "failed to parse quantified value. Expected value in '"
                <> unitS <> "' but got something else. e.g.: "
                <> "{ \"unit\": \"" <> unitS <> "\", \"quantity\": ...}"

instance (Demote u, ToSchema a) => ToSchema (MeasuredIn u a) where
    declareNamedSchema _ = do
        NamedSchema _ schema <- declareNamedSchema (Proxy @a)
        pure $ NamedSchema (Just "MeasuredIn") $ mempty
            & type_ ?~ SwaggerObject
            & required .~ ["quantity", "unit"]
            & properties .~ (mempty
                & at "quantity" ?~ Inline schema
                & at "unit" ?~ (Inline $ mempty
                    & type_ ?~ SwaggerString
                    & enum_ ?~ [toJSON $ demote $ Proxy @u]
                    )
                )

--
-- Internal
--

-- | Bring a type back to the world of value (invert of promote)
class Demote (u :: UnitOfMeasure) where
    demote :: Proxy u -> UnitOfMeasure
instance Demote 'Bytes           where demote _ = Bytes
instance Demote 'LovelacePerByte where demote _ = LovelacePerByte
instance Demote 'Lovelace        where demote _ = Lovelace
instance Demote 'Seconds         where demote _ = Seconds
instance Demote 'Milliseconds    where demote _ = Milliseconds
instance Demote 'Microseconds    where demote _ = Microseconds
instance Demote 'Percentage100   where demote _ = Percentage100
instance Demote 'Blocks          where demote _ = Blocks
instance Demote 'BlocksPerSecond where demote _ = BlocksPerSecond
