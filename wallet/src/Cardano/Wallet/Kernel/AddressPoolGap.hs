{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Kernel.AddressPoolGap (
    -- ** Address pool gap (for EOS-wallets)
    AddressPoolGap
  , MkAddressPoolGapError (..)
  , mkAddressPoolGap
  ) where

import           Universum

import qualified Data.Aeson.Options as Aeson
import           Data.Aeson.TH
import           Data.Default (Default (..))
import           Data.Swagger (ToSchema (..), defaultSchemaOptions,
                     genericDeclareNamedSchema)
import           Data.Text.Read (decimal)

import           Formatting (bprint, build, int, sformat, (%))
import qualified Formatting.Buildable

import           Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import           Test.QuickCheck
import qualified Test.QuickCheck.Gen as Gen

newtype AddressPoolGap = AddressPoolGap { getAddressPoolGap :: Word8 }
    deriving (Eq, Enum, Generic, Num, Ord, Real, Show)
    deriving newtype (Integral)

instance Bounded AddressPoolGap where
    -- NOTE: these values may change in the future.
    minBound = AddressPoolGap 10
    maxBound = AddressPoolGap 100

newtype MkAddressPoolGapError = GapOutOfRange Word8
    deriving (Eq, Show)

instance Buildable MkAddressPoolGapError where
    build (GapOutOfRange invalidGap) = bprint
        ("Address pool gap should be in range ["%int%".."%int%"], but "%int%" was provided.")
        (getAddressPoolGap minBound)
        (getAddressPoolGap maxBound)
        invalidGap

-- | Default value of address pool gap is taken from BIP-44 specification.
instance Default AddressPoolGap where
    def = AddressPoolGap 20

instance Arbitrary AddressPoolGap where
    arbitrary = AddressPoolGap <$>
        Gen.choose (getAddressPoolGap minBound, getAddressPoolGap maxBound)

instance Buildable AddressPoolGap where
    build (AddressPoolGap gap) =
        bprint ("Address pool gap "%int) gap

instance ToSchema AddressPoolGap where
    declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

instance FromHttpApiData AddressPoolGap where
    parseQueryParam rawGap = case decimal rawGap of
        Left _ -> Left "Unable to parse address pool gap (not an integer value)"
        Right (gap, _) -> case mkAddressPoolGap gap of
            Left problem   -> Left (sformat build problem)
            Right validGap -> Right validGap

instance ToHttpApiData AddressPoolGap where
    toQueryParam gap = sformat build gap

-- | Smart constructor for address pool gap.
mkAddressPoolGap :: Word8 -> Either MkAddressPoolGapError AddressPoolGap
mkAddressPoolGap gap
    | gap >= getAddressPoolGap minBound &&
      gap <= getAddressPoolGap maxBound = Right $ AddressPoolGap gap
    | otherwise = Left $ GapOutOfRange gap

deriveJSON Aeson.defaultOptions ''AddressPoolGap
