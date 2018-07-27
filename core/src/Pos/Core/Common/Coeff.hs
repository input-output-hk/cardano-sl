module Pos.Core.Common.Coeff
       ( Coeff (..)
       ) where

import           Universum

import           Data.Fixed (Fixed (..), Nano, showFixed)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Formatting.Buildable as Buildable
import           Text.JSON.Canonical (FromJSON (..), ReportSchemaErrors,
                     ToJSON (..))

import           Pos.Binary.Class (Bi (..))
import           Pos.Core.Genesis.Canonical ()

-- | A fractional coefficient of fixed precision.
newtype Coeff = Coeff Nano
    deriving (Eq, Ord, Show, Generic, NFData, Num)

instance Buildable Coeff where
    build (Coeff x) = fromString (showFixed True x)

instance Bi Coeff where
    encode (Coeff n) = encode n
    decode = Coeff <$> decode @Nano

instance Hashable Coeff

instance Monad m => ToJSON m Coeff where
    toJSON (Coeff (MkFixed integer)) = toJSON @_ @Integer integer

instance ReportSchemaErrors m => FromJSON m Coeff where
    fromJSON = fmap (Coeff . MkFixed) . fromJSON @_ @Integer

deriveSafeCopySimple 0 'base ''Coeff
