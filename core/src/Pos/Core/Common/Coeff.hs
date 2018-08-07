module Pos.Core.Common.Coeff
       ( Coeff (..)
       ) where

import           Universum

import qualified Data.Aeson as Aeson
import           Data.Fixed (Fixed (..), Nano, resolution, showFixed)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import qualified Formatting.Buildable as Buildable
import           Text.JSON.Canonical (FromJSON (..), ReportSchemaErrors,
                     ToJSON (..))

import           Pos.Binary.Class (Bi (..))
import           Pos.Core.Genesis.Canonical ()
import           Pos.Util.Util (aesonError)

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

instance Aeson.ToJSON Coeff where
    toJSON (Coeff v) = Aeson.toJSON (realToFrac @_ @Double v)

instance Aeson.FromJSON Coeff where
    parseJSON = Aeson.withScientific "Coeff" $ \sc -> do
        -- Code below is resistant to changes in precision of 'Coeff'.
        let
            rat = toRational sc * toRational res
            fxd = MkFixed (numerator rat)
            res = resolution fxd
            bad = denominator rat /= 1
        when bad $ aesonError "Fixed precision for coefficient exceeded"
        return $ Coeff fxd

deriveSafeCopySimple 0 'base ''Coeff
