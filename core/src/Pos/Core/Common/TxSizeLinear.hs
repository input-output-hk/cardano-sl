module Pos.Core.Common.TxSizeLinear
       ( TxSizeLinear (..)
       , txSizeLinearMinValue
       , calculateTxSizeLinear
       ) where

import           Universum

import           Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import           Data.Fixed (Nano)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Data.Memory.Units (Byte, toBytes)

import           Pos.Binary.Class (Bi (..), encodeListLen, enforceSize)
import           Pos.Core.Common.Coeff

-- | A linear equation on the transaction size. Represents the @\s -> a + b*s@
-- function where @s@ is the transaction size in bytes, @a@ and @b@ are
-- constant coefficients.
data TxSizeLinear = TxSizeLinear !Coeff !Coeff
    deriving (Eq, Ord, Show, Generic)

instance NFData TxSizeLinear

instance Buildable TxSizeLinear where
    build (TxSizeLinear a b) =
        bprint (build%" + "%build%"*s") a b

instance Bi TxSizeLinear where
    encode (TxSizeLinear a b) = encodeListLen 2 <> encode a <> encode b
    decode = do
        enforceSize "TxSizeLinear" 2
        !a <- decode @Coeff
        !b <- decode @Coeff
        return $ TxSizeLinear a b

instance Hashable TxSizeLinear

instance Aeson.ToJSON TxSizeLinear where
    toJSON (TxSizeLinear a b) = object [
        "a" .= a,
        "b" .= b
        ]

instance Aeson.FromJSON TxSizeLinear where
    parseJSON = Aeson.withObject "TxSizeLinear" $ \o -> do
        TxSizeLinear
            <$> (o Aeson..: "a")
            <*> (o Aeson..: "b")

calculateTxSizeLinear :: TxSizeLinear -> Byte -> Nano
calculateTxSizeLinear
    (TxSizeLinear (Coeff a) (Coeff b))
    (fromInteger . toBytes -> txSize) =
        a + b * txSize

txSizeLinearMinValue :: TxSizeLinear -> Nano
txSizeLinearMinValue (TxSizeLinear (Coeff minVal) _) = minVal

deriveSafeCopySimple 0 'base ''TxSizeLinear
