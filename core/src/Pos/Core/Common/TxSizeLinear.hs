module Pos.Core.Common.TxSizeLinear
       ( TxSizeLinear (..)
       , txSizeLinearMinValue
       , calculateTxSizeLinear
       ) where

import           Universum

import           Data.Fixed (Nano)
import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, build, (%))
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

calculateTxSizeLinear :: TxSizeLinear -> Byte -> Nano
calculateTxSizeLinear
    (TxSizeLinear (Coeff a) (Coeff b))
    (fromInteger . toBytes -> txSize) =
        a + b * txSize

txSizeLinearMinValue :: TxSizeLinear -> Nano
txSizeLinearMinValue (TxSizeLinear (Coeff minVal) _) = minVal
