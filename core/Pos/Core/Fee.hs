module Pos.Core.Fee
       ( Coeff(..)
       , TxSizeLinear(..)
       , TxFeePolicy(..)
       ) where

import           Universum

import           Data.Fixed          (Fixed (..), Nano, showFixed)
import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, build, shown, (%))

-- | A fractional coefficient of fixed precision.
newtype Coeff = Coeff Nano
    deriving (Eq, Show, Generic, NFData)

instance Buildable Coeff where
    build (Coeff x) = fromString (showFixed True x)

-- | A linear equation on the transaction size. Represents the @\s -> a + b*s@
-- function where @s@ is the transaction size, @a@ and @b@ are constant
-- coefficients.
data TxSizeLinear = TxSizeLinear !Coeff !Coeff
    deriving (Eq, Show, Generic)

instance NFData TxSizeLinear

instance Buildable TxSizeLinear where
    build (TxSizeLinear a b) =
        bprint (build%" + "%build%"*s") a b

data TxFeePolicy
    = TxFeePolicyTxSizeLinear !TxSizeLinear
    | TxFeePolicyUnknown !Word8 !ByteString
    deriving (Eq, Show, Generic)

instance NFData TxFeePolicy

instance Buildable TxFeePolicy where
    build (TxFeePolicyTxSizeLinear tsp) =
        bprint ("policy(tx-size-linear): "%build) tsp
    build (TxFeePolicyUnknown v bs) =
        bprint ("policy(unknown:"%build%"): "%shown) v bs

