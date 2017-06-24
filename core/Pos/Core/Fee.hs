module Pos.Core.Fee
       ( Coeff(..)
       , TxSizeLinear(..)
       , calculateTxSizeLinear
       , TxFeePolicy(..)
       ) where

import           Universum

import           Data.Fixed                 (Fixed (..), Nano, showFixed)
import qualified Data.Text.Buildable        as Buildable
import           Formatting                 (bprint, build, shown, (%))
import           Serokell.Data.Memory.Units (Byte, toBytes)

-- | A fractional coefficient of fixed precision.
newtype Coeff = Coeff Nano
    deriving (Eq, Show, Generic, NFData)

instance Buildable Coeff where
    build (Coeff x) = fromString (showFixed True x)

-- | A linear equation on the transaction size. Represents the @\s -> a + b*s@
-- function where @s@ is the transaction size in bytes, @a@ and @b@ are
-- constant coefficients.
data TxSizeLinear = TxSizeLinear !Coeff !Coeff
    deriving (Eq, Show, Generic)

instance NFData TxSizeLinear

instance Buildable TxSizeLinear where
    build (TxSizeLinear a b) =
        bprint (build%" + "%build%"*s") a b

calculateTxSizeLinear :: TxSizeLinear -> Byte -> Nano
calculateTxSizeLinear
    (TxSizeLinear (Coeff a) (Coeff b))
    (fromInteger . toBytes -> txSize) =
        a + b * txSize

-- | Transaction fee policy represents a formula to compute the minimal allowed
-- fee for a transaction. Transactions with lesser fees won't be accepted. The
-- minimal fee depends on the properties of a transaction (for example, its
-- size in bytes), so the minimal fee cannot be a constant.
--
-- Recall that a transaction fee is the difference between the sum of its
-- inputs and the sum of its outputs. The transaction is accepted when
-- @minimal_fee(tx) <= fee(tx)@, where @minimal_fee@ is the function defined
-- by the policy.
--
-- The policy can change during the lifetime of the blockchain (using the
-- update mechanism). At the moment we have just one policy type (a linear
-- equation on the transaction size), but in the future other policies may
-- be added. To make this future-proof, we also have an "unknown" policy used
-- by older node versions (the ones that haven't updated yet).
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
