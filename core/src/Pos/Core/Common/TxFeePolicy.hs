module Pos.Core.Common.TxFeePolicy
       ( TxFeePolicy (..)
       ) where

import           Universum

import qualified Data.ByteString.Lazy as LBS
import           Data.Hashable (Hashable)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (bprint, build, shown, (%))
import qualified Formatting.Buildable as Buildable

import           Pos.Binary.Class (Bi (..), decodeKnownCborDataItem,
                     decodeUnknownCborDataItem, encodeKnownCborDataItem,
                     encodeListLen, encodeUnknownCborDataItem, enforceSize)
import           Pos.Core.Common.TxSizeLinear

-- | Transaction fee policy represents a formula to compute the minimal allowed
-- fee for a transaction. Transactions with lesser fees won't be accepted. The
-- minimal fee may depend on the properties of a transaction (for example, its
-- size in bytes), so the policy can't be represented simply as a number.
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
    deriving (Eq, Ord, Show, Generic)

instance NFData TxFeePolicy

instance Buildable TxFeePolicy where
    build (TxFeePolicyTxSizeLinear tsp) =
        bprint ("policy(tx-size-linear): "%build) tsp
    build (TxFeePolicyUnknown v bs) =
        bprint ("policy(unknown:"%build%"): "%shown) v bs

instance Bi TxFeePolicy where
    encode policy = case policy of
        TxFeePolicyTxSizeLinear txSizeLinear ->
            encodeListLen 2 <> encode (0 :: Word8)
                            <> encodeKnownCborDataItem txSizeLinear
        TxFeePolicyUnknown word8 bs          ->
            encodeListLen 2 <> encode word8
                            <> encodeUnknownCborDataItem (LBS.fromStrict bs)
    decode = do
        enforceSize "TxFeePolicy" 2
        tag <- decode @Word8
        case tag of
            0 -> TxFeePolicyTxSizeLinear <$> decodeKnownCborDataItem
            _ -> TxFeePolicyUnknown tag  <$> decodeUnknownCborDataItem

instance Hashable TxFeePolicy

deriveSafeCopySimple 0 'base ''TxFeePolicy
