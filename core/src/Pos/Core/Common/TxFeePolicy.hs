module Pos.Core.Common.TxFeePolicy
       ( TxFeePolicy (..)
       ) where

import           Universum

import           Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM.S
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (bprint, build, shown, (%))
import qualified Formatting.Buildable as Buildable
import           Text.JSON.Canonical (FromJSON (..), ReportSchemaErrors,
                     ToJSON (..), fromJSField, mkObject)

import           Pos.Binary.Class (Bi (..), decodeKnownCborDataItem,
                     decodeUnknownCborDataItem, encodeKnownCborDataItem,
                     encodeListLen, encodeUnknownCborDataItem, enforceSize)
import           Pos.Core.Common.TxSizeLinear
import           Pos.Core.Genesis.Canonical ()
import           Pos.Util.Util (aesonError, toAesonError)

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

instance Monad m => ToJSON m TxFeePolicy where
    toJSON (TxFeePolicyTxSizeLinear (TxSizeLinear summand multiplier)) =
        mkObject
            [("summand", toJSON summand), ("multiplier", toJSON multiplier)]
    toJSON (TxFeePolicyUnknown {}) =
        error "Having TxFeePolicyUnknown in genesis is likely a bug"

instance ReportSchemaErrors m => FromJSON m TxFeePolicy where
    fromJSON obj = do
        summand <- fromJSField obj "summand"
        multiplier <- fromJSField obj "multiplier"
        return $ TxFeePolicyTxSizeLinear (TxSizeLinear summand multiplier)

instance Aeson.ToJSON TxFeePolicy where
    toJSON =
        object . \case
            TxFeePolicyTxSizeLinear linear -> ["txSizeLinear" .= linear]
            TxFeePolicyUnknown policyTag policyPayload ->
                ["unknown" .= (policyTag, decodeUtf8 @Text policyPayload)]

instance Aeson.FromJSON TxFeePolicy where
    parseJSON = Aeson.withObject "TxFeePolicy" $ \o -> do
        (policyName, policyBody) <- toAesonError $ case HM.S.toList o of
            []  -> Left "TxFeePolicy: none provided"
            [a] -> Right a
            _   -> Left "TxFeePolicy: ambiguous choice"
        let
          policyParser :: Aeson.FromJSON p => Aeson.Parser p
          policyParser = Aeson.parseJSON policyBody
        case policyName of
            "txSizeLinear" ->
                TxFeePolicyTxSizeLinear <$> policyParser
            "unknown" ->
                mkTxFeePolicyUnknown <$> policyParser
            _ ->
                aesonError "TxFeePolicy: unknown policy name"
        where
            mkTxFeePolicyUnknown (policyTag, policyPayload) =
                TxFeePolicyUnknown policyTag
                    (encodeUtf8 @Text @ByteString policyPayload)

deriveSafeCopySimple 0 'base ''TxFeePolicy
