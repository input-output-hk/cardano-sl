module Pos.Core.Txp.TxWitness
       ( TxWitness
       , TxInWitness (..)
       , TxSigData (..)
       , TxSig
       )where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (toJSON), object, withObject,
                     (.:), (.=))
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.ByteString.Lazy as LBS
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Util.Base16 (base16F)
import           Serokell.Util.Base64 (JsonByteString (..))

import           Pos.Binary.Class (Bi (..), Case (..), decodeKnownCborDataItem,
                     decodeListLenCanonical, decodeUnknownCborDataItem,
                     encodeKnownCborDataItem, encodeListLen,
                     encodeUnknownCborDataItem, knownCborDataItemSizeExpr,
                     matchSize, szCases)
import           Pos.Core.Common (Script, addressHash)
import           Pos.Crypto (Hash, PublicKey, RedeemPublicKey, RedeemSignature,
                     Signature, hash, shortHashF)
import           Pos.Util.Util (aesonError)

import           Pos.Core.Txp.Tx (Tx)

-- | A witness is a proof that a transaction is allowed to spend the funds it
-- spends (by providing signatures, redeeming scripts, etc). A separate proof
-- is provided for each input.
type TxWitness = Vector TxInWitness

-- | A witness for a single input.
data TxInWitness
    -- PkWitness twKey twSig
    = PkWitness !PublicKey !TxSig
    -- ScriptWitness twValidator twRedeemer
    | ScriptWitness !Script !Script
    -- RedeemWitness twRedeemKey twRedeemSig
    | RedeemWitness !RedeemPublicKey !(RedeemSignature TxSigData)
    | UnknownWitnessType !Word8 !ByteString
    deriving (Eq, Show, Generic, Typeable)

instance ToJSON TxInWitness where
    toJSON = \case
        PkWitness twKey twSig -> object
            [ "tag" .= ("PkWitness" :: Text)
            , "key" .= twKey
            , "sig" .= twSig
            ]
        ScriptWitness twValidator twRedeemer -> object
            [ "tag" .= ("ScriptWitness" :: Text)
            , "validator" .= twValidator
            , "redeemer" .= twRedeemer
            ]
        RedeemWitness twRedeemKey twRedeemSig -> object
            [ "tag" .= ("RedeemWitness" :: Text)
            , "redeemKey" .= twRedeemKey
            , "redeemSig" .= twRedeemSig
            ]
        UnknownWitnessType a b -> object
            [ "tag" .= ("UnknownWitnessType" :: Text)
            , "contents" .= [toJSON a, toJSON (JsonByteString b)]
            ]

instance FromJSON TxInWitness where
    parseJSON = withObject "TxInWitness" $ \o ->
        (o .: "tag") >>= \case
            ("PkWitness"::Text) ->
                PkWitness <$> (o .: "key") <*> (o .: "sig")
            "ScriptWitness" ->
                ScriptWitness <$> (o .: "validator") <*> (o .: "redeemer")
            "RedeemWitness" ->
                RedeemWitness <$> (o .: "redeemKey") <*> (o .: "redeemSig")
            "UnknownWitnessType" -> do
                (o .: "contents") >>= \case
                    [a, b] -> UnknownWitnessType <$> parseJSON a <*> (getJsonByteString <$> parseJSON b)
                    _      -> aesonError $ "expected 'contents' to have two elements"
            _  ->
                aesonError $ "expected 'tag' to be one of 'PkWitness', 'ScriptWitness', \
                    \'RedeemWitness', 'UnknownWitnessType'"

instance Hashable TxInWitness

instance Buildable TxInWitness where
    build (PkWitness key sig) =
        bprint ("PkWitness: key = "%build%", key hash = "%shortHashF%
                ", sig = "%build) key (addressHash key) sig
    build (ScriptWitness val red) =
        bprint ("ScriptWitness: "%
                "validator hash = "%shortHashF%", "%
                "redeemer hash = "%shortHashF) (hash val) (hash red)
    build (RedeemWitness key sig) =
        bprint ("PkWitness: key = "%build%", sig = "%build) key sig
    build (UnknownWitnessType t bs) =
        bprint ("UnknownWitnessType "%build%" "%base16F) t bs

instance Bi TxInWitness where
    encode input = case input of
        PkWitness key sig         ->
            encodeListLen 2 <>
            encode (0 :: Word8) <>
            encodeKnownCborDataItem (key, sig)
        ScriptWitness val red     ->
            encodeListLen 2 <>
            encode (1 :: Word8) <>
            encodeKnownCborDataItem (val, red)
        RedeemWitness key sig     ->
            encodeListLen 2 <>
            encode (2 :: Word8) <>
            encodeKnownCborDataItem (key, sig)
        UnknownWitnessType tag bs ->
            encodeListLen 2 <>
            encode tag <>
            encodeUnknownCborDataItem (LBS.fromStrict bs)
    decode = do
        len <- decodeListLenCanonical
        tag <- decode @Word8
        case tag of
            0 -> do
                matchSize len "TxInWitness.PkWitness" 2
                uncurry PkWitness <$> decodeKnownCborDataItem
            1 -> do
                matchSize len "TxInWitness.ScriptWitness" 2
                uncurry ScriptWitness <$> decodeKnownCborDataItem
            2 -> do
                matchSize len "TxInWitness.RedeemWitness" 2
                uncurry RedeemWitness <$> decodeKnownCborDataItem
            _ -> do
                matchSize len "TxInWitness.UnknownWitnessType" 2
                UnknownWitnessType tag <$> decodeUnknownCborDataItem

    encodedSizeExpr size _ = 2 +
        (szCases $ map (fmap knownCborDataItemSizeExpr) $
            [ let PkWitness key sig     = error "unused"
              in  Case "PkWitness" $ size ((,) <$> pure key <*> pure sig)
            , let ScriptWitness key sig = error "unused"
              in  Case "ScriptWitness" $ size ((,) <$> pure key <*> pure sig)
            , let RedeemWitness key sig = error "unused"
              in  Case "RedeemWitness" $ size ((,) <$> pure key <*> pure sig)
            ])

instance NFData TxInWitness

-- | Data that is being signed when creating a TxSig.
data TxSigData = TxSigData
    { -- | Transaction that we're signing
      txSigTxHash      :: !(Hash Tx)
    }
    deriving (Eq, Show, Generic, Typeable)

instance Bi TxSigData where
    encode (TxSigData {..}) = encode txSigTxHash
    decode = TxSigData <$> decode
    encodedSizeExpr size pxy = size (txSigTxHash <$> pxy)

-- | 'Signature' of addrId.
type TxSig = Signature TxSigData

deriveSafeCopySimple 0 'base ''TxInWitness

deriveJSON defaultOptions ''TxSigData
