{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | JSON instances for txp datatypes.

module Pos.Aeson.Txp where

import           Universum

import           Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..),
                             ToJSON (toJSON), ToJSONKey (..), object, withObject, (.:), (.=))
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Aeson.Types (toJSONKeyText)
import qualified Data.Text as T
import           Formatting (build, int, sformat, (%))
import qualified Serokell.Util.Base16 as B16
import           Serokell.Util.Base64 (JsonByteString (..))

import           Pos.Aeson.Core ()
import           Pos.Aeson.Crypto ()
import           Pos.Core (coinToInteger, decodeTextAddress, integerToCoin)
import           Pos.Core.Txp (Tx, TxAux, TxIn (..), TxInWitness (..), TxOut (..), TxOutAux,
                               TxSigData)
import           Pos.Crypto (decodeAbstractHash, hashHexF)
import           Pos.Util.Util (aesonError, toAesonError)

txInFromText :: Text -> Either Text TxIn
txInFromText t = case T.splitOn "_" t of
    ["TxInUtxo", h, idx]     -> TxInUtxo <$> decodeAbstractHash h <*> readEither idx
    ["TxInUnknown", tag, bs] -> TxInUnknown <$> readEither tag <*> B16.decode bs
    _                        -> Left $ "Invalid TxIn " <> t

txInToText :: TxIn -> Text
txInToText TxInUtxo {..}        = sformat ("TxInUtxo_"%hashHexF%"_"%int) txInHash txInIndex
txInToText (TxInUnknown tag bs) = sformat ("TxInUnknown_"%int%"_"%B16.base16F) tag bs

instance FromJSON TxIn where
    parseJSON v = toAesonError =<< txInFromText <$> parseJSON v

instance ToJSON TxIn where
    toJSON = toJSON . txInToText

instance FromJSONKey TxIn where
    fromJSONKey = FromJSONKeyTextParser (toAesonError . txInFromText)

instance ToJSONKey TxIn where
    toJSONKey = toJSONKeyText txInToText

instance FromJSON TxOut where
    parseJSON = withObject "TxOut" $ \o -> do
        txOutValue   <- toAesonError . integerToCoin =<< o .: "coin"
        txOutAddress <- toAesonError . decodeTextAddress =<< o .: "address"
        return $ TxOut {..}

instance ToJSON TxOut where
    toJSON TxOut{..} = object [
        "coin"    .= coinToInteger txOutValue,
        "address" .= sformat build txOutAddress ]

instance ToJSON TxInWitness where
    toJSON = \case
        PkWitness{..} -> object
            [ "tag" .= ("PkWitness" :: Text)
            , "key" .= twKey
            , "sig" .= twSig
            ]
        ScriptWitness{..} -> object
            [ "tag" .= ("ScriptWitness" :: Text)
            , "validator" .= twValidator
            , "redeemer" .= twRedeemer
            ]
        RedeemWitness{..} -> object
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

deriveJSON defaultOptions ''Tx
deriveJSON defaultOptions ''TxOutAux
deriveJSON defaultOptions ''TxSigData
deriveJSON defaultOptions ''TxAux
