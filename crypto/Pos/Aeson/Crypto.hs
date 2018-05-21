{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Aeson.Crypto
       (
       ) where

import           Universum

import           Control.Lens (_Left)
import           Crypto.Hash (HashAlgorithm)
import qualified Crypto.Sign.Ed25519 as Ed25519
import           Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..),
                             ToJSONKey (..))
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Aeson.Types (toJSONKeyText)
import           Formatting (sformat)
import           Serokell.Util.Base64 (JsonByteString (..))

import           Pos.Crypto (AbstractHash, HDAddressPayload (..), ProtocolMagic (..), ProxyCert,
                             ProxySecretKey, PublicKey, RedeemPublicKey, RedeemSignature,
                             Signature (..), decodeAbstractHash, fullProxyCertHexF, fullPublicKeyF,
                             fullSignatureHexF, hashHexF, parseFullProxyCert, parseFullPublicKey,
                             parseFullSignature)
import           Pos.Util.Util (parseJSONWithRead, toAesonError)

deriving instance ToJSON ProtocolMagic
deriving instance FromJSON ProtocolMagic

instance ToJSON (AbstractHash algo a) where
    toJSON = toJSON . sformat hashHexF

instance HashAlgorithm algo => FromJSON (AbstractHash algo a) where
    parseJSON = parseJSONWithRead

instance (HashAlgorithm algo, FromJSON (AbstractHash algo a))
         => FromJSONKey (AbstractHash algo a) where
    fromJSONKey = FromJSONKeyTextParser (toAesonError . decodeAbstractHash)

instance ToJSONKey (AbstractHash algo a) where
    toJSONKey = toJSONKeyText (sformat hashHexF)

instance ToJSON PublicKey where
    toJSON = toJSON . sformat fullPublicKeyF

instance ToJSON (ProxyCert w) where
    toJSON = toJSON . sformat fullProxyCertHexF

fmsg :: ToText s => Text -> Either s a -> Either Text a
fmsg msg = over _Left $ \e ->
    ("Unable to parse json " <> msg <> " reason: ") <> toText e

instance FromJSON PublicKey where
    parseJSON v = parseJSON v >>= toAesonError . fmsg "PublicKey" . parseFullPublicKey

instance FromJSON (Signature w) where
    parseJSON v = parseJSON v >>= toAesonError . fmsg "Signature" . parseFullSignature

instance ToJSON (Signature w) where
    toJSON = toJSON . sformat fullSignatureHexF

instance FromJSON (ProxyCert w) where
    parseJSON v = parseJSON v >>= toAesonError . fmsg "Signature" . parseFullProxyCert

deriveJSON defaultOptions ''ProxySecretKey

-- All these instances below are needed for wallet

instance FromJSON Ed25519.PublicKey where
    parseJSON v = Ed25519.PublicKey . getJsonByteString <$> parseJSON v

instance ToJSON Ed25519.PublicKey where
    toJSON = toJSON . JsonByteString . Ed25519.openPublicKey

instance FromJSON Ed25519.Signature where
    parseJSON v = Ed25519.Signature . getJsonByteString <$> parseJSON v

instance ToJSON Ed25519.Signature where
    toJSON = toJSON . JsonByteString . Ed25519.unSignature

instance FromJSON HDAddressPayload where
    parseJSON v = HDAddressPayload . getJsonByteString <$> parseJSON v

instance ToJSON HDAddressPayload where
    toJSON = toJSON . JsonByteString . getHDAddressPayload

deriveJSON defaultOptions ''RedeemPublicKey
deriveJSON defaultOptions ''RedeemSignature
