module Pos.Aeson.Crypto
       (
       ) where

import           Universum

import           Crypto.Hash   (HashAlgorithm)
import           Data.Aeson    (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..),
                                ToJSON (..))
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Formatting    (sformat)

import           Pos.Crypto    (AbstractHash, ProxyCert, ProxySecretKey, PublicKey,
                                Signature (..), decodeAbstractHash, fullProxyCertHexF,
                                fullPublicKeyF, hashHexF, parseFullProxyCert,
                                parseFullPublicKey, parseFullSignature)
import           Pos.Util.Util (eitherToFail, parseJSONWithRead)

instance ToJSON (AbstractHash algo a) where
    toJSON = toJSON . sformat hashHexF

instance HashAlgorithm algo => FromJSON (AbstractHash algo a) where
    parseJSON = parseJSONWithRead

instance (HashAlgorithm algo, FromJSON (AbstractHash algo a))
         => FromJSONKey (AbstractHash algo a) where
    fromJSONKey = FromJSONKeyTextParser parser
      where
        parser = eitherToFail . decodeAbstractHash

instance ToJSON PublicKey where
    toJSON = toJSON . sformat fullPublicKeyF

instance ToJSON (ProxyCert w) where
    toJSON = toJSON . sformat fullProxyCertHexF


eitherMsgFail :: (MonadFail m, ToString s) => String -> Either s a -> m a
eitherMsgFail msg =
    either (fail . (("Unable to parse json " <> msg <> " reason: ") <>) . toString) pure

instance FromJSON PublicKey where
    parseJSON v = parseJSON v >>= eitherMsgFail "PublicKey" . parseFullPublicKey

instance FromJSON (Signature w) where
    parseJSON v = parseJSON v >>= eitherMsgFail "Signature" . parseFullSignature

instance FromJSON (ProxyCert w) where
    parseJSON v = parseJSON v >>= eitherMsgFail "Signature" . parseFullProxyCert

deriveJSON defaultOptions ''ProxySecretKey
