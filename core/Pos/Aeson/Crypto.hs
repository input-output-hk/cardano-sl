module Pos.Aeson.Crypto
       (
       ) where

import           Universum

import           Crypto.Hash   (HashAlgorithm)
import           Data.Aeson    (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..),
                                ToJSON (..))
import qualified Data.Text     as T
import           Formatting    (sformat)

import           Pos.Crypto    (AbstractHash, ProxyCert, PublicKey, Signature (..),
                                decodeAbstractHash, fullPublicKeyF, hashHexF,
                                parseFullProxyCert, parseFullPublicKey,
                                parseFullSignature)
import           Pos.Util.Util (parseJSONWithRead)

instance ToJSON (AbstractHash algo a) where
    toJSON = toJSON . sformat hashHexF

instance HashAlgorithm algo => FromJSON (AbstractHash algo a) where
    parseJSON = parseJSONWithRead

instance (HashAlgorithm algo, FromJSON (AbstractHash algo a))
         => FromJSONKey (AbstractHash algo a) where
    fromJSONKey = FromJSONKeyTextParser parser
      where
        parser = either (fail . T.unpack) pure . decodeAbstractHash

instance ToJSON PublicKey where
    toJSON = toJSON . sformat fullPublicKeyF

instance FromJSON PublicKey where
    parseJSON v =
        parseJSON v >>=
        maybe (fail "FromJSON PublicKey: unable to parse") pure . parseFullPublicKey

instance FromJSON (Signature w) where
    parseJSON v =
        parseJSON v >>=
        maybe (fail "FromJSON Signature: unable to parse") pure . parseFullSignature

instance FromJSON (ProxyCert w) where
    parseJSON v =
        parseJSON v >>=
        maybe (fail "FromJSON ProxyCert: unable to parse") pure . parseFullProxyCert
