-- | Instances for JSON serialization.

module Pos.Explorer.Web.Sockets.Instances where

import           Data.Aeson           (FromJSON (..), ToJSON (..), Value (String))
import           Data.Aeson.Types     (Parser)
import qualified Data.ByteString.Lazy as LBS
import           Pos.Binary           (Bi, decodeFull, encode)
import           Pos.Crypto           (AbstractHash (..), HashAlgorithm)
import           Pos.Data.Attributes  (Attributes)
import           Pos.Types            (AddrPkAttrs, Address, ChainDifficulty)
import qualified Serokell.Util.Base64 as Base64
import           Universum

parseBinaryJSON :: Bi a => Value -> Parser a
parseBinaryJSON x = do
    bs <- parseJSON x
    case decodeFull bs of
        Left err -> fail err
        Right a  -> return a

toBinaryJSON :: Bi a => a -> Value
toBinaryJSON = toJSON . encode

instance FromJSON Address
instance ToJSON Address

instance HashAlgorithm algo => FromJSON (AbstractHash algo a) where
    parseJSON = parseBinaryJSON
instance HashAlgorithm algo => ToJSON (AbstractHash algo a) where
    toJSON = toBinaryJSON

instance FromJSON a => FromJSON (Attributes a)
instance ToJSON a => ToJSON (Attributes a)

instance FromJSON AddrPkAttrs
instance ToJSON AddrPkAttrs

instance FromJSON ChainDifficulty where
instance ToJSON ChainDifficulty where

-- this is bad style, but explorer is an application rather than a library,
-- right?
instance FromJSON LByteString where
    parseJSON = fmap LBS.fromStrict . parseJSON
instance ToJSON LByteString where
    toJSON = toJSON . LBS.toStrict

instance FromJSON ByteString where
    parseJSON (String text) =
        case Base64.decode text of
            Left err -> fail $ show err
            Right bs -> return bs
    parseJSON _             = fail "String expected"
instance ToJSON ByteString where
    toJSON bs = String $ Base64.encode bs
