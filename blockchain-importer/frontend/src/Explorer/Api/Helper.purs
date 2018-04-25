module Explorer.Api.Helper where

import Prelude
import Control.Monad.Eff.Exception (Error, error)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Generic.Aeson (userDecoding, userEncoding)
import Data.Argonaut.Generic.Decode (genericDecodeJson)
import Data.Argonaut.Generic.Encode (genericEncodeJson)
import Data.Argonaut.Generic.Options (Options(..), SumEncoding(..))
import Data.Argonaut.Generic.Util (stripModulePath)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Generic (class Generic)
import Explorer.Api.Types (EndpointError(..))


-- custom encoding

sumEncoding :: SumEncoding
sumEncoding =
    TaggedObject
    { tagFieldName: "tag"
    , contentsFieldName: "contents"
    , unpackRecords: false
    }

options :: Options
options =
    Options
    { constructorTagModifier: stripModulePath
    , allNullaryToStringTag: true
    , sumEncoding
    , flattenContentsArray: true
    , encodeSingleConstructors: false
    , userEncoding
    , userDecoding
    , fieldLabelModifier: id
    , omitNothingFields: false
    }


-- | Encodes JSON using custom Options provided by `options`
encodeJson :: forall a. (Generic a) => a -> Json
encodeJson = genericEncodeJson options

-- | Decodes JSON using custom Options provided by `options`
decodeJson :: forall a. (Generic a) => Json -> Either String a
decodeJson = genericDecodeJson options

-- decode result of `http` or `socket` endpoints

-- | Converts a JSONDecodingError into an error
mkJSONError :: String -> Error
mkJSONError = error <<< show <<< JSONDecodingError

-- | Decodes result considering JSON and Server errors
decodeResult :: forall a. Generic a => Json -> Either Error a
decodeResult = either (Left <<< mkJSONError) (bimap mkServerError id) <<< decodeJson
  where
    mkServerError = error <<< show <<< ServerError

-- | Decodes result considering JSON errors, only
decodeResult' :: forall a. Generic a => Json -> Either Error a
decodeResult' = either (Left <<< mkJSONError) pure <<< decodeJson
