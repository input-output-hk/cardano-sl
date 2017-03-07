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
    }


encodeJson :: forall a. (Generic a) => a -> Json
encodeJson = genericEncodeJson options

decodeJson :: forall a. (Generic a) => Json -> Either String a
decodeJson = genericDecodeJson options

-- decode result of `http` or `socket` endpoints

decodeResult :: forall a. Generic a => Json -> Either Error a
decodeResult = either (Left <<< mkJSONError) (bimap mkServerError id) <<< decodeJson
  where
    mkJSONError = error <<< show <<< JSONDecodingError
    mkServerError = error <<< show <<< ServerError
