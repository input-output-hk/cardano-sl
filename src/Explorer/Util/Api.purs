module Explorer.Util.Api where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Generic.Aeson (userDecoding, userEncoding)
import Data.Argonaut.Generic.Decode (genericDecodeJson)
import Data.Argonaut.Generic.Encode (genericEncodeJson)
import Data.Argonaut.Generic.Options (Options(..), SumEncoding(..))
import Data.Argonaut.Generic.Util (stripModulePath)
import Data.Either (Either)
import Data.Generic (class Generic)

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
