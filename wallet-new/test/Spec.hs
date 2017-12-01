module Main where

import           Universum

import qualified APISpec as API
import qualified MarshallingSpec as Marshalling
import qualified SwaggerSpec as Swagger
import           System.Info
import           Test.Hspec

-- | Tests whether or not some instances (JSON, Bi, etc) roundtrips.
-- Temporarily disable specs on AppVeyor: https://iohk.myjetbrains.com/youtrack/issue/CSL-2002
main :: IO ()
main = case System.Info.os of
    "windows" -> return ()
    _ -> hspec $ do
        Marshalling.spec
        API.spec
        Swagger.spec
