module Main where

import           Universum

import qualified APISpec as API
import qualified Data.Text as T
import qualified MarshallingSpec as Marshalling
import qualified SwaggerSpec as Swagger
import           System.Info
import           Test.Hspec

-- | Tests whether or not some instances (JSON, Bi, etc) roundtrips.
-- Temporarily disable specs on AppVeyor: https://iohk.myjetbrains.com/youtrack/issue/CSL-2002
main :: IO ()
main = do
    let platform = toText System.Info.os
    case "darwin" `T.isInfixOf` platform || "linux" `T.isInfixOf` platform of
        True -> hspec $ do
            Marshalling.spec
            API.spec
            Swagger.spec
        False -> return ()
