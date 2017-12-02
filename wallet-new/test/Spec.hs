{-# LANGUAGE CPP #-}
module Main where

#if defined(mingw32_HOST_OS)
import Prelude
main :: IO ()
main = return ()
#else

import           Universum

import qualified APISpec as API
import qualified MarshallingSpec as Marshalling
import qualified SwaggerSpec as Swagger
import           Test.Hspec

-- | Tests whether or not some instances (JSON, Bi, etc) roundtrips.
main :: IO ()
main = hspec $ do
    Marshalling.spec
    API.spec
    Swagger.spec
#endif
