{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Universum

import           Data.Typeable (typeRep)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Pos.Util.Parallel.Parallelize (parallelizeAllCores)
import           Test.Pos.Util.Tripping (runTests)
import           Test.QuickCheck

import           Cardano.Wallet.API.V1.Types

import qualified APISpec as API
import qualified MarshallingSpec as Marshalling
import qualified RequestSpec as ReqSpec
import qualified SwaggerSpec as Swagger
import qualified WalletNewJson

-- | Tests whether or not some instances (JSON, Bi, etc) roundtrips.
main :: IO ()
main = do
    parallelizeAllCores
    runTests
        [ WalletNewJson.tests
        ]
    hspec $ do
        parallel $ Marshalling.spec
        parallel $ API.spec
        parallel $ Swagger.spec
        parallel $ ReqSpec.spec

        parallel $ eqProps @WalletAddress
        parallel $ eqProps @Address
        parallel $ eqProps @Wallet
        parallel $ eqProps @Transaction

eqProps :: forall a. (Typeable a, Eq a, Arbitrary a, Show a) => Spec
eqProps = do
    describe ("Equality for " ++ show (typeRep (Proxy @a))) $ do
        prop "should be reflexive" $ \(x :: a) ->
            x === x
