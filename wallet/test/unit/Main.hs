{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Universum

import           Data.Typeable (typeRep)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Pos.Util.Tripping (runTests)
import           Test.QuickCheck

import           Cardano.Wallet.API.V1.Types

import qualified API.MarshallingSpec as Marshalling
import qualified API.RequestSpec as ReqSpec
import qualified API.SwaggerSpec as Swagger
import qualified Golden.APILayout
import qualified Golden.WalletError
import qualified Test.Spec.Accounts
import qualified Test.Spec.Addresses
import qualified Test.Spec.CoinSelection
import qualified Test.Spec.DeltaCompression
import qualified Test.Spec.GetTransactions
import qualified Test.Spec.Kernel
import qualified Test.Spec.Keystore
import qualified Test.Spec.Models
import qualified Test.Spec.NewPayment
import qualified Test.Spec.Submission
import qualified Test.Spec.Translation
import qualified Test.Spec.TxMetaStorage
import qualified Test.Spec.Wallets
import qualified Test.Spec.WalletWorker


-- | Tests whether or not some instances (JSON, Bi, etc) roundtrips.
main :: IO ()
main = do
    -- Golden tests
    runTests
        [ Golden.WalletError.tests
        ]
    hspec Golden.APILayout.spec

    -- API Specs
    hspec $ do
        Marshalling.spec
        Swagger.spec
        ReqSpec.spec

        eqProps @WalletAddress
        eqProps @Address
        eqProps @Wallet
        eqProps @Transaction

    -- New Data-Layer Unit Tests
    hspec $ do
        Test.Spec.Accounts.spec
        Test.Spec.Addresses.spec
        Test.Spec.CoinSelection.spec
        Test.Spec.DeltaCompression.spec
        Test.Spec.GetTransactions.spec
        Test.Spec.Kernel.spec
        Test.Spec.Keystore.spec
        Test.Spec.Models.spec
        Test.Spec.NewPayment.spec
        Test.Spec.Submission.spec
        Test.Spec.Translation.spec
        Test.Spec.TxMetaStorage.spec
        Test.Spec.WalletWorker.spec
        Test.Spec.Wallets.spec

eqProps :: forall a. (Typeable a, Eq a, Arbitrary a, Show a) => Spec
eqProps = do
    describe ("Equality for " ++ show (typeRep (Proxy @a))) $ do
        prop "should be reflexive" $ \(x :: a) ->
            x === x
