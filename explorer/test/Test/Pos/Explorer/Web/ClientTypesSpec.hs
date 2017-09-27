
-- | This module is testing the ClientTypes module.

{-# LANGUAGE AllowAmbiguousTypes       #-}

module Test.Pos.Explorer.Web.ClientTypesSpec
       ( spec
       ) where

import           Universum

import           Prelude                      (id)

import           Pos.Binary                   (Bi)
import           Pos.Crypto
import           Pos.Explorer.Web.ClientTypes
import           Pos.Txp                      (TxId)
import           Pos.Types                    (Address)
import           Test.Hspec                   (Spec, describe, it, shouldBe,
                                               shouldSatisfy)
import           Test.Hspec.QuickCheck        (modifyMaxSuccess, prop)
import           Test.QuickCheck              (Arbitrary, Gen, Property, arbitrary,
                                               forAll, (===))


----------------------------------------------------------------------------
-- Utility functions
----------------------------------------------------------------------------

-- | Copied from `CborSpec`.
soundInstanceProperty
    :: forall a. (Arbitrary a, Eq a, Show a, Bi a, Bi (Hash a))
    => Property
soundInstanceProperty = forAll (arbitrary :: Gen (Hash a)) $ \input ->
    decodeEncodeHashHex input === True

-- | A reversable function that we can use to test if the hashing works correctly.
decodeEncodeHashHex
    :: forall a. (Bi a, Bi (Hash a))
    => Hash a
    -> Bool
decodeEncodeHashHex hashA = case encodeThenDecode hashA of
    Left _       -> False
    Right hashA' -> hashA == hashA'
  where
    encodeThenDecode = decodeHashHex . encodeHashHex

----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

-- stack test cardano-sl-explorer --fast --test-arguments "-m Test.Pos.Explorer.Web"
spec :: Spec
spec = describe "ClientTypes" $ do
    unitTests
    quickcheckTests

----------------------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------------------

unitTests :: Spec
unitTests = do
    describe "TxId serialization" $ do
        it "should encode Text into TxId and back" $ do
            let cTxIdText = "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"

            let decodedCTxId :: Either Text TxId
                decodedCTxId = decodeHash cTxIdText

            decodedCTxId `shouldSatisfy` isRight

            let result :: Text
                result = either id encodeHashHex decodedCTxId

            result `shouldBe` "b29fa17156275a8589857376bfaeeef47f1846f82ea492a808e5c6155b450e02"

            let decodedResult :: Either Text TxId
                decodedResult = decodeHashHex result

            decodedResult `shouldBe` decodedCTxId

    describe "CAddress serialization" $ do
        it "should encode old Text into CAddress and back" $ do
            let cAddressTextOld = "Sfpj3GbcsazoxEFvidt6rfedaX6PiXnYpYXTfj8hEgXfUFzk1kPWCFEFrecC9iWs7QP7yktEih4YuygF1JitxKze4z3bUFs9J"

            let decodedCAddressTextOld :: Either Text Address
                decodedCAddressTextOld = fromCAddress $ CAddress cAddressTextOld

            decodedCAddressTextOld `shouldSatisfy` isLeft -- shouldn't work

        it "should encode new Text into CAddress and back" $ do
            let cAddressTextNew = "DdzFFzCqrht8wAQiwNCromuPxNjQoK2Cs2vMiVFwFYYAQCcA1nPs7BMXFYhZZVBYhAKexYhaiA8xCUW8EEnc4Wdn6X5zD7R9xcabHip8"
            let cAddress = CAddress cAddressTextNew

            let decodedCAddressTextNew :: Either Text Address
                decodedCAddressTextNew = fromCAddress cAddress

            decodedCAddressTextNew `shouldSatisfy` isRight
            (toCAddress <$> decodedCAddressTextNew) `shouldSatisfy` isRight

----------------------------------------------------------------------------
-- Quickcheck tests
----------------------------------------------------------------------------

quickcheckTests :: Spec
quickcheckTests =
    describe "Hash serialization" $ do
        modifyMaxSuccess (const 10000) $ do
            prop "TxId" (soundInstanceProperty @TxId)
            prop "Address" (soundInstanceProperty @Address)
