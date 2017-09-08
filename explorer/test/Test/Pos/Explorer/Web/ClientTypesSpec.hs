
-- | This module is testing the ClientTypes module.

{-# LANGUAGE AllowAmbiguousTypes       #-}

module Test.Pos.Explorer.Web.ClientTypesSpec
       ( spec
       ) where

import           Universum

import           Prelude                           (id)

import           Pos.Binary                        (Bi)
import           Pos.Crypto
import           Pos.Explorer.Web.ClientTypes
import           Pos.Txp                           (TxId)
import           Pos.Types                         (Address)
import           Test.Hspec                        (Spec, describe, it, shouldBe,
                                                    shouldSatisfy)
import           Test.Hspec.QuickCheck             (modifyMaxSuccess, prop)
import           Test.QuickCheck                   (Arbitrary, Property, Gen, forAll, arbitrary, (===))

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

----------------------------------------------------------------------------
-- Quickcheck tests
----------------------------------------------------------------------------

quickcheckTests :: Spec
quickcheckTests =
    describe "Hash serialization" $ do
        modifyMaxSuccess (const 10000) $ do
            prop "TxId" (soundInstanceProperty @TxId)
            prop "Address" (soundInstanceProperty @Address)
