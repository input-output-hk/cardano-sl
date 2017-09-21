
-- | This module is testing the ClientTypes module.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Pos.Explorer.Web.ClientTypesSpec
       ( spec
       ) where

import           Universum

import           Prelude                      (id)

import           Crypto.Hash                  (Blake2b_224, Blake2b_256)
import           Pos.Binary                   (Bi)
import           Pos.Crypto
import           Pos.Explorer.Web.ClientTypes (decodeHashHex, encodeHashHex, fromCAddress,
                                               fromCHash, fromCTxId, toCAddress, toCHash,
                                               toCTxId)
import           Pos.Txp                      (Tx, TxId)
import           Pos.Types                    (Address)
import           Test.Hspec                   (Spec, describe, it, shouldBe,
                                               shouldSatisfy)
import           Test.Hspec.QuickCheck        (modifyMaxSuccess, prop)
import           Test.QuickCheck              (Arbitrary, Gen, Property, arbitrary,
                                               forAll, (===))



----------------------------------------------------------------------------
-- Utility functions
----------------------------------------------------------------------------

-- | Generalized property for any @AbstractHash@. Original copied from @CborSpec@.
soundAbstractHashInstanceProperty
    :: forall algo a. (Arbitrary a, Typeable algo, HashAlgorithm algo, Bi a)
    => (AbstractHash algo a -> Either Text (AbstractHash algo a))
    -> Property
soundAbstractHashInstanceProperty reversableFunction =
    forAll (arbitrary :: Gen (AbstractHash algo a)) $ \input ->
        decodeEncodeHashHex input === True
  where
    -- | A reversable function that we can use to test if the hashing works correctly.
    decodeEncodeHashHex
        :: AbstractHash algo a
        -> Bool
    decodeEncodeHashHex hashA = case reversableFunction hashA of
        Left _       -> False
        Right hashA' -> hashA == hashA'

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

-- type AddressHash   = AbstractHash Blake2b_224
-- type Hash          = AbstractHash Blake2b_256
-- type TxId          = Hash Tx = AbstractHash Blake2b_256 Tx
-- type StakeholderId = AddressHash PublicKey = AbstractHash Blake2b_224 PublicKey

quickcheckTests :: Spec
quickcheckTests =
    describe "Hash serialization" $ do
        modifyMaxSuccess (const 10000) $ do
            prop
                "TxId"
                (soundAbstractHashInstanceProperty
                     @Blake2b_256
                     @Tx
                     reversableAbstractHashFunction)
            prop
                "Address"
                (soundAbstractHashInstanceProperty
                     @Blake2b_224
                     @Address
                     reversableAbstractHashFunction)
            prop
                "PublicKey"
                (soundAbstractHashInstanceProperty
                     @Blake2b_224
                     @PublicKey
                     reversableAbstractHashFunction)
            -- This property has a function that operates on @Hash@ so we don't need
            -- to instruct it to use a specific algorithm (Blake2b_256) as it will be
            -- inferred, we just need to reify/specify the free @a@ variable.
            prop
                "CHash"
                (soundAbstractHashInstanceProperty
                     @_
                     @Tx
                     reversableCHashFunction)
            -- This property has a function that operates on @Tx@ so we don't need
            -- to instruct it to use a specific algorithm (Blake2b_256) or a type
            -- variable as it will be inferred.
            prop
                "TxId"
                (soundAbstractHashInstanceProperty reversableCAddressFunction)
            prop "CAdress" propertyCAddressReversable
  where
    -- | This is the most general abstract hash function from source to client and back.
    reversableAbstractHashFunction
        :: forall a algo. (Typeable algo, HashAlgorithm algo, Bi a)
        => AbstractHash algo a
        -> Either Text (AbstractHash algo a)
    reversableAbstractHashFunction = decodeHashHex . encodeHashHex

    -- | This is a more specific hash function that deals with @Hash a@ which is
    -- actually @forall a. AbstractHash Blake2b_256 a@.
    reversableCHashFunction
        :: forall a. (Bi a)
        => Hash a
        -> Either Text (Hash a)
    reversableCHashFunction = fromCHash . toCHash

    -- | This is a very specific function that already covers all the free variables
    -- from @AbstractHash algo a@ with @AbstractHash Blake2b_256 Tx@.
    reversableCAddressFunction :: TxId -> Either Text TxId
    reversableCAddressFunction = fromCTxId . toCTxId

    -- | This is a specific property that we use to test valid @Address@ transformation.
    propertyCAddressReversable :: Property
    propertyCAddressReversable =
        forAll (arbitrary :: Gen Address) $ \input ->
            decodeEncodeHashHex input === True
      where
        -- | A reversable function that tests if the transformation works correctly.
        decodeEncodeHashHex :: Address -> Bool
        decodeEncodeHashHex hashA =
            case fromCAddress . toCAddress $ hashA of
                Left _       -> False
                Right hashA' -> hashA == hashA'
