{-# LANGUAGE TypeApplications #-}

-- | Pos.Crypto specification

module Test.Pos.CryptoSpec
       ( spec
       ) where

import           Data.Binary              (Binary)
import qualified Data.Binary              as Binary (decode, encode)
import qualified Data.ByteString          as BS
import           Data.String              (String)
import           Formatting               (build, sformat)
import           Test.Hspec               (Expectation, Spec, describe, shouldBe, specify)
import           Test.Hspec.QuickCheck    (modifyMaxSuccess, prop)
import           Test.QuickCheck          (Gen, Property, Testable, forAll, vector, (===),
                                           (==>))
import           Test.QuickCheck.Property (again, ioProperty)
import           Universum

import           Pos.Crypto               (Hash, PublicKey, SecretKey, decrypt,
                                           decryptRaw, encrypt, encryptRaw,
                                           fullPublicKeyF, hash, keyGen,
                                           parseFullPublicKey, sign, toPublic, verify)

spec :: Spec
spec = describe "Crypto" $ do
    describe "hashing" $ do
        describe "Hash instances" $ do
            prop
                "Binary"
                (binaryEncodeDecode @(Hash Int))
        describe "hashes of different values are different" $ do
            prop
                "Bool"
                (hashInequality @Bool)
            prop
                "[()]"
                (hashInequality @[()])
            prop
                "[[Maybe Integer]]"
                (hashInequality @[[Maybe Integer]])
        -- Let's protect ourselves against *accidental* hash changes
        describe "check hash sample" $ do
            specify "1 :: Int" $
                checkHash (1 :: Int)
                    "cd2662154e6d76b2b2b92e70c0cac3cc\
                    \f534f9b74eb5b89819ec509083d00a50"

    describe "PKI" $ do
        -- Generating keys is expensive, so let's say 5 passed test cases for
        -- each property is enough
        modifyMaxSuccess (const 5) $ do
            describe "Binary instances" $ do
                prop
                    "SecretKey"
                    (binaryEncodeDecode @SecretKey)
                prop
                    "PublicKey"
                    (binaryEncodeDecode @PublicKey)
            describe "keys" $ do
                prop
                    "derived pubkey equals to generated pubkey"
                    keyDerivation
                prop
                    "formatted key can be parsed back"
                    keyParsing
            describe "encryption" $ do
                prop
                    "encrypted data can be decrypted successfully"
                    (encryptThenDecrypt @[Int])
                prop1
                    "long data (100kB) can be encrypted"
                    (forAll (randomBS 100000) (flip encryptThenDecrypt))
                prop1
                    "zero-length data can be encrypted"
                    (forAll (return mempty) (flip encryptThenDecryptRaw))
            describe "signing" $ do
                prop
                    "signed data can be verified successfully"
                    (signThenVerify @[Int])
                prop
                    "signed data can't be verified by a different key"
                    (signThenVerifyDifferentKey @[Int])
                prop
                    "modified data can't be verified"
                    (signThenVerifyDifferentData @[Int])

-- Test a property only once.
prop1 :: Testable a => String -> a -> Spec
prop1 s t = modifyMaxSuccess (const 1) $ prop s t

randomBS :: Int -> Gen ByteString
randomBS n = BS.pack <$> vector n

binaryEncodeDecode :: (Show a, Eq a, Binary a) => a -> Property
binaryEncodeDecode a = Binary.decode (Binary.encode a) === a

hashInequality :: (Eq a, Binary a) => a -> a -> Property
hashInequality a b = a /= b ==> hash a /= hash b

checkHash :: Binary a => a -> Text -> Expectation
checkHash x s = sformat build (hash x) `shouldBe` s

keyDerivation :: Property
keyDerivation = again $ ioProperty $ do
    (pk, sk) <- keyGen
    return (pk === toPublic sk)

keyParsing :: PublicKey -> Property
keyParsing pk = ioProperty $ do
    return (parseFullPublicKey (sformat fullPublicKeyF pk) === Just pk)

encryptThenDecrypt
    :: (Eq a, Show a, Binary a)
    => SecretKey -> a -> Property
encryptThenDecrypt sk a = ioProperty $ do
    enc <- encrypt (toPublic sk) a
    dec <- decrypt sk enc
    return (dec === Right a)

encryptThenDecryptRaw
    :: SecretKey -> ByteString -> Property
encryptThenDecryptRaw sk a = ioProperty $ do
    enc <- encryptRaw (toPublic sk) a
    dec <- decryptRaw sk enc
    return (dec === Right a)

signThenVerify
    :: Binary a
    => SecretKey -> a -> Property
signThenVerify sk a = ioProperty $
    verify (toPublic sk) a <$> sign sk a

signThenVerifyDifferentKey
    :: Binary a
    => SecretKey -> PublicKey -> a -> Property
signThenVerifyDifferentKey sk1 pk2 a = ioProperty $ do
    not . verify pk2 a <$> sign sk1 a

signThenVerifyDifferentData
    :: (Eq a, Binary a)
    => SecretKey -> a -> a -> Property
signThenVerifyDifferentData sk a b =
    (a /= b) ==> ioProperty (
        not . verify (toPublic sk) b <$> sign sk a)
