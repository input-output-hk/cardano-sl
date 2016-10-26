{-# LANGUAGE TypeApplications #-}

-- | Pos.Crypto specification

module Test.Pos.CryptoSpec
       ( spec
       ) where

import           Data.Binary           (Binary)
import qualified Data.ByteString       as BS
import           Formatting            (build, sformat)
import           Test.Hspec            (Expectation, Spec, describe, shouldBe, specify)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, (===), (==>))
import           Universum

import           Pos.Crypto            (Hash, KeyPair (..), PublicKey, SecretKey,
                                        Signature, Signed, deterministic, fullPublicKeyF,
                                        hash, parseFullPublicKey, randomNumber, sign,
                                        toPublic, verify)

import           Test.Pos.Util         (binaryEncodeDecode)

spec :: Spec
spec = describe "Crypto" $ do
    describe "Random" $ do
        -- Let's protect ourselves against *accidental* random gen changes
        -- (e.g. if binary or cryptonite or some other package decide to
        -- behave differently in a new version)
        describe "random number determinism" $ do
            let seed = BS.pack [1..40]
            specify "[0,1)" $
                deterministic seed (randomNumber 1) `shouldBe` 0
            -- specify "[0,2)" $
            --     deterministic seed (randomNumber 2) `shouldBe` 1
            -- specify "[0,1000)" $
            --     deterministic seed (randomNumber 1000) `shouldBe` 327

    describe "Hashing" $ do
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

    describe "Signing" $ do
        describe "Binary instances" $ do
            prop
                "SecretKey"
                (binaryEncodeDecode @SecretKey)
            prop
                "PublicKey"
                (binaryEncodeDecode @PublicKey)
            prop
                "Signature"
                (binaryEncodeDecode @(Signature ()))
            prop
                "Signed"
                (binaryEncodeDecode @(Signed Bool))
        describe "keys" $ do
            prop
                "derived pubkey equals to generated pubkey"
                keyDerivation
            prop
                "formatted key can be parsed back"
                keyParsing
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

hashInequality :: (Eq a, Binary a) => a -> a -> Property
hashInequality a b = a /= b ==> hash a /= hash b

checkHash :: Binary a => a -> Text -> Expectation
checkHash x s = sformat build (hash x) `shouldBe` s

keyDerivation :: KeyPair -> Property
keyDerivation kp = getPub kp === toPublic (getSec kp)

keyParsing :: PublicKey -> Property
keyParsing pk = parseFullPublicKey (sformat fullPublicKeyF pk) === Just pk

signThenVerify
    :: Binary a
    => SecretKey -> a -> Bool
signThenVerify sk a = verify (toPublic sk) a $ sign sk a

signThenVerifyDifferentKey
    :: Binary a
    => SecretKey -> PublicKey -> a -> Property
signThenVerifyDifferentKey sk1 pk2 a =
    (toPublic sk1 /= pk2) ==> not (verify pk2 a $ sign sk1 a)

signThenVerifyDifferentData
    :: (Eq a, Binary a)
    => SecretKey -> a -> a -> Property
signThenVerifyDifferentData sk a b =
    (a /= b) ==> not (verify (toPublic sk) b $ sign sk a)
