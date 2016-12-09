{-# LANGUAGE ScopedTypeVariables #-}
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

import           Pos.Crypto            (EncShare, Hash, KeyPair (..), PublicKey, Secret,
                                        SecretKey, SecretProof, SecretSharingExtra, Share,
                                        Signature, Signed, VssPublicKey, checkSig,
                                        deterministic, fullPublicKeyF, hash,
                                        parseFullPublicKey, randomNumber, sign, toPublic)
import           Pos.Util              (AsBinary)
import           Pos.Ssc.GodTossing    ()

import           Test.Pos.Util         (binaryEncodeDecode, safeCopyEncodeDecode,
                                        serDeserId)

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
            prop
                "SafeCopy"
                (safeCopyEncodeDecode @(Hash Int))
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
                    "009d179ba955ae9b0690b8f6a96a866972b1606d97b0c9d8094073a374de77b7612d4ae35ac3e38f4092aced0f1680295a0bc95722ad039253ee6aa275569848"

    describe "Signing" $ do
        describe "Identity testing" $ do
            describe "Binary instances" $ do
                prop "SecretKey"                (binaryEncodeDecode @SecretKey)
                prop "PublicKey"                (binaryEncodeDecode @PublicKey)
                prop "Signature"                (binaryEncodeDecode @(Signature ()))
                prop "Signed"                   (binaryEncodeDecode @(Signed Bool))
                prop "VssPublicKey"             (binaryEncodeDecode @VssPublicKey)
                prop "AsBinary VssPublicKey"
                    (binaryEncodeDecode @(AsBinary VssPublicKey))
                prop "AsBinary Secret"
                    (binaryEncodeDecode @(AsBinary Secret))
                prop "AsBinary Share"
                    (binaryEncodeDecode @(AsBinary Share))
                prop "AsBinary EncShare"
                    (binaryEncodeDecode @(AsBinary EncShare))
                prop "AsBinary SecretProof"
                    (binaryEncodeDecode @(AsBinary SecretProof))
                prop "AsBinary SecretSharingExtra"
                    (binaryEncodeDecode @(AsBinary SecretSharingExtra))
            describe "SafeCopy instances" $ do
                prop "SecretKey" (safeCopyEncodeDecode @SecretKey)
                prop "PublicKey" (safeCopyEncodeDecode @PublicKey)
                prop "Signature" (safeCopyEncodeDecode @(Signature ()))
                prop "Signed"    (safeCopyEncodeDecode @(Signed Bool))
                prop "AsBinary VssPublicKey"
                    (safeCopyEncodeDecode @(AsBinary VssPublicKey))
                prop "AsBinary Secret"
                    (safeCopyEncodeDecode @(AsBinary Secret))
                prop "AsBinary Share"
                    (safeCopyEncodeDecode @(AsBinary Share))
                prop "AsBinary EncShare"
                    (safeCopyEncodeDecode @(AsBinary EncShare))
                prop "AsBinary SecretProof"
                    (safeCopyEncodeDecode @(AsBinary SecretProof))
                prop "AsBinary SecretSharingExtra"
                    (safeCopyEncodeDecode @(AsBinary SecretSharingExtra))
        describe "Serialized" $ do
            prop "VssPublicKey <-> AsBinary VssPublicKey"
                (serDeserId @VssPublicKey)
            prop "Secret <-> AsBinary Secret"
                (serDeserId @Secret)
            prop "Share <-> AsBinary Share"
                (serDeserId @Share)
            prop "EncShare <-> AsBinary EncShare"
                (serDeserId @EncShare)
            prop "SecretProof <-> AsBinary SecretProof"
                (serDeserId @SecretProof)
            prop "SecretSharingExtra <-> AsBinary SecretSharingExtra"
                (serDeserId @SecretSharingExtra)
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
signThenVerify sk a = checkSig (toPublic sk) a $ sign sk a

signThenVerifyDifferentKey
    :: Binary a
    => SecretKey -> PublicKey -> a -> Property
signThenVerifyDifferentKey sk1 pk2 a =
    (toPublic sk1 /= pk2) ==> not (checkSig pk2 a $ sign sk1 a)

signThenVerifyDifferentData
    :: (Eq a, Binary a)
    => SecretKey -> a -> a -> Property
signThenVerifyDifferentData sk a b =
    (a /= b) ==> not (checkSig (toPublic sk) b $ sign sk a)
