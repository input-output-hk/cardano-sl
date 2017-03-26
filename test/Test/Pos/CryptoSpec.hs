-- | Pos.Crypto specification

module Test.Pos.CryptoSpec
       ( spec
       ) where

import qualified Data.ByteString       as BS
import           Formatting            (sformat)
import           Test.Hspec            (Expectation, Spec, describe, it, shouldBe,
                                        specify)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, (===), (==>))
import           Universum

import           Pos.Binary            (AsBinary, Bi)
import qualified Pos.Crypto            as Crypto
import           Pos.Ssc.GodTossing    ()

import           Test.Pos.Util         (binaryEncodeDecode, binaryTest,
                                        safeCopyEncodeDecode, safeCopyTest, serDeserId)

spec :: Spec
spec = describe "Crypto" $ do
    describe "Random" $ do
        -- Let's protect ourselves against *accidental* random gen changes
        -- (e.g. if binary or cryptonite or some other package decide to
        -- behave differently in a new version)
        describe "random number determinism" $ do
            let seed = BS.pack [1..40]
            specify "[0,1)" $
                Crypto.deterministic seed (Crypto.randomNumber 1) `shouldBe` 0
            -- specify "[0,2)" $
            --     deterministic seed (randomNumber 2) `shouldBe` 1
            -- specify "[0,1000)" $
            --     deterministic seed (randomNumber 1000) `shouldBe` 327

    describe "Hashing" $ do
        describe "Hash instances" $ do
            prop
                "Bi"
                (binaryEncodeDecode @(Crypto.Hash Word64))
            prop
                "SafeCopy"
                (safeCopyEncodeDecode @(Crypto.Hash Word64))
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
            specify "1 :: Word64" $
                checkHash (1 :: Word64)
                    -- "009d179ba955ae9b0690b8f6a96a866972b1606d97b0c9d8094073a374de77b7612d4ae35ac3e38f4092aced0f1680295a0bc95722ad039253ee6aa275569848" -- Blake2b_512
                    -- "c43b29d95a3585cb5264b3223d70e853f899a82e01cb3e62b0bdd871" -- Blake2s_224
                    "4bd3a3255713f33d6c673f7d84048a7a8bcfc206464c85555c603ef4d72189c6" -- Blake2s_256

    describe "Signing" $ do
        describe "Identity testing" $ do
            describe "Bi instances" $ do
                binaryTest @Crypto.SecretKey
                binaryTest @Crypto.PublicKey
                binaryTest @(Crypto.Signature ())
                binaryTest @(Crypto.ProxyCert Int32)
                binaryTest @(Crypto.ProxySecretKey Int32)
                binaryTest @(Crypto.ProxySignature Int32 Int32)
                binaryTest @(Crypto.Signed Bool)
                binaryTest @Crypto.RedeemSecretKey
                binaryTest @Crypto.RedeemPublicKey
                binaryTest @(Crypto.RedeemSignature Bool)
                binaryTest @Crypto.VssPublicKey
                binaryTest @(AsBinary Crypto.VssPublicKey)
                binaryTest @(AsBinary Crypto.Secret)
                binaryTest @(AsBinary Crypto.Share)
                binaryTest @(AsBinary Crypto.EncShare)
                binaryTest @(AsBinary Crypto.SecretProof)
                binaryTest @(AsBinary Crypto.SecretSharingExtra)
            describe "SafeCopy instances" $ do
                safeCopyTest @Crypto.SecretKey
                safeCopyTest @Crypto.PublicKey
                safeCopyTest @(Crypto.Signature ())
                safeCopyTest @(Crypto.Signed ())
                safeCopyTest @(Crypto.ProxyCert Int32)
                safeCopyTest @(Crypto.ProxySecretKey Int32)
                safeCopyTest @(Crypto.ProxySignature Int32 Int32)
                safeCopyTest @(Crypto.Signed Bool)
                safeCopyTest @Crypto.RedeemSecretKey
                safeCopyTest @Crypto.RedeemPublicKey
                safeCopyTest @(Crypto.RedeemSignature Bool)
                safeCopyTest @(AsBinary Crypto.VssPublicKey)
                safeCopyTest @(AsBinary Crypto.Secret)
                safeCopyTest @(AsBinary Crypto.Share)
                safeCopyTest @(AsBinary Crypto.EncShare)
                safeCopyTest @(AsBinary Crypto.SecretProof)
                safeCopyTest @(AsBinary Crypto.SecretSharingExtra)
        describe "AsBinaryClass" $ do
            prop "VssPublicKey <-> AsBinary VssPublicKey"
                (serDeserId @Crypto.VssPublicKey)
            prop "Secret <-> AsBinary Secret"
                (serDeserId @Crypto.Secret)
            prop "Share <-> AsBinary Share"
                (serDeserId @Crypto.Share)
            prop "EncShare <-> AsBinary EncShare"
                (serDeserId @Crypto.EncShare)
            prop "SecretProof <-> AsBinary SecretProof"
                (serDeserId @Crypto.SecretProof)
            prop "SecretSharingExtra <-> AsBinary SecretSharingExtra"
                (serDeserId @Crypto.SecretSharingExtra)
        describe "keys" $ do
            it  "derived pubkey equals to generated pubkey"
                keyDerivation
            prop
                "formatted key can be parsed back"
                keyParsing
        describe "signing" $ do
            prop
                "signed data can be verified successfully"
                (signThenVerify @[Int32])
            prop
                "signed data can't be verified by a different key"
                (signThenVerifyDifferentKey @[Int32])
            prop
                "modified data signature can't be verified"
                (signThenVerifyDifferentData @[Int32])
        describe "proxy signature scheme" $ do
            prop
                "signature can be verified successfully"
                (proxySignVerify @[Int32] @(Int32,Int32))
            prop
                "signature can't be verified with a different key"
                (proxySignVerifyDifferentKey @[Int32] @(Int32,Int32))
            prop
                "modified data signature can't be verified "
                (proxySignVerifyDifferentData @[Int32] @(Int32,Int32))
            prop
                "correct proxy signature schemes pass correctness check"
                (proxySecretKeyCheckCorrect @(Int32,Int32))
            prop
                "incorrect proxy signature schemes fails correctness check"
                (proxySecretKeyCheckIncorrect @(Int32,Int32))
        describe "redeemer signatures" $ do
            prop
                "signature can be verified successfully"
                (redeemSignCheck @[Int32])
            prop
                "signature can't be verified with a different key"
                (redeemThenCheckDifferentKey @[Int32])
            prop
                "modified data signature can't be verified "
                (redeemThenCheckDifferentData @[Int32])

        describe "HD wallet" $ do
            prop "pack/unpack address payload" packUnpackHDAddress



hashInequality :: (Eq a, Bi a) => a -> a -> Property
hashInequality a b = a /= b ==> Crypto.hash a /= Crypto.hash b

checkHash :: Bi a => a -> Text -> Expectation
checkHash x s = sformat Crypto.hashHexF (Crypto.hash x) `shouldBe` s

keyDerivation :: Expectation
keyDerivation = do
    (pk, sk) <- Crypto.keyGen
    pk `shouldBe` Crypto.toPublic sk

keyParsing :: Crypto.PublicKey -> Property
keyParsing pk = Crypto.parseFullPublicKey (sformat Crypto.fullPublicKeyF pk) === Just pk

signThenVerify
    :: Bi a
    => Crypto.SecretKey -> a -> Bool
signThenVerify sk a = Crypto.checkSig (Crypto.toPublic sk) a $ Crypto.sign sk a

signThenVerifyDifferentKey
    :: Bi a
    => Crypto.SecretKey -> Crypto.PublicKey -> a -> Property
signThenVerifyDifferentKey sk1 pk2 a =
    (Crypto.toPublic sk1 /= pk2) ==> not (Crypto.checkSig pk2 a $ Crypto.sign sk1 a)

signThenVerifyDifferentData
    :: (Eq a, Bi a)
    => Crypto.SecretKey -> a -> a -> Property
signThenVerifyDifferentData sk a b =
    (a /= b) ==> not (Crypto.checkSig (Crypto.toPublic sk) b $ Crypto.sign sk a)

proxySecretKeyCheckCorrect
    :: (Bi w) => Crypto.SecretKey -> Crypto.SecretKey -> w -> Bool
proxySecretKeyCheckCorrect issuerSk delegateSk w =
    Crypto.verifyProxySecretKey proxySk
  where
    proxySk = Crypto.createProxySecretKey issuerSk (Crypto.toPublic delegateSk) w

proxySecretKeyCheckIncorrect
    :: (Bi w) => Crypto.SecretKey -> Crypto.SecretKey -> Crypto.PublicKey -> w -> Property
proxySecretKeyCheckIncorrect issuerSk delegateSk pk2 w = do
    let Crypto.ProxySecretKey{..} =
            Crypto.createProxySecretKey issuerSk (Crypto.toPublic delegateSk) w
        wrongPsk = Crypto.ProxySecretKey { pskIssuerPk = pk2, ..}
    (Crypto.toPublic issuerSk /= pk2) ==> not (Crypto.verifyProxySecretKey wrongPsk)

proxySignVerify
    :: (Bi a, Bi w, Eq w)
    => Crypto.SecretKey
    -> Crypto.SecretKey
    -> w
    -> a
    -> Bool
proxySignVerify issuerSk delegateSk w m =
    Crypto.proxyVerify issuerPk signature (== w) m
  where
    issuerPk = Crypto.toPublic issuerSk
    proxySk = Crypto.createProxySecretKey issuerSk (Crypto.toPublic delegateSk) w
    signature = Crypto.proxySign delegateSk proxySk m

proxySignVerifyDifferentKey
    :: (Bi a, Bi w, Eq w)
    => Crypto.SecretKey -> Crypto.SecretKey -> Crypto.PublicKey -> w -> a -> Property
proxySignVerifyDifferentKey issuerSk delegateSk pk2 w m =
    (Crypto.toPublic issuerSk /= pk2) ==> not (Crypto.proxyVerify pk2 signature (== w) m)
  where
    proxySk = Crypto.createProxySecretKey issuerSk (Crypto.toPublic delegateSk) w
    signature = Crypto.proxySign delegateSk proxySk m

proxySignVerifyDifferentData
    :: (Bi a, Eq a, Bi w, Eq w)
    => Crypto.SecretKey -> Crypto.SecretKey -> w -> a -> a -> Property
proxySignVerifyDifferentData issuerSk delegateSk w m m2 =
    (m /= m2) ==> not (Crypto.proxyVerify issuerPk signature (== w) m2)
  where
    issuerPk = Crypto.toPublic issuerSk
    proxySk = Crypto.createProxySecretKey issuerSk (Crypto.toPublic delegateSk) w
    signature = Crypto.proxySign delegateSk proxySk m

redeemSignCheck :: Bi a => Crypto.RedeemSecretKey -> a -> Bool
redeemSignCheck redeemerSK a =
    Crypto.redeemCheckSig redeemerPK a $ Crypto.redeemSign redeemerSK a
  where redeemerPK = Crypto.redeemToPublic redeemerSK

redeemThenCheckDifferentKey
    :: Bi a
    => Crypto.RedeemSecretKey -> Crypto.RedeemPublicKey -> a -> Property
redeemThenCheckDifferentKey sk1 pk2 a =
    (Crypto.redeemToPublic sk1 /= pk2) ==>
    not (Crypto.redeemCheckSig pk2 a $ Crypto.redeemSign sk1 a)

redeemThenCheckDifferentData
    :: (Eq a, Bi a)
    => Crypto.RedeemSecretKey -> a -> a -> Property
redeemThenCheckDifferentData sk a b =
    (a /= b) ==>
    not (Crypto.redeemCheckSig (Crypto.redeemToPublic sk) b $ Crypto.redeemSign sk a)

packUnpackHDAddress :: Crypto.HDPassphrase -> [Word32] -> Bool
packUnpackHDAddress passphrase path =
    maybe False (== path) (Crypto.unpackHDAddressAttr passphrase (Crypto.packHDAddressAttr passphrase path))
