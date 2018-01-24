-- | Pos.Crypto specification

module Test.Pos.CryptoSpec
       ( spec
       ) where

import           Crypto.Hash (Blake2b_224, Blake2b_256)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import           Formatting (sformat)
import           Prelude ((!!))
import           Test.Hspec (Expectation, Spec, describe, it, shouldBe, specify)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), Gen, Property, ioProperty, property, vector,
                                  (===), (==>))
import           Test.QuickCheck.Monadic (assert, monadicIO, run)
import           Universum

import           Pos.Arbitrary.Crypto (SharedSecrets (..))
import           Pos.Binary (AsBinary, Bi)
import           Pos.Core (HasConfiguration)
import qualified Pos.Crypto as Crypto
import           Pos.SafeCopy ()
import           Pos.Ssc ()

import           Test.Pos.Cbor.CborSpec (U)
import           Test.Pos.Helpers (binaryEncodeDecode, binaryTest, msgLenLimitedTest,
                                   safeCopyEncodeDecode, safeCopyTest, serDeserId, (.=.))
import           Test.Pos.Util (qcIsLeft, withDefConfiguration, withDefInfraConfiguration)

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

spec :: Spec
spec = withDefInfraConfiguration $ withDefConfiguration $ describe "Crypto" $ do
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
            prop "Bi" (binaryEncodeDecode @(Crypto.Hash Word64))
            prop "SafeCopy" (safeCopyEncodeDecode @(Crypto.Hash Word64))
        describe "hashes of different values are different" $ do
            prop "Bool" (hashInequality @Bool)
            prop "[()]" (hashInequality @[()])
            prop "[[Maybe Integer]]" (hashInequality @[[Maybe Integer]])
        -- Let's protect ourselves against *accidental* hash changes
        describe "check hash sample" $ do
            specify "1 :: Word64" $
                checkHash (1 :: Word64)
                    -- "009d179ba955ae9b0690b8f6a96a866972b1606d97b0c9d8094073a374de77b7612d4ae35ac3e38f4092aced0f1680295a0bc95722ad039253ee6aa275569848" -- Blake2b_512
                    -- "c43b29d95a3585cb5264b3223d70e853f899a82e01cb3e62b0bdd871" -- Blake2s_224
                    -- "4bd3a3255713f33d6c673f7d84048a7a8bcfc206464c85555c603ef4d72189c6" -- Blake2s_256
                    --"12dd0a6a7d0e222a97926da03adb5a7768d31cc7c5c2bd6828e14a7d25fa3a60" -- Blake2b_256, before switching to different endianness
                    --"1dbd7d0b561a41d23c2a469ad42fbd70d5438bae826f6fd607413190c37c363b" -- Before switching to CBOR
                    "ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25"
        describe "decodeAbstractHash" $ do
            it "decodes hash formatted using 'hashHexF'" $ do
                let someHash = Crypto.hash (True, (), False, ())
                Crypto.decodeAbstractHash (sformat Crypto.hashHexF someHash)
                    == Right someHash

    describe "Signing" $ do
        describe "SafeSigning" $ do
            prop "passphrase matches" matchingPassphraseWorks
            prop "passphrase doesn't match" mismatchingPassphraseFails
            prop -- if you see this case failing, then passphrase changing endpoint
                 -- in wallets have to be reconsidered
                 "passphrase change doesn't modify key address"
                 passphraseChangeLeavesAddressUnmodified

        describe "Identity testing" $ do
            describe "Bi instances" $ do
                binaryTest @Crypto.SecretKey
                binaryTest @Crypto.PublicKey
                binaryTest @(Crypto.Signature ())
                binaryTest @(Crypto.Signature U)
                binaryTest @(Crypto.ProxyCert Int32)
                binaryTest @(Crypto.ProxySecretKey Int32)
                binaryTest @(Crypto.ProxySecretKey U)
                binaryTest @(Crypto.ProxySignature Int32 Int32)
                binaryTest @(Crypto.ProxySignature U U)
                binaryTest @(Crypto.Signed Bool)
                binaryTest @(Crypto.Signed U)
                binaryTest @Crypto.RedeemSecretKey
                binaryTest @Crypto.RedeemPublicKey
                binaryTest @(Crypto.RedeemSignature Bool)
                binaryTest @(Crypto.RedeemSignature U)
                binaryTest @Crypto.Threshold
                binaryTest @Crypto.VssPublicKey
                binaryTest @Crypto.PassPhrase
                binaryTest @Crypto.VssKeyPair
                binaryTest @Crypto.Secret
                binaryTest @Crypto.DecShare
                binaryTest @Crypto.EncShare
                binaryTest @Crypto.SecretProof
                binaryTest @Crypto.HDAddressPayload
                binaryTest @(Crypto.AbstractHash Blake2b_224 U)
                binaryTest @(Crypto.AbstractHash Blake2b_256 U)
                binaryTest @(AsBinary Crypto.VssPublicKey)
                binaryTest @(AsBinary Crypto.Secret)
                binaryTest @(AsBinary Crypto.DecShare)
                binaryTest @(AsBinary Crypto.EncShare)
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
                safeCopyTest @Crypto.Threshold
                safeCopyTest @(AsBinary Crypto.VssPublicKey)
                safeCopyTest @(AsBinary Crypto.Secret)
                safeCopyTest @(AsBinary Crypto.DecShare)
                safeCopyTest @(AsBinary Crypto.EncShare)
            describe "msgLenLimitedTest" $ do
                msgLenLimitedTest @Crypto.PublicKey
                msgLenLimitedTest @Crypto.EncShare
                msgLenLimitedTest @Crypto.Secret
                -- msgLenLimitedTest @(C.MaxSize SecretProof)
                msgLenLimitedTest @(Crypto.Signature ())
                msgLenLimitedTest @(Crypto.AbstractHash Blake2b_224 Void)
                msgLenLimitedTest @(Crypto.AbstractHash Blake2b_256 Void)
                msgLenLimitedTest @Crypto.VssPublicKey
                msgLenLimitedTest @Crypto.DecShare

        describe "AsBinaryClass" $ do
            prop "VssPublicKey <-> AsBinary VssPublicKey"
                (serDeserId @Crypto.VssPublicKey)
            prop "Secret <-> AsBinary Secret"
                (serDeserId @Crypto.Secret)
            prop "DecShare <-> AsBinary DecShare"
                (serDeserId @Crypto.DecShare)
            prop "EncShare <-> AsBinary EncShare"
                (serDeserId @Crypto.EncShare)
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
            prop "decryptChaCha . encryptChaCha = id" encrypyDecryptChaChaPoly
            prop
                "signed data can't be verified with a different key"
                encrypyDecryptChaChaDifferentKey
            prop
                "signed data can't be verified with a different header"
                encrypyDecryptChaChaDifferentHeader
            prop
                "signed data can't be verified with a different nonce"
                encrypyDecryptChaChaDifferentNonce

        describe "Safe Signing" $ do
            prop
                "turning a secret key into an encrypted secret key and this encrypted\
                 \ secret key into a public key is the same as turning the secret key\
                 \ into a public key"
                 encToPublicToEnc
            prop
                "turning a secret key into an safe signer and this safe signer into a\
                 \ public key is the same as turning the secret key into a public key"
                 skToSafeSigner

        describe "Secret Sharing" $ do
            prop
                "VSS key pairs generated from different 'ByteString' seeds are different"
                keygenInequality
            prop
                "successfully verifies correct decryption of an encrypted share"
                goodShareIsDecrypted
            prop
                "verifying encrypted shares works"
                verifyEncSharesGoodData
            prop
                "verifying encrypted shares with invalid secret proof fails"
                verifyEncSharesBadSecProof
            prop
                "verifying encrypted shares with mismatching VSS keys fails"
                verifyEncSharesWrongKeys
            prop
                "successfully verifies a properly decrypted share"
                verifyShareGoodData
            prop
                "verifying a correctly decrypted share with the wrong public key fails"
                verifyShareBadShare
            prop
                "verifying a correctly decrypted share with a mismatching encrypted\
                \ share fails"
                verifyShareMismatchingShares
            prop
                "successfully verifies a secret proof with its secret"
                verifyProofGoodData
            prop
                "unsuccessfully verifies an invalid secret with an unrelated secret\
                \ proof"
                verifyProofBadSecret
            prop
                "unsuccessfully verifies a secret with an invalid proof"
                verifyProofBadSecProof
            prop
                "successfully recover a secret given a list of encrypted shares, those\
                \ decrypted shares and their corresponding VSS public keys"
                recoverSecretSuccessfully
            prop
                "unsuccessfully recovers a secret with insufficient shares for the given\
                \ threshold"
                recoverSecBadThreshold

hashInequality :: (Eq a, Bi a) => a -> a -> Property
hashInequality a b = a /= b ==> Crypto.hash a /= Crypto.hash b

checkHash :: Bi a => a -> Text -> Expectation
checkHash x s = sformat Crypto.hashHexF (Crypto.hash x) `shouldBe` s

keyDerivation :: Expectation
keyDerivation = do
    (pk, sk) <- Crypto.keyGen
    pk `shouldBe` Crypto.toPublic sk

keyParsing :: Crypto.PublicKey -> Property
keyParsing pk = Crypto.parseFullPublicKey (sformat Crypto.fullPublicKeyF pk) === Right pk

signThenVerify
    :: Bi a
    => HasConfiguration => Crypto.SignTag -> Crypto.SecretKey -> a -> Bool
signThenVerify t sk a = Crypto.checkSig t (Crypto.toPublic sk) a $ Crypto.sign t sk a

signThenVerifyDifferentKey
    :: Bi a
    => HasConfiguration => Crypto.SignTag -> Crypto.SecretKey -> Crypto.PublicKey -> a -> Property
signThenVerifyDifferentKey t sk1 pk2 a =
    (Crypto.toPublic sk1 /= pk2) ==> not (Crypto.checkSig t pk2 a $ Crypto.sign t sk1 a)

signThenVerifyDifferentData
    :: (Eq a, Bi a)
    => HasConfiguration => Crypto.SignTag -> Crypto.SecretKey -> a -> a -> Property
signThenVerifyDifferentData t sk a b =
    (a /= b) ==> not (Crypto.checkSig t (Crypto.toPublic sk) b $ Crypto.sign t sk a)

proxySecretKeyCheckCorrect
    :: (HasConfiguration, Bi w) => Crypto.SecretKey -> Crypto.SecretKey -> w -> Bool
proxySecretKeyCheckCorrect issuerSk delegateSk w =
    isRight (Crypto.validateProxySecretKey proxySk)
  where
    proxySk = Crypto.createPsk issuerSk (Crypto.toPublic delegateSk) w

proxySecretKeyCheckIncorrect
    :: (HasConfiguration, Bi w) => Crypto.SecretKey -> Crypto.SecretKey -> Crypto.PublicKey -> w -> Property
proxySecretKeyCheckIncorrect issuerSk delegateSk pk2 w = do
    let Crypto.UnsafeProxySecretKey{..} =
            Crypto.createPsk issuerSk (Crypto.toPublic delegateSk) w
        wrongPsk = Crypto.UnsafeProxySecretKey { Crypto.pskIssuerPk = pk2, ..}
    (Crypto.toPublic issuerSk /= pk2) ==>
        isLeft (Crypto.validateProxySecretKey wrongPsk)

proxySignVerify
    :: (HasConfiguration, Bi a, Bi w, Eq w)
    => Crypto.SecretKey
    -> Crypto.SecretKey
    -> w
    -> a
    -> Bool
proxySignVerify issuerSk delegateSk w m =
    Crypto.proxyVerify Crypto.SignForTestingOnly signature (== w) m
  where
    proxySk = Crypto.createPsk issuerSk (Crypto.toPublic delegateSk) w
    signature = Crypto.proxySign Crypto.SignForTestingOnly delegateSk proxySk m

proxySignVerifyDifferentKey
    :: (HasConfiguration, Bi a, Bi w, Eq w)
    => Crypto.SecretKey -> Crypto.SecretKey -> Crypto.PublicKey -> w -> a -> Property
proxySignVerifyDifferentKey issuerSk delegateSk pk2 w m =
    (Crypto.toPublic issuerSk /= pk2) ==>
    not (Crypto.proxyVerify Crypto.SignForTestingOnly sigBroken (== w) m)
  where
    proxySk = Crypto.createPsk issuerSk (Crypto.toPublic delegateSk) w
    signature = Crypto.proxySign Crypto.SignForTestingOnly delegateSk proxySk m
    sigBroken = signature { Crypto.psigPsk = proxySk { Crypto.pskIssuerPk = pk2 } }

proxySignVerifyDifferentData
    :: (HasConfiguration, Bi a, Eq a, Bi w, Eq w)
    => Crypto.SecretKey -> Crypto.SecretKey -> w -> a -> a -> Property
proxySignVerifyDifferentData issuerSk delegateSk w m m2 =
    (m /= m2) ==>
    not (Crypto.proxyVerify Crypto.SignForTestingOnly signature (== w) m2)
  where
    proxySk = Crypto.createPsk issuerSk (Crypto.toPublic delegateSk) w
    signature = Crypto.proxySign Crypto.SignForTestingOnly delegateSk proxySk m

redeemSignCheck :: (HasConfiguration, Bi a) => Crypto.RedeemSecretKey -> a -> Bool
redeemSignCheck redeemerSK a =
    Crypto.redeemCheckSig Crypto.SignForTestingOnly redeemerPK a $
    Crypto.redeemSign Crypto.SignForTestingOnly redeemerSK a
  where
    redeemerPK = Crypto.redeemToPublic redeemerSK

redeemThenCheckDifferentKey
    :: Bi a
    => HasConfiguration => Crypto.RedeemSecretKey -> Crypto.RedeemPublicKey -> a -> Property
redeemThenCheckDifferentKey sk1 pk2 a =
    (Crypto.redeemToPublic sk1 /= pk2) ==>
    not (Crypto.redeemCheckSig Crypto.SignForTestingOnly pk2 a $
         Crypto.redeemSign Crypto.SignForTestingOnly sk1 a)

redeemThenCheckDifferentData
    :: (HasConfiguration, Eq a, Bi a)
    => Crypto.RedeemSecretKey -> a -> a -> Property
redeemThenCheckDifferentData sk a b =
    (a /= b) ==>
    not (Crypto.redeemCheckSig Crypto.SignForTestingOnly (Crypto.redeemToPublic sk) b $
         Crypto.redeemSign Crypto.SignForTestingOnly sk a)

packUnpackHDAddress :: Crypto.HDPassphrase -> [Word32] -> Bool
packUnpackHDAddress passphrase path =
    maybe False (== path) $
    Crypto.unpackHDAddressAttr passphrase (Crypto.packHDAddressAttr passphrase path)

newtype Nonce = Nonce ByteString
    deriving (Show, Eq)

instance Arbitrary Nonce where
    arbitrary = Nonce . BS.pack <$> vector 12

encrypyDecryptChaChaPoly
    :: Nonce
    -> Crypto.HDPassphrase
    -> ByteString
    -> ByteString
    -> Bool
encrypyDecryptChaChaPoly (Nonce nonce) (Crypto.HDPassphrase key) header plaintext =
    (decrypt =<< (Crypto.toEither . encrypt $ plaintext)) == Right plaintext
  where
    encrypt = Crypto.encryptChaChaPoly nonce key header
    decrypt = Crypto.decryptChaChaPoly nonce key header

encrypyDecryptChaChaDifferentKey
    :: Nonce
    -> Crypto.HDPassphrase
    -> Crypto.HDPassphrase
    -> ByteString
    -> ByteString
    -> Property
encrypyDecryptChaChaDifferentKey
    (Nonce nonce)
    (Crypto.HDPassphrase key1)
    (Crypto.HDPassphrase key2)
    header
    plaintext =
    (key1 /= key2) ==>
    qcIsLeft (decrypt =<< (Crypto.toEither . encrypt $ plaintext))
  where
    encrypt = Crypto.encryptChaChaPoly nonce key1 header
    decrypt = Crypto.decryptChaChaPoly nonce key2 header

encrypyDecryptChaChaDifferentHeader
    :: Nonce
    -> Crypto.HDPassphrase
    -> ByteString
    -> ByteString
    -> ByteString
    -> Property
encrypyDecryptChaChaDifferentHeader
    (Nonce nonce)
    (Crypto.HDPassphrase key)
    header1
    header2
    plaintext =
    (header1 /= header2) ==>
    qcIsLeft (decrypt =<< (Crypto.toEither . encrypt $ plaintext))
  where
    encrypt = Crypto.encryptChaChaPoly nonce key header1
    decrypt = Crypto.decryptChaChaPoly nonce key header2

encrypyDecryptChaChaDifferentNonce
    :: Nonce
    -> Nonce
    -> Crypto.HDPassphrase
    -> ByteString
    -> ByteString
    -> Property
encrypyDecryptChaChaDifferentNonce
    (Nonce nonce1)
    (Nonce nonce2)
    (Crypto.HDPassphrase key)
    header
    plaintext =
    (nonce1 /= nonce2) ==>
    qcIsLeft (decrypt =<< (Crypto.toEither . encrypt $ plaintext))
  where
    encrypt = Crypto.encryptChaChaPoly nonce1 key header
    decrypt = Crypto.decryptChaChaPoly nonce2 key header

encToPublicToEnc :: Crypto.SecretKey -> Property
encToPublicToEnc =
    Crypto.encToPublic . Crypto.noPassEncrypt .=. Crypto.toPublic

skToSafeSigner :: Crypto.SecretKey -> Property
skToSafeSigner =
    Crypto.safeToPublic . Crypto.fakeSigner .=. Crypto.toPublic

-- | It appears 'Crypto.deterministicVSsKeyGen' ignores all consecutive null
-- characters starting from the beginning of the 'ByteString' seed, so for
-- this property to make sense they need to be dropped.
keygenInequality :: ByteString -> ByteString -> Property
keygenInequality a b =
    (a' /= b') ==> Crypto.deterministicVssKeyGen a' /= Crypto.deterministicVssKeyGen b'
  where
    dropNULs = BS.dropWhile ((== "\NUL") . BS.singleton)
    a' = dropNULs a
    b' = dropNULs b

goodShareIsDecrypted :: Crypto.VssKeyPair -> Crypto.EncShare -> Property
goodShareIsDecrypted vssKP encShare = monadicIO $ do
    decShare <- run $ Crypto.decryptShare vssKP encShare
    assert $ Crypto.verifyDecShare (Crypto.toVssPublicKey vssKP) encShare decShare

verifyEncSharesGoodData :: SharedSecrets -> Property
verifyEncSharesGoodData SharedSecrets {..} =
    property $
    Crypto.verifyEncShares @Gen ssSecProof ssThreshold
        (zip ssVssKeys ssEncShares)

verifyEncSharesBadSecProof :: SharedSecrets -> Crypto.SecretProof -> Property
verifyEncSharesBadSecProof SharedSecrets {..} secProof =
    (ssSecProof /= secProof) ==>
    not <$> Crypto.verifyEncShares @Gen secProof ssThreshold
                (zip ssVssKeys ssEncShares)

verifyEncSharesWrongKeys :: SharedSecrets -> Property
verifyEncSharesWrongKeys SharedSecrets {..} =
    (headMay ssVssKeys /= lastMay ssVssKeys) ==>
    not <$> Crypto.verifyEncShares @Gen ssSecProof ssThreshold
               (zip (reverse ssVssKeys) ssEncShares)

verifyShareGoodData :: SharedSecrets -> Bool
verifyShareGoodData SharedSecrets {..} =
    Crypto.verifyDecShare vssPK encShare decShare
  where
    encShare = ssEncShares !! ssPos
    decShare = ssDecShares !! ssPos
    vssPK = ssVssKeys !! ssPos

verifyShareBadShare :: SharedSecrets -> Int -> Property
verifyShareBadShare SharedSecrets {..} p =
    (s1 /= s2 || vssPK1 /= vssPK2) ==>
    not (Crypto.verifyDecShare vssPK1 encShare1 decShare1) &&
    not (Crypto.verifyDecShare vssPK2 encShare2 decShare2)
  where
    len = length ssVssKeys
    pos = abs $ p `mod` len
    s1@(encShare1, decShare1) = zip ssEncShares ssDecShares !! ssPos
    s2@(encShare2, decShare2) = zip ssEncShares ssDecShares !! pos
    vssPK1 = ssVssKeys !! pos
    vssPK2 = ssVssKeys !! ssPos

verifyShareMismatchingShares :: SharedSecrets -> Int -> Property
verifyShareMismatchingShares SharedSecrets {..} p =
    (vssPK1 /= vssPK2) ==>
    not (Crypto.verifyDecShare vssPK1 encShare1 decShare2)
  where
    len = length ssVssKeys
    pos = abs $ p `mod` len
    encShare1 = ssEncShares !! ssPos
    decShare2 = ssDecShares !! pos
    vssPK1 = ssVssKeys !! pos
    vssPK2 = ssVssKeys !! ssPos

verifyProofGoodData :: SharedSecrets -> Bool
verifyProofGoodData SharedSecrets {..} =
    Crypto.verifySecret ssThreshold ssSecProof ssSecret

verifyProofBadSecret :: SharedSecrets -> Crypto.Secret -> Property
verifyProofBadSecret SharedSecrets {..} secret =
    (ssSecret /= secret) ==>
    not (Crypto.verifySecret ssThreshold ssSecProof secret)

verifyProofBadSecProof :: SharedSecrets -> Crypto.SecretProof -> Property
verifyProofBadSecProof SharedSecrets {..} secProof =
    (ssSecProof /= secProof) ==>
    not (Crypto.verifySecret ssThreshold secProof ssSecret)

recoverSecretSuccessfully :: SharedSecrets -> Property
recoverSecretSuccessfully SharedSecrets {..} =
    Crypto.recoverSecret ssThreshold keys shares === Just ssSecret
  where
    keys = map (,1) ssVssKeys
    shares = HM.fromList $ zip ssVssKeys (map one ssDecShares)

recoverSecBadThreshold :: SharedSecrets -> Integer -> Property
recoverSecBadThreshold SharedSecrets {..} rnd =
    (badThreshold > ssThreshold) ==>
    isNothing (Crypto.recoverSecret badThreshold keys shares)
  where
    maxThreshold = genericLength ssEncShares
    -- badThreshold is in ]actualThreshold, actualThreshold * 2]
    badThreshold = maxThreshold + (succ . abs $ rnd `mod` maxThreshold)
    keys = map (,1) ssVssKeys
    shares = HM.fromList $ zip ssVssKeys (map one ssDecShares)

matchingPassphraseWorks :: Crypto.PassPhrase -> Property
matchingPassphraseWorks passphrase = ioProperty $ do
    (_, key) <- Crypto.safeKeyGen passphrase
    Crypto.withSafeSigner key (return passphrase) (return . isJust)

mismatchingPassphraseFails
    :: Crypto.PassPhrase
    -> Crypto.PassPhrase
    -> Property
mismatchingPassphraseFails genPass signPass = ioProperty $ do
    (_, key) <- Crypto.safeKeyGen genPass
    Crypto.withSafeSigner key (return signPass) $ \signer ->
        return $ genPass /= signPass ==> property (isNothing signer)

passphraseChangeLeavesAddressUnmodified
    :: Crypto.PassPhrase
    -> Crypto.PassPhrase
    -> Property
passphraseChangeLeavesAddressUnmodified oldPass newPass = ioProperty $ do
    (_, oldKey) <- Crypto.safeKeyGen oldPass
    newKey <- fromMaybe (error "Passphrase didn't match") <$>
              Crypto.changeEncPassphrase oldPass newPass oldKey
    return $ Crypto.encToPublic oldKey === Crypto.encToPublic newKey
