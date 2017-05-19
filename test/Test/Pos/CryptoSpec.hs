-- | Pos.Crypto specification

module Test.Pos.CryptoSpec
       ( spec
       ) where

import qualified Data.ByteString         as BS
import           Formatting              (sformat)
import           Prelude                 ((!!))
import           Test.Hspec              (Expectation, Spec, describe, it, shouldBe,
                                          specify)
import           Test.Hspec.QuickCheck   (prop)
import           Test.QuickCheck         (Arbitrary (..), Property, ioProperty, property,
                                          vector, (===), (==>))
import           Test.QuickCheck.Monadic (assert, monadicIO, run)
import           Universum

import           Pos.Binary              (AsBinary, Bi)
import qualified Pos.Crypto              as Crypto
import           Pos.Crypto.Arbitrary    (SharedSecrets (..))
import           Pos.Ssc.GodTossing      ()

import           Test.Pos.Util           (binaryEncodeDecode, binaryTest,
                                          safeCopyEncodeDecode, safeCopyTest, serDeserId,
                                          (.=.))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

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
                    -- "4bd3a3255713f33d6c673f7d84048a7a8bcfc206464c85555c603ef4d72189c6" -- Blake2s_256
                    "12dd0a6a7d0e222a97926da03adb5a7768d31cc7c5c2bd6828e14a7d25fa3a60" -- Blake2b_256

    describe "Signing" $ do
        describe "SafeSigning" $ do
            prop
                "passphrase matches"
                matchingPassphraseWorks
            prop
                "passphrase doesn't match"
                mismatchingPassphraseFails
            prop
                -- if you see this case failing, then passphrase changing endpoint
                -- in wallets have to be reconsidered
                "passphrase change doesn't modify key address"
                passphraseChangeLeavesAddressUnmodified

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
                binaryTest @Crypto.Threshold
                binaryTest @Crypto.VssPublicKey
                binaryTest @Crypto.PassPhrase
                binaryTest @Crypto.VssKeyPair
                binaryTest @Crypto.Secret
                binaryTest @Crypto.Share
                binaryTest @Crypto.EncShare
                binaryTest @Crypto.SecretProof
                binaryTest @Crypto.SecretSharingExtra
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
                safeCopyTest @Crypto.Threshold
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
                " VSS key pairs generated from different 'ByteString' seeds are different"
                keygenInequality
            prop
                "successfully verifies correct decryption of an encrypted share"
                goodShareIsDecrypted
            prop
                "verifying an encrypted share with a valid VSS public key and valid extra\
                \ secret information works"
                verifyEncShareGoodData
            prop
                "verifying an encrypted share with a valid VSS public key and invalid\
                \ extra secret information fails"
                verifyEncShareBadSecShare
            prop
                "verifying an encrypted share with a mismatching VSS public key fails"
                verifyEncShareMismatchShareKey
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
                "unsuccessfully verifies a valid secret with a valid proof when given\
                \ invalid secret sharing extra data"
                verifyProofBadSecShare
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
keyParsing pk = Crypto.parseFullPublicKey (sformat Crypto.fullPublicKeyF pk) === Just pk

signThenVerify
    :: Bi a
    => Crypto.SignTag -> Crypto.SecretKey -> a -> Bool
signThenVerify t sk a = Crypto.checkSig t (Crypto.toPublic sk) a $ Crypto.sign t sk a

signThenVerifyDifferentKey
    :: Bi a
    => Crypto.SignTag -> Crypto.SecretKey -> Crypto.PublicKey -> a -> Property
signThenVerifyDifferentKey t sk1 pk2 a =
    (Crypto.toPublic sk1 /= pk2) ==> not (Crypto.checkSig t pk2 a $ Crypto.sign t sk1 a)

signThenVerifyDifferentData
    :: (Eq a, Bi a)
    => Crypto.SignTag -> Crypto.SecretKey -> a -> a -> Property
signThenVerifyDifferentData t sk a b =
    (a /= b) ==> not (Crypto.checkSig t (Crypto.toPublic sk) b $ Crypto.sign t sk a)

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
    Crypto.proxyVerify Crypto.SignForTestingOnly issuerPk signature (== w) m
  where
    issuerPk = Crypto.toPublic issuerSk
    proxySk = Crypto.createProxySecretKey issuerSk (Crypto.toPublic delegateSk) w
    signature = Crypto.proxySign Crypto.SignForTestingOnly delegateSk proxySk m

proxySignVerifyDifferentKey
    :: (Bi a, Bi w, Eq w)
    => Crypto.SecretKey -> Crypto.SecretKey -> Crypto.PublicKey -> w -> a -> Property
proxySignVerifyDifferentKey issuerSk delegateSk pk2 w m =
    (Crypto.toPublic issuerSk /= pk2) ==>
    not (Crypto.proxyVerify Crypto.SignForTestingOnly pk2 signature (== w) m)
  where
    proxySk = Crypto.createProxySecretKey issuerSk (Crypto.toPublic delegateSk) w
    signature = Crypto.proxySign Crypto.SignForTestingOnly delegateSk proxySk m

proxySignVerifyDifferentData
    :: (Bi a, Eq a, Bi w, Eq w)
    => Crypto.SecretKey -> Crypto.SecretKey -> w -> a -> a -> Property
proxySignVerifyDifferentData issuerSk delegateSk w m m2 =
    (m /= m2) ==>
    not (Crypto.proxyVerify Crypto.SignForTestingOnly issuerPk signature (== w) m2)
  where
    issuerPk = Crypto.toPublic issuerSk
    proxySk = Crypto.createProxySecretKey issuerSk (Crypto.toPublic delegateSk) w
    signature = Crypto.proxySign Crypto.SignForTestingOnly delegateSk proxySk m

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
    isLeft (decrypt =<< (Crypto.toEither . encrypt $ plaintext))
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
    isLeft (decrypt =<< (Crypto.toEither . encrypt $ plaintext))
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
    isLeft (decrypt =<< (Crypto.toEither . encrypt $ plaintext))
  where
    encrypt = Crypto.encryptChaChaPoly nonce1 key header
    decrypt = Crypto.decryptChaChaPoly nonce2 key header

encToPublicToEnc :: Crypto.SecretKey -> Property
encToPublicToEnc =
    Crypto.encToPublic . Crypto.noPassEncrypt .=. Crypto.toPublic

skToSafeSigner :: Crypto.SecretKey -> Property
skToSafeSigner =
    Crypto.safeToPublic . Crypto.fakeSigner .=. Crypto.toPublic

-- | It appears 'Crypto.deterministicVSsKeyGen' ignores all consecutive null characters
--starting from the beginning of the 'ByteString' seed, so for this property to make sense
-- they need to be dropped.
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
    assert $ Crypto.verifyShare encShare (Crypto.toVssPublicKey vssKP) decShare

verifyEncShareGoodData :: SharedSecrets -> Bool
verifyEncShareGoodData SharedSecrets {..} =
    Crypto.verifyEncShare ssSecShare
                          (ssVSSPKs !! ssPos)
                          (fst $ ssShares !! ssPos)

verifyEncShareBadSecShare :: SharedSecrets -> Crypto.SecretSharingExtra -> Property
verifyEncShareBadSecShare SharedSecrets {..} secShare =
    (ssSecShare /= secShare) ==>
    not $ Crypto.verifyEncShare secShare (ssVSSPKs !! ssPos) (fst $ ssShares !! ssPos)

verifyEncShareMismatchShareKey :: SharedSecrets -> Int -> Property
verifyEncShareMismatchShareKey SharedSecrets {..} p =
    (ssPos /= pos) ==>
    not (Crypto.verifyEncShare ssSecShare (ssVSSPKs !! ssPos) (fst $ ssShares !! pos)) &&
    not (Crypto.verifyEncShare ssSecShare (ssVSSPKs !! pos) (fst $ ssShares !! ssPos))
  where
    len = length ssVSSPKs
    pos = abs $ p `mod` len

verifyShareGoodData :: SharedSecrets -> Bool
verifyShareGoodData SharedSecrets {..} =
    Crypto.verifyShare encShare vssPK decShare
  where
    (encShare, decShare) = ssShares !! ssPos
    vssPK = ssVSSPKs !! ssPos

verifyShareBadShare :: SharedSecrets -> Int -> Property
verifyShareBadShare SharedSecrets {..} p =
    (s1 /= s2 || vssPK1 /= vssPK2) ==>
    not (Crypto.verifyShare encShare1 vssPK1 decShare1) &&
    not (Crypto.verifyShare encShare2 vssPK2 decShare2)
  where
    len = length ssVSSPKs
    pos = abs $ p `mod` len
    s1@(encShare1, decShare1) = ssShares !! ssPos
    s2@(encShare2, decShare2) = ssShares !! pos
    vssPK1 = ssVSSPKs !! pos
    vssPK2 = ssVSSPKs !! ssPos

verifyShareMismatchingShares :: SharedSecrets -> Int -> Property
verifyShareMismatchingShares SharedSecrets {..} p =
    (vssPK1 /= vssPK2) ==>
    not (Crypto.verifyShare encShare1 vssPK1 decShare2)
  where
    len = length ssVSSPKs
    pos = abs $ p `mod` len
    (encShare1, _) = ssShares !! ssPos
    (_, decShare2) = ssShares !! pos
    vssPK1 = ssVSSPKs !! pos
    vssPK2 = ssVSSPKs !! ssPos

verifyProofGoodData :: SharedSecrets -> Bool
verifyProofGoodData SharedSecrets {..} =
    Crypto.verifySecretProof ssSecShare ssSecret ssSecProof

verifyProofBadSecShare :: SharedSecrets -> Crypto.SecretSharingExtra -> Property
verifyProofBadSecShare SharedSecrets {..} secShare =
    (ssSecShare /= secShare) ==>
    not (Crypto.verifySecretProof secShare ssSecret ssSecProof)

verifyProofBadSecret :: SharedSecrets -> Crypto.Secret -> Property
verifyProofBadSecret SharedSecrets {..} secret =
    (ssSecret /= secret) ==>
    not (Crypto.verifySecretProof ssSecShare secret ssSecProof)

verifyProofBadSecProof :: SharedSecrets -> Crypto.SecretProof -> Property
verifyProofBadSecProof SharedSecrets {..} secProof =
    (ssSecProof /= secProof) ==>
    not (Crypto.verifySecretProof ssSecShare ssSecret secProof)

recoverSecretSuccessfully :: SharedSecrets -> Property
recoverSecretSuccessfully SharedSecrets {..} =
    Crypto.recoverSecret ssThreshold triplesList === Just ssSecret
  where
    triplesList = zipWith (\(a,c) b -> (a,b,c)) ssShares ssVSSPKs

recoverSecBadThreshold :: SharedSecrets -> Integer -> Property
recoverSecBadThreshold SharedSecrets {..} rnd =
    (badThreshold > ssThreshold) ==>
    isNothing (Crypto.recoverSecret badThreshold triplesList)
  where
    maxThreshold = genericLength triplesList
    -- badThreshold is in ]actualThreshold, actualThreshold * 2]
    badThreshold = maxThreshold + (succ . abs $ rnd `mod` maxThreshold)
    triplesList = zipWith (\(a,c) b -> (a,b,c)) ssShares ssVSSPKs

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
    let newKey = fromMaybe (error "Passphrase didn't match") $
                 Crypto.changeEncPassphrase oldPass newPass oldKey
    return $ Crypto.encToPublic oldKey === Crypto.encToPublic newKey

