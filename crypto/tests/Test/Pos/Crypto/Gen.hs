module Test.Pos.Crypto.Gen
        (
        -- Protocol Magic Generator
          genProtocolMagic

        -- Sign Tag Generator
        , genSignTag

        -- Key Generators
        , genKeypair
        , genPublicKey
        , genSecretKey
        , genEncryptedSecretKey

        -- Redeem Key Generators
        , genRedeemKeypair
        , genRedeemPublicKey
        , genRedeemSecretKey

        -- VSS Key Generators
        , genVssKeyPair
        , genVssPublicKey

        -- Proxy Cert and Key Generators
        , genProxyCert
        , genProxySecretKey
        , genProxySignature

        -- Signature Generators
        , genSignature
        , genSignatureEncoded
        , genSigned
        , genRedeemSignature

        -- Secret Generators
        , genSharedSecretData
        , genSecret
        , genSecretProof

        -- Hash Generators
        , genAbstractHash

        -- Passphrase Generators
        , genPassPhrase

        -- HD Generators
        , genHDPassphrase
        , genHDAddressPayload
        ) where

import           Universum

import           Crypto.Hash
import qualified Data.ByteArray as ByteArray
import           Data.List.NonEmpty (fromList)
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Binary.Class (Bi)
import           Pos.Crypto (AbstractHash (..), PassPhrase,
                             abstractHash)
import           Pos.Crypto.Configuration (ProtocolMagic (..))
import           Pos.Crypto.HD (HDAddressPayload (..), HDPassphrase (..))
import           Pos.Crypto.Random (deterministic)
import           Pos.Crypto.SecretSharing (EncShare, Secret, SecretProof,
                                           VssKeyPair, VssPublicKey,
                                           deterministicVssKeyGen,
                                           genSharedSecret, toVssPublicKey)
import           Pos.Crypto.Signing (EncryptedSecretKey, ProxyCert,
                                     ProxySecretKey, ProxySignature, PublicKey,
                                     SafeSigner (..), SecretKey, Signature,
                                     Signed, SignTag (..), deterministicKeyGen,
                                     mkSigned, noPassEncrypt, proxySign,
                                     safeCreateProxyCert, safeCreatePsk, sign,
                                     signEncoded)
import           Pos.Crypto.Signing.Redeem (RedeemPublicKey, RedeemSecretKey,
                                            RedeemSignature,
                                            redeemDeterministicKeyGen,
                                            redeemSign)

----------------------------------------------------------------------------
-- Protocol Magic Generator
----------------------------------------------------------------------------

genProtocolMagic :: Gen ProtocolMagic
genProtocolMagic = ProtocolMagic <$> (Gen.int32 Range.constantBounded)

----------------------------------------------------------------------------
-- Sign Tag Generator
----------------------------------------------------------------------------

genSignTag :: Gen SignTag
genSignTag = Gen.element
        [ SignForTestingOnly
        , SignTx
        , SignRedeemTx
        , SignVssCert
        , SignUSProposal
        , SignCommitment
        , SignUSVote
        , SignMainBlock
        , SignMainBlockLight
        , SignMainBlockHeavy
        , SignProxySK
        ]

----------------------------------------------------------------------------
-- Key Generators
----------------------------------------------------------------------------

genKeypair :: Gen (PublicKey, SecretKey)
genKeypair = deterministicKeyGen <$> gen32Bytes

genPublicKey :: Gen PublicKey
genPublicKey =  fst <$> genKeypair

genSecretKey :: Gen SecretKey
genSecretKey = snd <$> genKeypair

genEncryptedSecretKey :: Gen EncryptedSecretKey
genEncryptedSecretKey = noPassEncrypt <$> genSecretKey

----------------------------------------------------------------------------
-- Redeem Key Generators
----------------------------------------------------------------------------

genRedeemKeypair :: Gen (Maybe (RedeemPublicKey, RedeemSecretKey))
genRedeemKeypair = redeemDeterministicKeyGen <$> gen32Bytes

genRedeemPublicKey :: Gen (RedeemPublicKey)
genRedeemPublicKey = do
    rkp <- genRedeemKeypair
    case rkp of
        Nothing      -> error "Error generating a RedeemPublicKey."
        Just (pk, _) -> return pk

genRedeemSecretKey :: Gen (RedeemSecretKey)
genRedeemSecretKey = do
    rkp <- genRedeemKeypair
    case rkp of
        Nothing      -> error "Error generating a RedeemSecretKey."
        Just (_, sk) -> return sk

----------------------------------------------------------------------------
-- VSS Key Generators
----------------------------------------------------------------------------

genVssKeyPair :: Gen VssKeyPair
genVssKeyPair =  deterministicVssKeyGen <$> gen32Bytes

genVssPublicKey :: Gen VssPublicKey
genVssPublicKey = toVssPublicKey <$> genVssKeyPair

----------------------------------------------------------------------------
-- Proxy Cert and Key Generators
----------------------------------------------------------------------------

genProxyCert :: Bi w => ProtocolMagic -> Gen w -> Gen (ProxyCert w)
genProxyCert pm genW =
    safeCreateProxyCert pm <$> genSafeSigner <*> genPublicKey <*> genW

genProxySecretKey :: Bi w => ProtocolMagic -> Gen w -> Gen (ProxySecretKey w)
genProxySecretKey pm genW =
    safeCreatePsk pm <$> genSafeSigner <*> genPublicKey <*> genW

genProxySignature
    :: (Bi w, Bi a)
    => ProtocolMagic
    -> Gen a
    -> Gen w
    -> Gen (ProxySignature w a)
genProxySignature pm genA genW = do
    st  <- genSignTag
    sk  <- genSecretKey
    psk <- genProxySecretKey pm genW
    a   <- genA
    return $ proxySign pm st sk psk a

----------------------------------------------------------------------------
-- Signature Generators
----------------------------------------------------------------------------

genSignature :: Bi a => ProtocolMagic -> Gen a -> Gen (Signature a)
genSignature pm genA = sign pm <$> genSignTag <*> genSecretKey <*> genA

genSignatureEncoded :: ProtocolMagic -> Gen ByteString -> Gen (Signature a)
genSignatureEncoded pm genB =
    signEncoded pm <$> genSignTag <*> genSecretKey <*> genB

genSigned :: Bi a => ProtocolMagic -> Gen a -> Gen (Signed a)
genSigned pm genA =
    mkSigned pm <$> genSignTag <*> genSecretKey <*> genA

genRedeemSignature
    ::  Bi a
    => ProtocolMagic
    -> Gen a
    -> Gen (RedeemSignature a)
genRedeemSignature pm genA =
    redeemSign pm <$> genSignTag <*> genRedeemSecretKey <*> genA

----------------------------------------------------------------------------
-- Secret Generators
----------------------------------------------------------------------------

genSharedSecretData :: Gen (Secret, SecretProof, [(VssPublicKey, EncShare)])
genSharedSecretData = do
    let numKeys = 128 :: Int
    parties <-
        Gen.integral (Range.constant 4 (fromIntegral numKeys)) :: Gen Integer
    threshold <- Gen.integral (Range.constant 2 (parties - 2)) :: Gen Integer
    vssKeys <- replicateM numKeys genVssPublicKey
    let ss = deterministic "ss" $ genSharedSecret threshold (fromList vssKeys)
    return ss

genSecret :: Gen Secret
genSecret = do
    (s, _, _) <- genSharedSecretData
    return s

genSecretProof :: Gen SecretProof
genSecretProof = do
    (_, sp, _) <- genSharedSecretData
    return sp

----------------------------------------------------------------------------
-- Hash Generators
----------------------------------------------------------------------------

genAbstractHash :: Bi a => Gen a -> Gen (AbstractHash Blake2b_256 a)
genAbstractHash genA = abstractHash <$> genA

----------------------------------------------------------------------------
-- Passphrase Generators
----------------------------------------------------------------------------

genPassPhrase :: Gen PassPhrase
genPassPhrase = ByteArray.pack <$> genWord8List
  where
    genWord8List :: Gen [Word8]
    genWord8List =
        Gen.list (Range.singleton 32) (Gen.word8 Range.constantBounded)

----------------------------------------------------------------------------
-- HD Generators
----------------------------------------------------------------------------

genHDPassphrase :: Gen HDPassphrase
genHDPassphrase = HDPassphrase <$> gen32Bytes

genHDAddressPayload :: Gen HDAddressPayload
genHDAddressPayload = HDAddressPayload <$> gen32Bytes

----------------------------------------------------------------------------
-- Helper Generators
----------------------------------------------------------------------------
genBytes :: Int -> Gen ByteString
genBytes n = Gen.bytes (Range.singleton n)

gen32Bytes :: Gen ByteString
gen32Bytes = genBytes 32

genSafeSigner :: Gen SafeSigner
genSafeSigner = FakeSigner <$> genSecretKey
