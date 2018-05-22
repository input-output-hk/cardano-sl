{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | `Arbitrary` instances for using in tests and benchmarks

module Test.Pos.Crypto.Arbitrary
       ( SharedSecrets (..)
       , genSignature
       , genSignatureEncoded
       , genRedeemSignature
       ) where

import           Universum

import           Control.Monad (zipWithM)
import qualified Data.ByteArray as ByteArray
import           Data.List.NonEmpty (fromList)
import           Test.QuickCheck (Arbitrary (..), Gen, elements, oneof, vector)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Binary.Class (AsBinary (..), AsBinaryClass (..), Bi, Raw)
import           Pos.Binary.Crypto ()
import           Pos.Crypto.AsBinary ()
import           Pos.Crypto.Configuration (HasProtocolMagic, ProtocolMagic (..), protocolMagic)
import           Pos.Crypto.Hashing (AHash (..), AbstractHash (..), HashAlgorithm, WithHash (..),
                                     unsafeCheatingHashCoerce, withHash)
import           Pos.Crypto.HD (HDAddressPayload, HDPassphrase (..))
import           Pos.Crypto.Random (deterministic, randomNumberInRange)
import           Pos.Crypto.SecretSharing (DecShare, EncShare, Secret, SecretProof, Threshold,
                                           VssKeyPair, VssPublicKey, decryptShare, genSharedSecret,
                                           toVssPublicKey, vssKeyGen)
import           Pos.Crypto.Signing (EncryptedSecretKey (..), PassPhrase, ProxyCert, ProxySecretKey,
                                     ProxySignature, PublicKey, SecretKey, SignTag (..), Signature,
                                     Signed, createProxyCert, createPsk, keyGen, mkSigned,
                                     noPassEncrypt, proxySign, sign, signEncoded, toPublic)
import           Pos.Crypto.Signing.Redeem (RedeemPublicKey, RedeemSecretKey, RedeemSignature,
                                            redeemKeyGen, redeemSign)

import           Pos.Util.Orphans ()

import           Test.Pos.Util.Orphans ()
import           Test.Pos.Util.QuickCheck.Arbitrary (Nonrepeating (..), arbitraryUnsafe, runGen,
                                                     sublistN)

import           Test.Pos.Crypto.Arbitrary.Unsafe ()

deriving instance Arbitrary ProtocolMagic

{- A note on 'Arbitrary' instances
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Generating keys takes time, so we just pregenerate lots of keys in advance
and use them in 'Arbitrary' instances.
-}

keysToGenerate :: Int
keysToGenerate = 128

----------------------------------------------------------------------------
-- SignTag
----------------------------------------------------------------------------

instance Arbitrary SignTag where
    arbitrary = genericArbitrary
    shrink = genericShrink

----------------------------------------------------------------------------
-- Arbitrary signing keys
----------------------------------------------------------------------------

-- If you want an arbitrary keypair, just generate a secret key with
-- 'arbitrary' and then use 'Pos.Crypto.toPublic' to get the corresponding
-- public key.

keys :: [(PublicKey, SecretKey)]
keys = deterministic "keys" $
    replicateM keysToGenerate keyGen

instance Arbitrary PublicKey where
    arbitrary = fst <$> elements keys
instance Arbitrary SecretKey where
    arbitrary = snd <$> elements keys

instance Nonrepeating PublicKey where
    nonrepeating n = map fst <$> sublistN n keys
instance Nonrepeating SecretKey where
    nonrepeating n = map snd <$> sublistN n keys

-- Repeat the same for ADA redemption keys
redemptionKeys :: [(RedeemPublicKey, RedeemSecretKey)]
redemptionKeys = deterministic "redemptionKeys" $
    replicateM keysToGenerate redeemKeyGen

instance Arbitrary RedeemPublicKey where
    arbitrary = fst <$> elements redemptionKeys
instance Arbitrary RedeemSecretKey where
    arbitrary = snd <$> elements redemptionKeys

instance Nonrepeating RedeemPublicKey where
    nonrepeating n = map fst <$> sublistN n redemptionKeys
instance Nonrepeating RedeemSecretKey where
    nonrepeating n = map snd <$> sublistN n redemptionKeys

----------------------------------------------------------------------------
-- Arbitrary VSS keys
----------------------------------------------------------------------------

vssKeys :: [VssKeyPair]
vssKeys = deterministic "vssKeys" $
    replicateM keysToGenerate vssKeyGen

instance Arbitrary VssKeyPair where
    arbitrary = elements vssKeys

instance Arbitrary VssPublicKey where
    arbitrary = toVssPublicKey <$> arbitrary

instance Arbitrary (AsBinary VssPublicKey) where
    arbitrary = asBinary @VssPublicKey <$> arbitrary

instance Nonrepeating VssKeyPair where
    nonrepeating n = sublistN n vssKeys

instance Nonrepeating VssPublicKey where
    nonrepeating n = map toVssPublicKey <$> nonrepeating n

----------------------------------------------------------------------------
-- Arbitrary signatures
----------------------------------------------------------------------------

-- hlint thinks that the where clauses in the following 3 definitions are
-- unnecessary duplication. I disagree completely.

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

{-# ANN genSignatureEncoded ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Generate a signature with a given 'ProtocolMagic', for some generated
-- bytes. The 'SignTag' and 'SecretKey' are generated using their
-- 'Arbitrary' instances.
genSignatureEncoded :: ProtocolMagic -> Gen ByteString -> Gen (Signature a)
genSignatureEncoded pm genBytes = signEncoded pm <$> genSignTag <*> genSecretKey <*> genBytes
  where
    genSignTag :: Gen SignTag
    genSignTag = arbitrary
    genSecretKey :: Gen SecretKey
    genSecretKey = arbitrary

{-# ANN genSignature ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Like 'genSignatureEncoded' but use an 'a' that can be serialized.
genSignature :: Bi a => ProtocolMagic -> Gen a -> Gen (Signature a)
genSignature pm genA = sign pm <$> genSignTag <*> genSecretKey <*> genA
  where
    genSignTag :: Gen SignTag
    genSignTag = arbitrary
    genSecretKey :: Gen SecretKey
    genSecretKey = arbitrary

{-# ANN genRedeemSignature ("HLint: ignore Reduce duplication" :: Text) #-}

genRedeemSignature :: Bi a => ProtocolMagic -> Gen a -> Gen (RedeemSignature a)
genRedeemSignature pm genA = redeemSign pm <$> genSignTag <*> genSecretKey <*> genA
  where
    genSignTag :: Gen SignTag
    genSignTag = arbitrary
    genSecretKey :: Gen RedeemSecretKey
    genSecretKey = arbitrary

instance (HasProtocolMagic, Bi a, Arbitrary a) => Arbitrary (Signature a) where
    arbitrary = genSignature protocolMagic arbitrary

instance (HasProtocolMagic, Bi a, Arbitrary a) => Arbitrary (RedeemSignature a) where
    arbitrary = genRedeemSignature protocolMagic arbitrary

instance (HasProtocolMagic, Bi a, Arbitrary a) => Arbitrary (Signed a) where
    arbitrary = mkSigned protocolMagic <$> arbitrary <*> arbitrary <*> arbitrary

instance (HasProtocolMagic, Bi w, Arbitrary w) => Arbitrary (ProxyCert w) where
    arbitrary = liftA3 (createProxyCert protocolMagic) arbitrary arbitrary arbitrary

instance (HasProtocolMagic, Bi w, Arbitrary w) => Arbitrary (ProxySecretKey w) where
    arbitrary = liftA3 (createPsk protocolMagic) arbitrary arbitrary arbitrary

instance (HasProtocolMagic, Bi w, Arbitrary w, Bi a, Arbitrary a) =>
         Arbitrary (ProxySignature w a) where
    arbitrary = do
        delegateSk <- arbitrary
        issuerSk <- arbitrary
        w <- arbitrary
        let psk = createPsk protocolMagic issuerSk (toPublic delegateSk) w
        proxySign protocolMagic SignProxySK delegateSk psk <$> arbitrary

----------------------------------------------------------------------------
-- Arbitrary secrets
----------------------------------------------------------------------------

data SharedSecrets = SharedSecrets
    { ssSecret    :: !Secret
    , ssSecProof  :: !SecretProof
    , ssEncShares :: ![EncShare]
    , ssDecShares :: ![DecShare]
    , ssThreshold :: !Threshold
    , ssVssKeys   :: ![VssPublicKey]
    , ssPos       :: !Int            -- This field is a valid, zero-based index in the
                                     -- shares/keys lists.
    } deriving (Show, Eq)

sharedSecrets :: [SharedSecrets]
sharedSecrets =
    runGen $ replicateM keysToGenerate $ do
        parties <- randomNumberInRange 4 (toInteger (length vssKeys))
        ssThreshold <- randomNumberInRange 2 (parties - 2)
        vssKs <- sortWith toVssPublicKey <$>
                 sublistN (fromInteger parties) vssKeys
        let ssVssKeys = map toVssPublicKey vssKs
        (ssSecret, ssSecProof, map snd -> ssEncShares) <-
            genSharedSecret ssThreshold (fromList ssVssKeys)
        ssDecShares <- zipWithM decryptShare vssKs ssEncShares
        let ssPos = fromInteger parties - 1
        return SharedSecrets{..}

instance Arbitrary Secret where
    arbitrary = elements . fmap ssSecret $ sharedSecrets

instance Arbitrary (AsBinary Secret) where
    arbitrary = asBinary @Secret <$> arbitrary

instance Arbitrary SecretProof where
    arbitrary = elements . fmap ssSecProof $ sharedSecrets

instance Arbitrary EncShare where
    arbitrary = elements . concatMap ssEncShares $ sharedSecrets

instance Arbitrary (AsBinary EncShare) where
    arbitrary = asBinary @EncShare <$> arbitrary

instance Arbitrary DecShare where
    arbitrary = elements . concatMap ssDecShares $ sharedSecrets

instance Arbitrary (AsBinary DecShare) where
    arbitrary = asBinary @DecShare <$> arbitrary

instance Arbitrary SharedSecrets where
    arbitrary = elements sharedSecrets

----------------------------------------------------------------------------
-- Arbitrary hashes
----------------------------------------------------------------------------

instance (HashAlgorithm algo, Bi a) => Arbitrary (AbstractHash algo a) where
    arbitrary = arbitraryUnsafe

instance Arbitrary AHash where
    arbitrary = unsafeCheatingHashCoerce @Raw <$> arbitrary

----------------------------------------------------------------------------
-- Arbitrary passphrases
----------------------------------------------------------------------------

instance Arbitrary PassPhrase where
    arbitrary = oneof [
        pure mempty,
        ByteArray.pack <$> vector 32
        ]

----------------------------------------------------------------------------
-- HD
----------------------------------------------------------------------------

instance Arbitrary HDPassphrase where
    arbitrary = HDPassphrase . fromString <$> vector 32

instance Arbitrary HDAddressPayload where
    arbitrary = genericArbitrary

----------------------------------------------------------------------------
-- WithHash
----------------------------------------------------------------------------

instance (Bi a, Arbitrary a) => Arbitrary (WithHash a) where
    arbitrary = fmap withHash arbitrary
    shrink = genericShrink

--------------------------------------------------------------------------
-- Pos.Crypto.Signing.Types.Safe
--------------------------------------------------------------------------

instance Arbitrary EncryptedSecretKey where
    arbitrary = noPassEncrypt <$> arbitrary
