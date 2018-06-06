{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Crypto.Bi
    ( tests
    ) where

import           Universum

import           Cardano.Crypto.Wallet (XPrv, unXPrv, xprv, xpub)

import           Crypto.Hash (Blake2b_256)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS

import           Hedgehog (Gen, Property)
import qualified Hedgehog as H

import           Pos.Aeson.Crypto ()
import           Pos.Crypto (AbstractHash, PassPhrase, ProtocolMagic (..), ProxyCert,
                             ProxySecretKey, PublicKey (..), RedeemSignature,
                             SafeSigner (FakeSigner), Secret, SecretKey (..), SecretProof,
                             SignTag (SignForTestingOnly), Signature, WithHash, deriveHDPassphrase,
                             deterministicVssKeyGen, hash, mkSigned, noPassEncrypt,
                             packHDAddressAttr, proxySign, redeemDeterministicKeyGen, redeemSign,
                             safeCreateProxyCert, safeCreatePsk, sign, toPublic, toVssPublicKey)

import           Test.Pos.Crypto.Gen
import           Test.Pos.Crypto.TempHelpers (discoverGolden, discoverRoundTrip, eachOf,
                                              goldenTestBi, roundTripsAesonBuildable,
                                              roundTripsAesonShow, roundTripsBiBuildable,
                                              roundTripsBiShow)

--------------------------------------------------------------------------------
-- ProtocolMagic
--------------------------------------------------------------------------------

roundTripProtocolMagicAeson :: Property
roundTripProtocolMagicAeson = eachOf 1000 genProtocolMagic roundTripsAesonShow

--------------------------------------------------------------------------------
-- PublicKey
--------------------------------------------------------------------------------

golden_PublicKey :: Property
golden_PublicKey = goldenTestBi pkey "test/golden/PublicKey"
    where Right pkey = PublicKey <$> xpub (getBytes 0 64)

roundTripPublicKeyBi :: Property
roundTripPublicKeyBi = eachOf 1000 genPublicKey roundTripsBiBuildable

roundTripPublicKeyAeson :: Property
roundTripPublicKeyAeson = eachOf 1000 genPublicKey roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- SecretKey
--------------------------------------------------------------------------------

golden_SecretKey :: Property
golden_SecretKey = goldenTestBi skey "test/golden/SecretKey"
    where Right skey = SecretKey <$> xprv (getBytes 10 128)

roundTripSecretKeyBi :: Property
roundTripSecretKeyBi = eachOf 1000 genSecretKey roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Signature
--------------------------------------------------------------------------------

golden_Signature :: Property
golden_Signature = goldenTestBi sig "test/golden/Signature"
  where
    Right skey = SecretKey <$> xprv (getBytes 10 128)
    sig        = sign (ProtocolMagic 0) SignForTestingOnly skey ()

genUnitSignature :: Gen (Signature ())
genUnitSignature = genSignature $ pure ()

roundTripSignatureBi :: Property
roundTripSignatureBi = eachOf 1000 genUnitSignature roundTripsBiBuildable

roundTripSignatureAeson :: Property
roundTripSignatureAeson = eachOf 1000 genUnitSignature roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- Signed
--------------------------------------------------------------------------------

golden_Signed :: Property
golden_Signed = goldenTestBi signed "test/golden/Signed"
  where
    Right skey = SecretKey <$> xprv (getBytes 10 128)
    signed     = mkSigned (ProtocolMagic 0) SignForTestingOnly skey ()

roundTripSignedBi :: Property
roundTripSignedBi = eachOf 1000 genUnitSigned roundTripsBiShow
    where genUnitSigned = genSigned $ pure ()

--------------------------------------------------------------------------------
-- EncryptedSecretKey
--------------------------------------------------------------------------------

-- | This instance is unsafe, as it allows a timing attack. But it's OK for
-- tests.
instance Eq XPrv where
   (==) = (==) `on` unXPrv

golden_EncryptedSecretKey :: Property
golden_EncryptedSecretKey = goldenTestBi esk "test/golden/EncryptedSecretKey"
    where Right esk = noPassEncrypt . SecretKey <$> xprv (getBytes 10 128)

roundTripEncryptedSecretKeysBi :: Property
roundTripEncryptedSecretKeysBi =
    eachOf 100 genEncryptedSecretKey roundTripsBiBuildable

--------------------------------------------------------------------------------
-- RedeemPublicKey
--------------------------------------------------------------------------------

golden_RedeemPublicKey :: Property
golden_RedeemPublicKey = goldenTestBi rpk "test/golden/RedeemPublicKey"
    where Just rpk = fst <$> redeemDeterministicKeyGen (getBytes 0 32)

roundTripRedeemPublicKeyBi :: Property
roundTripRedeemPublicKeyBi =
    eachOf 1000 genRedeemPublicKey roundTripsBiBuildable

roundTripRedeemPublicKeyAeson :: Property
roundTripRedeemPublicKeyAeson =
    eachOf 1000 genRedeemPublicKey roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- RedeemSecretKey
--------------------------------------------------------------------------------

golden_RedeemSecretKey :: Property
golden_RedeemSecretKey = goldenTestBi rsk "test/golden/RedeemSecretKey"
    where Just rsk = snd <$> redeemDeterministicKeyGen (getBytes 0 32)

roundTripRedeemSecretKeyBi :: Property
roundTripRedeemSecretKeyBi =
    eachOf 1000 genRedeemSecretKey roundTripsBiBuildable

--------------------------------------------------------------------------------
-- RedeemSignature
--------------------------------------------------------------------------------

golden_RedeemSignature :: Property
golden_RedeemSignature = goldenTestBi rsig "test/golden/RedeemSignature"
  where
    Just rsk = snd <$> redeemDeterministicKeyGen (getBytes 0 32)
    rsig     = redeemSign (ProtocolMagic 0) SignForTestingOnly rsk ()

genUnitRedeemSignature :: Gen (RedeemSignature ())
genUnitRedeemSignature = genRedeemSignature $ pure ()

roundTripRedeemSignatureBi :: Property
roundTripRedeemSignatureBi =
    eachOf 1000 genUnitRedeemSignature roundTripsBiBuildable

roundTripRedeemSignatureAeson :: Property
roundTripRedeemSignatureAeson =
    eachOf 1000 genUnitRedeemSignature roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- VssPublicKey
--------------------------------------------------------------------------------

golden_VssPublicKey :: Property
golden_VssPublicKey = goldenTestBi vpk "test/golden/VssPublicKey"
    where vpk = toVssPublicKey . deterministicVssKeyGen $ getBytes 0 32

roundTripVssPublicKeyBi :: Property
roundTripVssPublicKeyBi = eachOf 1000 genVssPublicKey roundTripsBiShow

--------------------------------------------------------------------------------
-- ProxyCert
--------------------------------------------------------------------------------

golden_ProxyCert :: Property
golden_ProxyCert = goldenTestBi pcert "test/golden/ProxyCert"
  where
    Right pkey = PublicKey <$> xpub (getBytes 0 64)
    Right skey = SecretKey <$> xprv (getBytes 10 128)
    pcert      = safeCreateProxyCert (ProtocolMagic 0) (FakeSigner skey) pkey ()

genUnitProxyCert :: Gen (ProxyCert ())
genUnitProxyCert = genProxyCert $ pure ()

roundTripProxyCertBi :: Property
roundTripProxyCertBi = eachOf 100 genUnitProxyCert roundTripsBiBuildable

roundTripProxyCertAeson :: Property
roundTripProxyCertAeson = eachOf 100 genUnitProxyCert roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- ProxySecretKey
--------------------------------------------------------------------------------

golden_ProxySecretKey :: Property
golden_ProxySecretKey = goldenTestBi psk "test/golden/ProxySecretKey"
  where
    Right pkey = PublicKey <$> xpub (getBytes 0 64)
    Right skey = SecretKey <$> xprv (getBytes 10 128)
    psk        = safeCreatePsk (ProtocolMagic 0) (FakeSigner skey) pkey ()

genUnitProxySecretKey :: Gen (ProxySecretKey ())
genUnitProxySecretKey = genProxySecretKey $ pure ()

roundTripProxySecretKeyBi :: Property
roundTripProxySecretKeyBi =
    eachOf 100 genUnitProxySecretKey roundTripsBiBuildable

roundTripProxySecretKeyAeson :: Property
roundTripProxySecretKeyAeson =
    eachOf 100 genUnitProxySecretKey roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- ProxySignature
--------------------------------------------------------------------------------

golden_ProxySignature :: Property
golden_ProxySignature = goldenTestBi psig "test/golden/ProxySignature"
  where
    Right skey = SecretKey <$> xprv (getBytes 10 128)
    psk = safeCreatePsk (ProtocolMagic 0) (FakeSigner skey) (toPublic skey) ()
    psig = proxySign (ProtocolMagic 0) SignForTestingOnly skey psk ()

roundTripProxySignatureBi :: Property
roundTripProxySignatureBi = eachOf 100
                                   genUnitProxySignature
                                   roundTripsBiBuildable
    where genUnitProxySignature = genProxySignature (pure ()) (pure ())

--------------------------------------------------------------------------------
-- DecShare
--------------------------------------------------------------------------------

roundTripDecShareBi :: Property
roundTripDecShareBi = eachOf 20 genDecShare roundTripsBiShow

--------------------------------------------------------------------------------
-- EncShare
--------------------------------------------------------------------------------

roundTripEncShareBi :: Property
roundTripEncShareBi = eachOf 20 genEncShare roundTripsBiShow

--------------------------------------------------------------------------------
-- Secret
--------------------------------------------------------------------------------

-- | Not done because the constructor for the underlying `Point` type is not
--   exposed and there is no deterministic generation function
todo_golden_Secret :: Property
todo_golden_Secret = goldenTestBi
    (error "golden_Secret not yet defined" :: Secret)
    "test/golden/Secret"

roundTripSecretBi :: Property
roundTripSecretBi = eachOf 20 genSecret roundTripsBiShow

--------------------------------------------------------------------------------
-- SecretProof
--------------------------------------------------------------------------------

-- | We have a similar problem for this
todo_golden_SecretProof :: Property
todo_golden_SecretProof = goldenTestBi
    (error "golden_SecretProof not yet defined" :: SecretProof)
    "test/golden/SecretProof"

roundTripSecretProofBi :: Property
roundTripSecretProofBi = eachOf 20 genSecretProof roundTripsBiShow

--------------------------------------------------------------------------------
-- AbstractHash
--------------------------------------------------------------------------------

golden_AbstractHash :: Property
golden_AbstractHash = goldenTestBi (hash ()) "test/golden/AbstractHash"

genUnitAbstractHash :: Gen (AbstractHash Blake2b_256 ())
genUnitAbstractHash = genAbstractHash $ pure ()

roundTripAbstractHashBi :: Property
roundTripAbstractHashBi = eachOf 1000 genUnitAbstractHash roundTripsBiBuildable

roundTripAbstractHashAeson :: Property
roundTripAbstractHashAeson =
    eachOf 1000 genUnitAbstractHash roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- WithHash
--------------------------------------------------------------------------------

golden_WithHash :: Property
golden_WithHash = goldenTestBi (hash ()) "test/golden/WithHash"

genUnitWithHash :: Gen (WithHash ())
genUnitWithHash = genWithHash $ pure ()

roundTripWithHashBi :: Property
roundTripWithHashBi = eachOf 1000 genUnitWithHash roundTripsBiBuildable

--------------------------------------------------------------------------------
-- PassPhrase
--------------------------------------------------------------------------------

golden_PassPhrase :: Property
golden_PassPhrase = goldenTestBi passphrase "test/golden/PassPhrase"
    where
    -- PassPhrase has to be 32 bytes in length
          passphrase = ByteArray.pack (BS.unpack $ getBytes 3 32) :: PassPhrase

roundTripPassPhraseBi :: Property
roundTripPassPhraseBi = eachOf 1000 genPassPhrase roundTripsBiBuildable

--------------------------------------------------------------------------------
-- HDAddressPayload
--------------------------------------------------------------------------------

golden_HDAddressPayload :: Property
golden_HDAddressPayload = goldenTestBi hdap "test/golden/HDAddressPayload"
  where
    Right hdap =
        flip packHDAddressAttr [] . deriveHDPassphrase . PublicKey <$> xpub
            (getBytes 0 64)

roundTripHDAddressPayloadBi :: Property
roundTripHDAddressPayloadBi = eachOf 1000 genHDAddressPayload roundTripsBiShow

roundTripHDAddressPayloadAeson :: Property
roundTripHDAddressPayloadAeson =
    eachOf 1000 genHDAddressPayload roundTripsAesonShow

--------------------------------------------------------------------------------

getBytes :: Int -> Int -> ByteString
getBytes offset len = BS.take len $ BS.drop offset constantByteString

-- | Changing existing values in this string will break existing golden
-- tests, but it us OK to append more data to the end.
constantByteString :: ByteString
constantByteString
    = "Kmyw4lDSE5S4fSH6etNouiXezCyEjKc3tG4ja0kFjO8qzai26ZMPUEJfEy15ox5kJ0uKD\
    \bi7i6dLXkuesVZ9JfHgjrctsLFt2NvovXnchsOvX05Y6LohlTNt5mkPFhUoXu1EZSJTIy\
    \3fTU53b412r4AEusD7tcdRgH47yTr5hMO63bJnYBbmNperLHfiT1lP0MLQLh1J1DfoYBs\
    \auoJOzvtAgvjHo6UFttnK6vZ3Cknpuob6uMS2MkJKmuoQsqsAYcRDWbJ2Rgw4bm2ndTM4\
    \zFfuRDKvdrL6sDkuPNPYqxMWlqnXjSbU0eLtceZuKgXLHR8cdvsEvywt4JaZUQhnbq3Vl\
    \7nZqcXdoi4XGTCgSGcGp8N0SDVhvkVh0QF1RVpWPnOMyYISJvuaHfo1zXMdq9tEdtJfID"

--------------------------------------------------------------------------------

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkSequential $$discoverRoundTrip
