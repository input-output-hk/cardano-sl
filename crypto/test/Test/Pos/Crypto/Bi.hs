{-# LANGUAGE OverloadedStrings #-}

module Test.Pos.Crypto.Bi
    ( tests
    ) where

import           Universum

import           Cardano.Crypto.Wallet (xprv, xpub)

import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.Crypto (HDAddressPayload, PassPhrase, PublicKey (..), RedeemPublicKey,
                             RedeemSecretKey, Secret, SecretKey (..), SecretProof, VssPublicKey)

import           Test.Pos.Crypto.Gen
import           Test.Pos.Crypto.TempHelpers (discoverGolden, discoverRoundTrip, eachOf,
                                              goldenTestBi, roundTripsBiBuildable, roundTripsBiShow)

--------------------------------------------------------------------------------
-- PublicKey
--------------------------------------------------------------------------------

golden_PublicKey :: Property
golden_PublicKey = goldenTestBi pkey "test/golden/PublicKey"
  where
    Right pkey = PublicKey <$> xpub (getBytes 0 64)

roundTripPublicKeyBi :: Property
roundTripPublicKeyBi = eachOf 1000 genPublicKey roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SecretKey
--------------------------------------------------------------------------------

golden_SecretKey :: Property
golden_SecretKey = goldenTestBi skey "test/golden/SecretKey"
  where
    Right skey = SecretKey <$> xprv (getBytes 10 128)

roundTripSecretKeyBi :: Property
roundTripSecretKeyBi = eachOf 1000 genSecretKey roundTripsBiBuildable

--------------------------------------------------------------------------------
-- EncryptedSecretKey
--------------------------------------------------------------------------------

{-
Currently cannot roundtrip test EncryptedSecretKey because one of its components,
XPrv, doesn't have an Eq instance. See Pos.Crypto.Signing.Types.Safe

golden_EncryptedSecretKey :: Property
golden_EncryptedSecretKey = do
    let Right skey = SecretKey <$> xprv (getBytes 10 128)
        let esky = EncryptedSecretKey xkey undefined
    goldenTestBi skey "test/golden/EncryptedSecretKey"

roundTripEncryptedSecretKeysBi :: Property
roundTripEncryptedSecretKeysBi = eachOf genEncryptedSecretKey roundTripsBiBuildable
-}

--------------------------------------------------------------------------------
-- RedeemPublicKey
--------------------------------------------------------------------------------

todo_golden_RedeemPublicKey :: Property
todo_golden_RedeemPublicKey = goldenTestBi
    (error "golden_RedeemPublicKey not yet defined" :: RedeemPublicKey)
    "test/golden/RedeemPublicKey"

roundTripRedeemPublicKeyBi :: Property
roundTripRedeemPublicKeyBi = eachOf 1000 genRedeemPublicKey roundTripsBiBuildable

--------------------------------------------------------------------------------
-- RedeemSecretKey
--------------------------------------------------------------------------------

todo_golden_RedeemSecretKey :: Property
todo_golden_RedeemSecretKey = goldenTestBi
    (error "golden_RedeemSecretKey not yet defined" :: RedeemSecretKey)
    "test/golden/RedeemSecretKey"

roundTripRedeemSecretKeyBi :: Property
roundTripRedeemSecretKeyBi = eachOf 1000 genRedeemSecretKey roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssPublicKey
--------------------------------------------------------------------------------

todo_golden_VssPublicKey :: Property
todo_golden_VssPublicKey = goldenTestBi
    (error "golden_VssPublicKey not yet defined" :: VssPublicKey)
    "test/golden/VssPublicKey"

roundTripVssPublicKeyBi :: Property
roundTripVssPublicKeyBi = eachOf 1000 genVssPublicKey roundTripsBiShow

--------------------------------------------------------------------------------
-- Secret
--------------------------------------------------------------------------------

todo_golden_Secret :: Property
todo_golden_Secret = goldenTestBi
    (error "golden_Secret not yet defined" :: Secret)
    "test/golden/Secret"

roundTripSecretBi :: Property
roundTripSecretBi = eachOf 100 genSecret roundTripsBiShow

--------------------------------------------------------------------------------
-- SecretProof
--------------------------------------------------------------------------------

todo_golden_SecretProof :: Property
todo_golden_SecretProof = goldenTestBi
    (error "golden_SecretProof not yet defined" :: SecretProof)
    "test/golden/SecretProof"

roundTripSecretProofBi :: Property
roundTripSecretProofBi = eachOf 100 genSecretProof roundTripsBiShow

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

todo_golden_HDAddressPayload :: Property
todo_golden_HDAddressPayload = goldenTestBi
    (error "golden_HDAddressPayload not yet defined" :: HDAddressPayload)
    "test/golden/HDAddressPayload"

roundTripHDAddressPayloadBi :: Property
roundTripHDAddressPayloadBi = eachOf 1000 genHDAddressPayload roundTripsBiShow

--------------------------------------------------------------------------------

getBytes :: Int -> Int -> ByteString
getBytes offset len = BS.take len $ BS.drop offset constantByteString

-- | Changing existing values in this string will break existing golden
-- tests, but it us OK to append more data to the end.
constantByteString :: ByteString
constantByteString =
    "Kmyw4lDSE5S4fSH6etNouiXezCyEjKc3tG4ja0kFjO8qzai26ZMPUEJfEy15ox5kJ0uKD\
    \bi7i6dLXkuesVZ9JfHgjrctsLFt2NvovXnchsOvX05Y6LohlTNt5mkPFhUoXu1EZSJTIy\
    \3fTU53b412r4AEusD7tcdRgH47yTr5hMO63bJnYBbmNperLHfiT1lP0MLQLh1J1DfoYBs\
    \auoJOzvtAgvjHo6UFttnK6vZ3Cknpuob6uMS2MkJKmuoQsqsAYcRDWbJ2Rgw4bm2ndTM4\
    \zFfuRDKvdrL6sDkuPNPYqxMWlqnXjSbU0eLtceZuKgXLHR8cdvsEvywt4JaZUQhnbq3Vl\
    \7nZqcXdoi4XGTCgSGcGp8N0SDVhvkVh0QF1RVpWPnOMyYISJvuaHfo1zXMdq9tEdtJfID"

--------------------------------------------------------------------------------

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkParallel $$discoverRoundTrip
