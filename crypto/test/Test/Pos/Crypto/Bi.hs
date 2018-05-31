{-# LANGUAGE OverloadedStrings #-}
module Test.Pos.Crypto.Bi
    ( tests
    ) where

import           Universum

import           Cardano.Crypto.Wallet (xpub, xprv)

import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS

import           Hedgehog (Property, discover)
import qualified Hedgehog as H

import           Pos.Crypto (PassPhrase, SecretKey (..), PublicKey (..))

import           Test.Pos.Crypto.TempHelpers (discoverGolden, goldenTestBi,
                                              trippingBiBuildable)
import           Test.Pos.Crypto.Gen

golden_PublicKey :: Property
golden_PublicKey = do
    let Right pkey = PublicKey <$> xpub (getBytes 0 64)
    goldenTestBi pkey "test/golden/PublicKey"

prop_trippingBi_PublicKey :: Property
prop_trippingBi_PublicKey =
    H.withTests 1000 . H.property $ H.forAll genPublicKey >>= trippingBiBuildable

golden_SecretKey :: Property
golden_SecretKey = do
    let Right skey = SecretKey <$> xprv (getBytes 10 128)
    goldenTestBi skey "test/golden/SecretKey"

prop_trippingBi_SecretKey :: Property
prop_trippingBi_SecretKey =
    H.withTests 1000 . H.property $ H.forAll genSecretKey >>= trippingBiBuildable


{-
Currently cannot roundtrip test EncryptedSecretKey because one of its comonents,
XPrv, doesn't have an Eq instance. See Pos.Crypto.Signing.Types.Safe

golden_EncryptedSecretKey :: Property
golden_EncryptedSecretKey = do
    let Right skey = SecretKey <$> xprv (getBytes 10 128)
        let esky = EncryptedSecretKey xkey undefined
    goldenTestBi skey "test/golden/EncryptedSecretKey"

prop_trippingBi_EncryptedSecretKey :: Property
prop_trippingBi_EncryptedSecretKey =
    H.withTests 1000 . H.property $ H.forAll genEncryptedSecretKey >>= trippingBiBuildable
-}

golden_PassPhrase :: Property
golden_PassPhrase = do
    -- PassPhrase has to be 32 bytes in length
    let passphrase = ByteArray.pack (BS.unpack $ getBytes 3 32) :: PassPhrase
    goldenTestBi passphrase "test/golden/PassPhrase"

prop_trippingBi_PassPhrase :: Property
prop_trippingBi_PassPhrase =
    H.withTests 1000 . H.property $ H.forAll genPassPhrase >>= trippingBiBuildable


-- ----------------------------------------------------------------------------

getBytes :: Int -> Int -> ByteString
getBytes offset len =
    BS.take len $ BS.drop offset constantByteString

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

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests = do
  (&&) <$> H.checkSequential $$discoverGolden
        <*> H.checkParallel $$discover
