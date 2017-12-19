-- | Specification of Pos.Core.Address.

module Test.Pos.Core.AddressSpec
       ( spec
       ) where

import           Universum

import qualified Data.ByteString as BS
import           Formatting (formatToString, int, (%))
import           Serokell.Data.Memory.Units (Byte, memory)
import           Test.Hspec (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Gen, arbitrary, counterexample, forAll, frequency, vectorOf)

import           Pos.Arbitrary.Core ()
import           Pos.Binary.Class (biSize)
import           Pos.Core (Address, IsBootstrapEraAddr (..), deriveLvl2KeyPair,
                           largestHDAddressBoot, largestPubKeyAddressBoot,
                           largestPubKeyAddressSingleKey, makePubKeyAddress, makePubKeyAddressBoot,
                           makePubKeyHdwAddress)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, PublicKey, SecretKey (..),
                             ShouldCheckPassphrase (..), deterministicKeyGen, emptyPassphrase,
                             mkEncSecretUnsafe, noPassEncrypt, toPublic)
import           Pos.Crypto.HD (HDAddressPayload (..))

spec :: Spec
spec = describe "Address" $ do
    modifyMaxSuccess (min 10) $ do
        prop "PK and HDW addresses with same public key are shown differently"
            pkAndHdwAreShownDifferently

    describe "Largest addresses" $ do
        let genPubKeyAddrBoot = pure . makePubKeyAddressBoot . toPublic
        largestAddressProp "PubKey address with BootstrapEra distribution"
            genPubKeyAddrBoot largestPubKeyAddressBoot 43

        let genPubKeyAddrSingleKey = pure . makePubKeyAddress
                (IsBootstrapEraAddr False) . toPublic
        largestAddressProp "PubKey address with SingleKey distribution"
            genPubKeyAddrSingleKey largestPubKeyAddressSingleKey 78

        let genHDAddrBoot :: SecretKey -> Gen Address
            genHDAddrBoot sk = frequency
                [ (1, genHDAddrBootEmptyPass sk)
                , (5, genHDAddrBootSomePass sk)
                ]

            genHDAddrBoot' :: PassPhrase -> EncryptedSecretKey -> Word32 -> Word32 -> Address
            genHDAddrBoot' passphrase esk accIdx addrIdx =
                case deriveLvl2KeyPair
                            (IsBootstrapEraAddr True)
                            (ShouldCheckPassphrase False)
                            passphrase
                            esk
                            accIdx
                            addrIdx of
                    Nothing        -> error "genHDAddrBoot' failed in tests"
                    Just (addr, _) -> addr

            genHDAddrBootEmptyPass sk =
                genHDAddrBoot' emptyPassphrase (noPassEncrypt sk) <$>
                arbitrary <*> arbitrary

            genHDAddrBootSomePass (SecretKey sk) = do
                passphrase <- arbitrary
                esk <- mkEncSecretUnsafe passphrase sk
                genHDAddrBoot' passphrase esk <$> arbitrary <*> arbitrary

        largestAddressProp "HD address with BootstrapEra distribution"
            genHDAddrBoot largestHDAddressBoot 76

pkAndHdwAreShownDifferently :: Bool -> PublicKey -> Bool
pkAndHdwAreShownDifferently isBootstrap pk =
    show (makePubKeyAddress (IsBootstrapEraAddr isBootstrap) pk) /=
    (show @Text (makePubKeyHdwAddress (IsBootstrapEraAddr isBootstrap)
                (HDAddressPayload "pataq") pk))

largestAddressProp :: Text -> (SecretKey -> Gen Address) -> Address -> Byte -> Spec
largestAddressProp addressDescription genAddress largestAddress expectedLargestSize = do
    describe (toString addressDescription) $ do

        it (formatToString ("has the size we expect: "%memory) expectedLargestSize) $ do
            biSize largestAddress `shouldBe` expectedLargestSize

        prop "is indeed the largest address" $ do
            forAll (deterministicKeyGen . BS.pack <$> vectorOf 32 arbitrary) $ \(_, sk) ->
                forAll (genAddress sk) $ \address ->
                    let generatedSize = biSize address
                    in counterexample
                           (formatToString (int % " > " %int) generatedSize expectedLargestSize)
                           (generatedSize <= expectedLargestSize)
