-- | Specification of Pos.Core.Address.

module Test.Pos.Core.AddressSpec
       ( spec
       ) where

import           Universum

import qualified Data.ByteString as BS
import           Formatting (formatToString, int, (%))
import           Serokell.Data.Memory.Units (Byte, memory)
import           Test.Hspec (Spec, describe, it, runIO, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Gen, arbitrary, counterexample, forAll,
                     frequency, generate, vectorOf)

import           Pos.Binary.Class (biSize)
import           Pos.Core (Address, IsBootstrapEraAddr (..), deriveLvl2KeyPair,
                     largestHDAddressBoot, largestPubKeyAddressBoot,
                     largestPubKeyAddressSingleKey, makePubKeyAddress,
                     makePubKeyAddressBoot, makePubKeyHdwAddress)
import           Pos.Core.NetworkMagic (NetworkMagic (..), makeNetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, PassPhrase, ProtocolMagic (..),
                     PublicKey, RequiresNetworkMagic (..), SecretKey (..),
                     ShouldCheckPassphrase (..), deterministicKeyGen,
                     emptyPassphrase, mkEncSecretUnsafe, noPassEncrypt,
                     toPublic)
import           Pos.Crypto.HD (HDAddressPayload (..))

import           Test.Pos.Core.Arbitrary ()

spec :: Spec
spec = do
    runWithMagic RequiresNoMagic
    runWithMagic RequiresMagic

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

-- An attempt to avoid rightward creep
specBody :: ProtocolMagic -> Spec
specBody pm = do
    let nm = makeNetworkMagic pm

    modifyMaxSuccess (min 10) $ do
        prop "PK and HDW addresses with same public key are shown differently"
            (pkAndHdwAreShownDifferently nm)

    describe "Largest addresses" $ do
        let genPubKeyAddrBoot = pure . makePubKeyAddressBoot nm . toPublic
            pubKeyAddrBootSize = 43 + networkMagicExtraBytes pm
        largestAddressProp "PubKey address with BootstrapEra distribution"
            genPubKeyAddrBoot (largestPubKeyAddressBoot nm) pubKeyAddrBootSize

        let genPubKeyAddrSingleKey = pure . makePubKeyAddress nm
                (IsBootstrapEraAddr False) . toPublic
            pubKeyAddrSingleKeySize = 78 + networkMagicExtraBytes pm
        largestAddressProp "PubKey address with SingleKey distribution"
            genPubKeyAddrSingleKey (largestPubKeyAddressSingleKey nm) pubKeyAddrSingleKeySize

        let genHDAddrBoot :: SecretKey -> Gen Address
            genHDAddrBoot sk = frequency
                [ (1, genHDAddrBootEmptyPass sk)
                , (5, genHDAddrBootSomePass sk)
                ]

            genHDAddrBoot' :: PassPhrase -> EncryptedSecretKey -> Word32 -> Word32 -> Address
            genHDAddrBoot' passphrase esk accIdx addrIdx =
                case deriveLvl2KeyPair
                            nm
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

        let hdAddrBootSize = 76 + networkMagicExtraBytes pm
        largestAddressProp "HD address with BootstrapEra distribution"
            genHDAddrBoot (largestHDAddressBoot nm) hdAddrBootSize

networkMagicExtraBytes :: ProtocolMagic -> Byte
networkMagicExtraBytes pm = case makeNetworkMagic pm of
    NetworkMainOrStage -> 0
    -- Encoding size:
    -- Map key: 2 bytes (1 for header, 1 for Word8)
    -- Map val: 1-5 bytes (1 for header, 0-4 for Int32)
    NetworkTestnet v   -> 2 + biSize v

pkAndHdwAreShownDifferently :: NetworkMagic -> Bool -> PublicKey -> Bool
pkAndHdwAreShownDifferently nm isBootstrap pk =
    show (makePubKeyAddress nm (IsBootstrapEraAddr isBootstrap) pk) /=
    (show @Text (makePubKeyHdwAddress nm (IsBootstrapEraAddr isBootstrap)
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
