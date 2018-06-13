{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Util.MnemonicSpec where

import           Universum

import           Crypto.Hash (Blake2b_256, Digest, hash)
import           Data.ByteArray (convert)
import           Data.Default (def)
import           Data.Set (Set)
import           Test.Hspec (Spec, describe, it, shouldSatisfy, xit)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), forAll, property, (===))
import           Test.QuickCheck.Gen (vectorOf)

import           Pos.Crypto (AesKey (..), EncryptedSecretKey, PassPhrase (..),
                             safeDeterministicKeyGen)
import           Pos.Util.Mnemonic (Entropy, EntropySize, Mnemonic, entropyToByteString,
                                    entropyToMnemonic, mkEntropy, mkMnemonic, mnemonicToAesKey,
                                    mnemonicToEntropy, mnemonicToSeed)
import           Pos.Wallet.Web.ClientTypes.Functions (encToCId)
import           Pos.Wallet.Web.ClientTypes.Types (CId)

import qualified Cardano.Crypto.Wallet as CC
import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import qualified Test.Pos.Util.BackupPhraseOld as Old
import qualified Test.Pos.Util.MnemonicOld as Old


-- | By default, private keys aren't comparable for security reasons (timing
-- attacks). We allow it here for testing purpose which is fine.
instance Eq CC.XPrv where
    (==) = (==) `on` CC.unXPrv



spec :: Spec
spec = do
    describe "Old and New implementation behave identically" $ do
        modifyMaxSuccess (const 100) $ prop "entropyToESK (no passphrase)" $
            \ent -> entropyToESK mempty ent === entropyToESKOld mempty ent

        modifyMaxSuccess (const 100) $ prop "entropyToESK (with passphrase)" $
            \ent -> entropyToESK defPwd ent === entropyToESKOld defPwd ent

        modifyMaxSuccess (const 1000) $ prop "entropyToAesKEy" $
            \ent -> entropyToAesKey ent === entropyToAesKeyOld ent

    modifyMaxSuccess (const 1000) $ prop "(9) entropyToMnemonic . mnemonicToEntropy == identity" $
        \e -> (mnemonicToEntropy @9 . entropyToMnemonic @9 @(EntropySize 9)) e == e

    modifyMaxSuccess (const 1000) $ prop "(12) entropyToMnemonic . mnemonicToEntropy == identity" $
        \e -> (mnemonicToEntropy @12 . entropyToMnemonic @12 @(EntropySize 12)) e == e

    it "No example mnemonic" $
        (mkMnemonic @12 defMnemonic) `shouldSatisfy` isLeft

    it "No empty mnemonic" $
        (mkMnemonic @12 []) `shouldSatisfy` isLeft

    it "No empty entropy" $
        (mkEntropy @(EntropySize 12) "") `shouldSatisfy` isLeft

    xit "entropyToWalletId is injective (very long to run, used for investigation)"
        $ property
        $ forAll (vectorOf 1000 arbitrary)
        $ \inputs -> length (inject entropyToWalletId inputs) == length inputs
  where
    defPwd :: PassPhrase
    defPwd =
        PassPhrase "cardano"

    defMnemonic :: [Text]
    defMnemonic = either (error . show) identity
        $ Aeson.eitherDecode
        $ Aeson.encode
        $ def @(Mnemonic 12)

    -- | Collect function results in a Set
    inject :: Ord b => (a -> b) -> [a] -> Set b
    inject fn =
        Set.fromList . fmap fn

    entropyToWalletId :: Entropy (EntropySize 12) -> CId w
    entropyToWalletId =
        encToCId . entropyToESK mempty

    blake2b :: ByteString -> ByteString
    blake2b =
        convert @(Digest Blake2b_256) . hash

    -- | Generate an EncryptedSecretKey using the old implementation
    entropyToESKOld :: PassPhrase -> Entropy (EntropySize 12) -> EncryptedSecretKey
    entropyToESKOld passphrase ent = esk
      where
        backupPhrase = either
            (error . (<>) "[Old] Wrong arbitrary Entropy generated: " . show)
            (Old.BackupPhrase . words)
            (Old.toMnemonic $ entropyToByteString ent)

        esk = either
            (error . (<>) "[Old] Couldn't create keys from generated BackupPhrase" . show)
            fst
            (Old.safeKeysFromPhrase passphrase backupPhrase)

    -- | Generate an EncryptedSecretKey using the revised implementation
    entropyToESK :: PassPhrase -> Entropy (EntropySize 12) -> EncryptedSecretKey
    entropyToESK passphrase ent = esk
      where
        seed =
            mnemonicToSeed $ entropyToMnemonic ent

        esk =
            snd (safeDeterministicKeyGen seed passphrase)

    entropyToAesKeyOld :: Entropy (EntropySize 9) -> AesKey
    entropyToAesKeyOld ent = key
      where
        backupPhrase = either
            (error . (<>) "[Old] Wrong arbitrary Entropy generated: " . show)
            (Old.BackupPhrase . words)
            (Old.toMnemonic $ entropyToByteString ent)

        key = either
            (error . (<>) "[Old] Couldn't create Aes keys from generated BackupPhrase" . show)
            identity
            (AesKey . blake2b <$> Old.toSeed backupPhrase)

    entropyToAesKey :: Entropy (EntropySize 9) -> AesKey
    entropyToAesKey =
        mnemonicToAesKey . entropyToMnemonic
