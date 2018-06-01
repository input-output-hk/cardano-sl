{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Pos.Util.MnemonicSpec where

import           Universum

import           Data.Default (def)
import           Data.Set (Set)
import           Test.Hspec (Spec, describe, it, shouldSatisfy, xit)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (Arbitrary (..), forAll, property, (===))
import           Test.QuickCheck.Gen (vectorOf)

import           Pos.Crypto (EncryptedSecretKey, PassPhrase (..), safeDeterministicKeyGen)
import           Pos.Util.Mnemonic (Entropy, emptyMnemonic, entropyToByteString, entropyToMnemonic,
                                    mkEntropy, mnemonicToEntropy, mnemonicToSeed)
import           Pos.Wallet.Web.ClientTypes.Functions (encToCId)
import           Pos.Wallet.Web.ClientTypes.Types (CId)

import qualified Cardano.Crypto.Wallet as CC
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

        modifyMaxSuccess (const 100) $ prop "entropyToESK (no passphrase)" $
            \ent -> entropyToESK defPwd ent === entropyToESKOld defPwd ent

    modifyMaxSuccess (const 10000) $ prop "entropyToMnemonic >=> mnemonicToEntropy = Right" $
        \e -> (entropyToMnemonic e >>= mnemonicToEntropy) == Right e

    xit "No example mnemonic" $
        mnemonicToEntropy def `shouldSatisfy` isLeft

    xit "No empty mnemonic" $
        (mnemonicToEntropy emptyMnemonic) `shouldSatisfy` isLeft

    xit "No empty entropy" $
        (mkEntropy "") `shouldSatisfy` isLeft

    xit "entropyToWalletId is injective (very long to run, used for investigation)"
        $ property
        $ forAll (vectorOf 1000 arbitrary)
        $ \inputs -> length (inject entropyToWalletId inputs) == length inputs
  where
    defPwd :: PassPhrase
    defPwd =
        PassPhrase "cardano"

    -- | Collect function results in a Set
    inject :: Ord b => (a -> b) -> [a] -> Set b
    inject fn =
        Set.fromList . fmap fn

    entropyToWalletId :: Entropy -> CId w
    entropyToWalletId =
        encToCId . entropyToESK mempty

    -- | Generate an EncryptedSecretKey using the old implementation
    entropyToESKOld :: PassPhrase -> Entropy -> EncryptedSecretKey
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
    entropyToESK :: PassPhrase -> Entropy -> EncryptedSecretKey
    entropyToESK passphrase ent = esk
      where
        seed = either
            (error . (<>) "[New] Wrong arbitrary Entropy generated: " . show)
            (identity @ByteString)
            (entropyToMnemonic ent >>= mnemonicToSeed)

        esk =
            snd (safeDeterministicKeyGen seed passphrase)


