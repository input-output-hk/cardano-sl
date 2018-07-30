{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Spec.Keystore (
    spec
  ) where

import           Universum

import           System.Directory (doesFileExist, removeFile)
import           System.IO.Error (IOError)

import           Test.Hspec (Spec, describe, it, shouldBe, shouldReturn,
                     shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Gen, arbitrary)
import           Test.QuickCheck.Monadic (forAllM, monadicIO, pick, run)

import           Pos.Crypto (EncryptedSecretKey, hash, safeKeyGen)

import           Cardano.Wallet.Kernel.DB.HdWallet (eskToHdRootId)
import           Cardano.Wallet.Kernel.Keystore (DeletePolicy (..), Keystore)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Types (WalletId (..))

import           Util.Buildable (ShowThroughBuild (..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Creates and operate on a keystore. The 'Keystore' is created in a temporary
-- directory and garbage-collected from the Operating System.
withKeystore :: (Keystore -> IO a) -> IO a
withKeystore = Keystore.bracketTestKeystore

genKeypair :: Gen ( ShowThroughBuild WalletId
                  , ShowThroughBuild EncryptedSecretKey
                  )
genKeypair = do
    (_, esk) <- arbitrary >>= safeKeyGen
    return $ bimap STB STB (WalletIdHdRnd . eskToHdRootId  $ esk, esk)

genKeys :: Gen ( ShowThroughBuild WalletId
               , ShowThroughBuild EncryptedSecretKey
               , ShowThroughBuild EncryptedSecretKey
               )
genKeys = do
    (wId, origKey) <- genKeypair
    (_, esk2) <- arbitrary >>= safeKeyGen
    return (wId, origKey, STB esk2)

nukeKeystore :: FilePath -> IO ()
nukeKeystore fp =
    removeFile fp `catch` (\(_ :: IOError) -> return ())

spec :: Spec
spec =
    describe "Keystore to store UserSecret(s)" $ do
        it "creating a brand new one works" $ do
            nukeKeystore "test_keystore.key"
            Keystore.bracketKeystore KeepKeystoreIfEmpty "test_keystore.key" $ \_ks ->
                return ()
            doesFileExist "test_keystore.key" `shouldReturn` True

        it "destroying a keystore (completely) works" $ do
            nukeKeystore "test_keystore.key"
            Keystore.bracketKeystore RemoveKeystoreIfEmpty "test_keystore.key" $ \_ks ->
                return ()
            doesFileExist "test_keystore.key" `shouldReturn` False

        prop "lookup of keys works" $ monadicIO $ do
            forAllM genKeypair $ \(STB wid, STB esk) -> run $ do
                withKeystore $ \ks -> do
                    Keystore.insert wid esk ks
                    mbKey <- Keystore.lookup wid ks
                    (fmap hash mbKey) `shouldBe` (Just (hash esk))

        prop "replacement of keys works" $ monadicIO $ do
            forAllM genKeys $ \(STB wid, STB oldKey, STB newKey) -> run $ do
                withKeystore $ \ks -> do
                    Keystore.insert wid oldKey ks
                    mbOldKey <- Keystore.lookup wid ks
                    Keystore.replace wid newKey ks
                    mbNewKey <- Keystore.lookup wid ks
                    (fmap hash mbOldKey) `shouldSatisfy` ((/=) (fmap hash mbNewKey))

        prop "Inserts are persisted after releasing the keystore" $ monadicIO $ do
            (STB wid, STB esk) <- pick genKeypair
            run $ do
                nukeKeystore "test_keystore.key"
                Keystore.bracketKeystore KeepKeystoreIfEmpty "test_keystore.key" $ \keystore1 ->
                    Keystore.insert wid esk keystore1
                Keystore.bracketKeystore KeepKeystoreIfEmpty "test_keystore.key" $ \keystore2 -> do
                    mbKey <- Keystore.lookup wid keystore2
                    (fmap hash mbKey) `shouldBe` (Just (hash esk))

        prop "deletion of keys works" $ monadicIO $ do
            forAllM genKeypair $ \(STB wid, STB esk) -> run $ do
                withKeystore $ \ks -> do
                    Keystore.insert wid esk ks
                    Keystore.delete wid ks
                    mbKey <- Keystore.lookup wid ks
                    (fmap hash mbKey) `shouldBe` Nothing

        prop "Deletion of keys are persisted after releasing the keystore" $ monadicIO $ do
            (STB wid, STB esk) <- pick genKeypair
            run $ do
                nukeKeystore "test_keystore.key"
                Keystore.bracketKeystore KeepKeystoreIfEmpty "test_keystore.key" $ \keystore1 -> do
                    Keystore.insert wid esk keystore1
                    Keystore.delete wid keystore1
                Keystore.bracketKeystore KeepKeystoreIfEmpty "test_keystore.key" $ \keystore2 -> do
                    mbKey <- Keystore.lookup wid keystore2
                    (fmap hash mbKey) `shouldBe` Nothing
