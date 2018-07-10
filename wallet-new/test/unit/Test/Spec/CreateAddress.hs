{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Spec.CreateAddress (spec) where

import           Universum

import           Test.Hspec (Spec, describe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick, run)

import qualified Data.Map.Strict as M

import           Data.Acid (update)
import           System.Random (randomRIO)
import           System.Wlog (Severity)


import           Cardano.Wallet.Kernel (PassiveWallet, wallets)
import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState
import           Cardano.Wallet.Kernel.DB.HdWallet (AssuranceLevel (..),
                     HasSpendingPassword (..), HdAccountId (..),
                     HdAccountIx (..), HdRootId (..), WalletName (..))
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (initHdRoot)
import           Cardano.Wallet.Kernel.DB.HdWallet.Derivation
                     (HardeningMode (..), deriveIndex)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..))
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Types (AccountId (..))
import qualified Cardano.Wallet.WalletLayer as WalletLayer

import           Util.Buildable (ShowThroughBuild (..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- | Do not pollute the test runner output with logs.
devNull :: Severity -> Text -> IO ()
devNull _ _ = return ()

-- | Prepare some fixtures using the 'PropertyM' context to prepare the data,
-- and execute the 'acid-state' update once the 'PassiveWallet' gets into
-- scope (after the bracket initialisation).
prepareFixtures :: PropertyM IO (PassiveWallet -> IO AccountId)
prepareFixtures = do
    newRootId   <- HdRootId . InDb <$> pick arbitrary
    newRoot <- initHdRoot <$> pure newRootId
                          <*> pure (WalletName "A wallet")
                          <*> pure NoSpendingPassword
                          <*> pure AssuranceLevelNormal
                          <*> (InDb <$> pick arbitrary)
    newAccountId <- HdAccountId newRootId <$> lift (deriveIndex randomRIO HdAccountIx HardDerivation)
    let accounts = M.singleton newAccountId mempty
    return $ \pw -> do
        void $ update (pw ^. wallets) (CreateHdWallet newRoot accounts)
        return (AccountIdHdRnd newAccountId)

spec :: Spec
spec =
{--
    describe "Address creation (wallet layer)" $ do

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
--}
    describe "Address creation (kernel)" $ do

        prop "works as expected in the happy path scenario" $ monadicIO $ do
          keystore <- run (Keystore.newTestKeystore)
          genAccId <- prepareFixtures
          liftIO $ WalletLayer.bracketKernelPassiveWallet @IO devNull keystore $ \_ wallet -> do
              myAccountId <- genAccId wallet
              res <- liftIO (Kernel.createAddress mempty myAccountId wallet)
              liftIO ((bimap STB STB res) `shouldSatisfy` isRight)


