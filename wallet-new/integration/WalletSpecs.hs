{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module WalletSpecs (walletSpecs) where

import           Universum

import           Cardano.Wallet.API.V1.Errors (WalletError (WalletAlreadyExists, WalletNotFound))
import           Cardano.Wallet.Client.Http
import           Control.Lens hiding ((^..), (^?))
import           Test.Hspec

import           Util


walletSpecs :: WalletRef -> WalletClient IO -> Spec
walletSpecs _ wc = do
    describe "Wallets" $ do
        it "Creating a wallet makes it available." $ do
            newWallet <- randomWallet CreateWallet
            Wallet{..} <- createWalletCheck wc newWallet

            eresp <- getWallet wc walId
            void $ eresp `shouldPrism` _Right

        it "Updating a wallet persists the update" $ do
            newWallet <- randomWallet CreateWallet
            wallet <- createWalletCheck wc newWallet
            let newName = "Foobar Bazquux"
                newAssurance = NormalAssurance
            eupdatedWallet <- updateWallet wc (walId wallet) WalletUpdate
                { uwalName = newName
                , uwalAssuranceLevel = newAssurance
                }
            Wallet{..} <- wrData <$> eupdatedWallet `shouldPrism` _Right
            walName `shouldBe` newName
            walAssuranceLevel `shouldBe` newAssurance

        it "CreateWallet with the same mnemonics rises WalletAlreadyExists error" $
            testWalletAlreadyExists CreateWallet

        it "RestoreWallet with the same mnemonics throws WalletAlreadyExists" $
            testWalletAlreadyExists RestoreWallet

        it "Can accept Unicode characters" $ do
            newWallet <- randomWallet CreateWallet
            wallet <- createWalletCheck wc newWallet

            eresp <- updateWallet wc (walId wallet) WalletUpdate
                { uwalName = "patate漢patate字patat"
                , uwalAssuranceLevel = NormalAssurance
                }

            eresp `shouldPrism_` _Right

    describe "External Wallets" $ do
        it "Creating an external wallet makes it available" $ do
            newExtWallet <- randomExternalWallet CreateWallet
            Wallet{..} <- createExternalWalletCheck wc newExtWallet

            void $ getWallet wc walId
                >>= (`shouldPrism` _Right)

        it "Delete an external wallet removes it and its accounts completely" $ do
            -- By default external wallet has one account _without_ addresses
            -- (because they didn't generated yet), so we shouldn't check addresses.
            newExtWallet <- randomExternalWallet CreateWallet
            let pubKeyAsText = newewalExtPubKey newExtWallet
            Wallet{..} <- createExternalWalletCheck wc newExtWallet

            deleteExternalWallet wc pubKeyAsText
                >>= (`shouldPrism` _Right)
            getWallet wc walId
                >>= (`shouldFailWith` (ClientWalletError WalletNotFound))
  where
    testWalletAlreadyExists action = do
            newWallet1 <- randomWallet action
            preWallet2 <- randomWallet action
            let newWallet2 =
                    preWallet2
                        { newwalBackupPhrase = newwalBackupPhrase newWallet1
                        }
            -- First wallet creation/restoration should succeed
            result <- postWallet wc newWallet1
            void $ result `shouldPrism` _Right
            -- Second wallet creation/restoration should rise WalletAlreadyExists
            eresp <- postWallet wc newWallet2
            clientError <- eresp `shouldPrism` _Left
            clientError `shouldBe` ClientWalletError WalletAlreadyExists
