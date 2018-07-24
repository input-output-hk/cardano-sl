{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module WalletSpecs (walletSpecs) where

import           Universum

import           Cardano.Wallet.API.V1.Errors
                     (WalletError (WalletAlreadyExists, WalletNotFound))
import           Cardano.Wallet.Client.Http
import           Cardano.Wallet.API.V1.Errors (WalletError (WalletAlreadyExists, WalletNotFound))
import           Test.Hspec
import           Test.QuickCheck (arbitrary, generate)

import           Util


walletSpecs :: WalletRef -> WalletClient IO -> Spec
walletSpecs _ wc = do
    describe "Wallets" $ do
        it "Creating a wallet makes it available." $ do
            newWallet <- randomWallet CreateWallet
            Wallet{..} <- createWalletCheck wc newWallet

            void $ shouldReturnRight $ getWallet wc walId

        it "Updating a wallet persists the update" $ do
            newWallet <- randomWallet CreateWallet
            wallet <- createWalletCheck wc newWallet
            let newName = "Foobar Bazquux"
                newAssurance = NormalAssurance
            Wallet{..} <- fmap wrData $ shouldReturnRight $ updateWallet wc (walId wallet) WalletUpdate
                { uwalName = newName
                , uwalAssuranceLevel = newAssurance
                }
            walName `shouldBe` newName
            walAssuranceLevel `shouldBe` newAssurance

        it "CreateWallet with the same mnemonics rises WalletAlreadyExists error" $
            testWalletAlreadyExists CreateWallet

        it "RestoreWallet with the same mnemonics throws WalletAlreadyExists" $
            testWalletAlreadyExists RestoreWallet

        it "Can accept Unicode characters" $ do
            newWallet <- randomWallet CreateWallet
            wallet <- createWalletCheck wc newWallet

            void $ shouldReturnRight $ updateWallet wc (walId wallet) WalletUpdate
                { uwalName = "patate漢patate字patat"
                , uwalAssuranceLevel = NormalAssurance
                }

    describe "External Wallets" $ do
        it "Creating an external wallet makes it available" $ do
            newExtWallet <- randomExternalWallet CreateWallet
            Wallet{..} <- createExternalWalletCheck wc newExtWallet
            void $ shouldReturnRight $ getWallet wc walId

        it "Creating an external wallet (using prepared public key) makes it available" $ do
            publicKey <- makeWalletKey
            newExtWallet <- randomExternalWalletWithPublicKey CreateWallet publicKey
            Wallet{..} <- createExternalWalletCheck wc newExtWallet
            void $ shouldReturnRight $ getWallet wc walId

        it "Delete an external wallet removes it and its accounts completely" $ do
            -- By default external wallet has one account _without_ addresses
            -- (because they didn't generated yet), so we shouldn't check addresses.
            newExtWallet <- randomExternalWallet CreateWallet
            let pubKeyAsText = newewalExtPubKey newExtWallet
            Wallet{..} <- createExternalWalletCheck wc newExtWallet

            void $ shouldReturnRight $ deleteExternalWallet wc pubKeyAsText
            getWallet wc walId `shouldFailWith` (ClientWalletError WalletNotFound)

  where
    testWalletAlreadyExists action = do
            newWallet1 <- randomWallet action
            preWallet2 <- randomWallet action
            let newWallet2 =
                    preWallet2
                        { newwalBackupPhrase = newwalBackupPhrase newWallet1
                        }
            -- First wallet creation/restoration should succeed
            void $ shouldReturnRight $ postWallet wc newWallet1
            -- Second wallet creation/restoration should rise WalletAlreadyExists
            postWallet wc newWallet2 `shouldFailWith` ClientWalletError WalletAlreadyExists

    makeWalletKey = do
        secretKey <- (generate arbitrary :: IO SecretKey)
        let publicKey = encToPublic $ noPassEncrypt secretKey
        pure publicKey
