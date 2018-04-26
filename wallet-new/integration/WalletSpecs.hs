{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module WalletSpecs (walletSpecs) where

import           Universum

import           Cardano.Wallet.Client.Http
import           Servant (errBody)
import           Cardano.Wallet.API.V1.Errors (WalletError (WalletAlreadyExists), toServantError)
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
            let errorBody = errBody $ toServantError WalletAlreadyExists
            case clientError of
                ClientHttpError (FailureResponse response) ->
                    responseBody response `shouldBe` errorBody
                _ ->
                    expectationFailure $
                        "expected (ClientHttpError FailureResponse) but got: "
                        <> show clientError

