{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module WalletSpecs (walletSpecs) where

import           Universum

import           Cardano.Wallet.Client.Http
import           Control.Lens
import           Functions (randomTest)
import           Test.Hspec
import           Test.QuickCheck.Monadic (run)

import           Util


walletSpecs :: WalletRef -> WalletClient IO -> Spec
walletSpecs _ wc =

    describe "Wallets" $ do

        randomTest "Creating a wallet makes it available." 1 $ do
            newWallet <- run $ randomWallet CreateWallet
            Wallet{..} <- run $ createWalletCheck wc newWallet

            eresp <- run $ getWallet wc walId
            liftIO $ void $ eresp `shouldPrism` _Right

        randomTest "Updating a wallet persists the update" 1 $ do
            newWallet <- run $ randomWallet CreateWallet
            wallet <- run $ createWalletCheck wc newWallet
            let newName = "Foobar Bazquux"
                newAssurance = NormalAssurance
            eupdatedWallet <- run $ updateWallet wc (walId wallet) WalletUpdate
                { uwalName = newName
                , uwalAssuranceLevel = newAssurance
                }
            Wallet{..} <- run $ wrData <$> eupdatedWallet `shouldPrism` _Right
            liftIO $ walName `shouldBe` newName
            liftIO $ walAssuranceLevel `shouldBe` newAssurance

        randomTest "CreateWallet with the same mnemonics rises WalletAlreadyExists error" 1 $ do
            testWalletAlreadyExists CreateWallet

        randomTest "RestoreWallet with the same mnemonics throws WalletAlreadyExists" 1 $ do
            testWalletAlreadyExists RestoreWallet

        randomTest "Can accept Unicode characters" 1 $ do
            newWallet <- run $ randomWallet CreateWallet
            wallet <- run $ createWalletCheck wc newWallet

            eresp <- run $ updateWallet wc (walId wallet) WalletUpdate
                { uwalName = "patate漢patate字patat"
                , uwalAssuranceLevel = NormalAssurance
                }

            liftIO $ eresp `shouldPrism_` _Right

        randomTest "creating wallet gives rise to an empty Utxo histogram" 1 $ do
            newWallet <- run $ randomWallet CreateWallet
            wallet <- run $ createWalletCheck wc newWallet

            eresp <- run $ getUtxoStatistics wc (walId wallet)
            utxoStatistics <- run $ fmap wrData eresp `shouldPrism` _Right
            let utxoStatisticsExpected = computeUtxoStatistics log10 []
            liftIO $ utxoStatistics `shouldBe` utxoStatisticsExpected

  where
    testWalletAlreadyExists action = do
            newWallet1 <- run $ randomWallet action
            preWallet2 <- run $ randomWallet action
            let newWallet2 =
                    preWallet2
                        { newwalBackupPhrase = newwalBackupPhrase newWallet1
                        }
            -- First wallet creation/restoration should succeed
            result <- run $ postWallet wc newWallet1
            wallet <- run $ fmap wrData (result `shouldPrism` _Right)
            -- Second wallet creation/restoration should rise WalletAlreadyExists
            eresp <- run $ postWallet wc newWallet2
            clientError <- run $ eresp `shouldPrism` _Left
            liftIO $ clientError
                `shouldBe`
                    ClientWalletError (WalletAlreadyExists (walId wallet))
