{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module WalletSpecs (walletSpecs) where

import           Universum

import           Cardano.Wallet.API.V1.Errors
                     (WalletError (WalletAlreadyExists))
import           Cardano.Wallet.Client.Http
import           Control.Lens
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

        it "creating wallet gives rise to an empty Utxo histogram" $ do
            newWallet <- randomWallet CreateWallet
            wallet <- createWalletCheck wc newWallet

            eresp <- getUtxoStatistics wc (walId wallet)
            utxoStatistics <- fmap wrData eresp `shouldPrism` _Right
            let possibleBuckets = fmap show $ ( map (\toPower -> 10^toPower :: Word64) [(1::Word64)..16] ) ++ [45 * (10^(15::Word64))]
            let histogram = map (\ x -> curry HistogramBarCount x 0) possibleBuckets
            let allStakes = 0
            utxoStatistics `shouldBe` UtxoStatistics histogram allStakes
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
