{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module AccountSpecs (accountSpecs) where

import           Universum

import           Cardano.Wallet.API.V1.Errors (WalletError (..))
import           Cardano.Wallet.Client.Http
import           Control.Lens hiding ((^..), (^?))
import           Test.Hspec

import           Util


accountSpecs :: WalletClient IO -> Spec
accountSpecs wc = do
    describe "Accounts" $ do
        it "Create a new address path for the account" $ do
            -- create a wallet
            newWallet <- randomWallet CreateWallet
            Wallet{..} <- createWalletCheck wc newWallet

            -- create an account
            accResp <- postAccount wc walId (NewAccount Nothing "hello")
            Account{..} <- wrData <$> accResp `shouldPrism` _Right

            -- create an address path
            pathResp <- postAddressPath wc walId accIndex
            addrPath <- pathResp `shouldPrism` _Right

            -- create another address path
            pathResp' <- postAddressPath wc walId accIndex
            addrPath' <- pathResp' `shouldPrism` _Right

            -- We don't expect a different path unless new addresses have
            -- actually been created
            addrPath `shouldBe` addrPath'

        it "Cannot create an address path on a non-hardened account key" $ do
            -- create a wallet
            newWallet <- randomWallet CreateWallet
            Wallet{..} <- createWalletCheck wc newWallet

            pathResp <- postAddressPath wc walId 0
            err <- pathResp `shouldPrism` _Left
            let errMsg = "AddressLevel out-of-bound: must be a 31-byte unsigned integer"

            err `shouldBe` (ClientWalletError (CannotCreateAddress errMsg))
