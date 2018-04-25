{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module AddressSpecs (addressSpecs) where

import           Universum

import           Cardano.Wallet.Client.Http
import           Control.Lens hiding ((^..), (^?))
import           Test.Hspec

import           Util


addressSpecs :: WalletRef -> WalletClient IO -> Spec
addressSpecs wRef wc = do
    describe "Addresses" $ do
        it "Creating an address makes it available" $ do
            -- create a wallet
            Wallet{..} <- sampleWallet wRef wc

            -- create an account
            accResp <- postAccount wc walId (NewAccount Nothing "hello")
            acc@Account{..} <- wrData <$> accResp `shouldPrism` _Right

            -- accounts should exist
            accResp' <- getAccounts wc walId
            accs <- wrData <$> accResp' `shouldPrism` _Right
            accs `shouldContain` [acc]

            -- create an address
            addResp <- postAddress wc (NewAddress Nothing accIndex walId)
            addr <- wrData <$> addResp `shouldPrism` _Right

            -- verify that address is in the API
            idxResp <- getAddressIndex wc
            addrs <- wrData <$> idxResp `shouldPrism` _Right

            map addrId addrs `shouldContain` [addrId addr]

        it "Index returns real data" $ do
            addrsResp <- getAddressIndex wc
            addrs <- wrData <$> addrsResp `shouldPrism` _Right

            addrsResp' <- getAddressIndex wc
            addrs' <- wrData <$> addrsResp' `shouldPrism` _Right

            addrs `shouldBe` addrs'
