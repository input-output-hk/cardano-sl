{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module AddressSpecs (addressSpecs) where

import           Universum

import           Cardano.Wallet.Client.Http
import           Test.Hspec

import           Util


addressSpecs :: WalletRef -> WalletClient IO -> Spec
addressSpecs wRef wc = do
    describe "Addresses" $ do
        it "Creating an address makes it available" $ do
            -- create a wallet
            Wallet{..} <- sampleWallet wRef wc

            -- create an account
            acc@Account{..} <- fmap wrData $ shouldReturnRight $
               postAccount wc walId (NewAccount Nothing "hello")

            -- accounts should exist
            accs <- fmap wrData $ shouldReturnRight $ getAccounts wc walId
            accs `shouldContain` [acc]

            -- create an address
            addr <- fmap wrData $ shouldReturnRight $ postAddress wc (NewAddress Nothing accIndex walId)

            -- verify that address is in the API
            addrs <- fmap wrData $ shouldReturnRight $ getAddressIndex wc

            map addrId addrs `shouldContain` [addrId addr]

        it "Index returns real data" $ do
            addrs  <- fmap wrData $ shouldReturnRight $ getAddressIndex wc
            addrs' <- fmap wrData $ shouldReturnRight $ getAddressIndex wc

            addrs `shouldBe` addrs'
