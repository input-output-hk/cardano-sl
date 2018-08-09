module AddressSpecs (addressSpecs) where

import           Universum

import           Cardano.Wallet.Client.Http
import           Test.Hspec

import           Util


addressSpecs :: WalletClient IO -> Spec
addressSpecs wc = describe "Addresses" $ do
    before (createRandomSampleWallet wc) $ do
        it "Creating an address makes it available" $ \someWallet -> do
            let Wallet{..} = someWallet

            -- create an account
            acc@Account{..} <- wrData <$> shouldReturnRight
               (postAccount wc walId (NewAccount Nothing "hello"))

            -- accounts should exist
            accs <- wrData <$> shouldReturnRight (getAccounts wc walId)
            accs `shouldContain` [acc]

            -- create an address
            addr <- wrData <$> shouldReturnRight (postAddress wc (NewAddress Nothing accIndex walId))

            -- verify that address is in the API
            addrs <- wrData <$> shouldReturnRight (getAddressIndex wc)

            map addrId addrs `shouldContain` [addrId addr]

    it "Index returns real data" $ do
        addrs  <- wrData <$> shouldReturnRight (getAddressIndex wc)
        addrs' <- wrData <$> shouldReturnRight (getAddressIndex wc)

        addrs `shouldBe` addrs'
