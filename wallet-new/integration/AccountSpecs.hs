{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module AccountSpecs (accountSpecs) where

import           Universum

import           Cardano.Wallet.Client.Http
import           Pos.Core (addrToBase58Text)
import           Test.Hspec
import           Test.QuickCheck (arbitrary, generate)

import           Util

accountSpecs :: WalletClient IO -> Spec
accountSpecs wc = do
    describe "Accounts" $ do
        it "Create a new address path for the account" $ do
            -- create a wallet
            newWallet <- randomWallet CreateWallet
            Wallet{..} <- createWalletCheck wc newWallet

            -- create an account
            Account{..} <- fmap wrData $ shouldReturnRight $
                             postAccount wc walId (NewAccount Nothing "hello")

            -- create an address path
            addrPath <- shouldReturnRight $ postAddressPath wc walId accIndex

            -- create another address path
            addrPath' <- shouldReturnRight $ postAddressPath wc walId accIndex

            -- We don't expect a different path unless new addresses have
            -- actually been created
            addrPath `shouldBe` addrPath'

        it "Store a new address in the default account of an external wallet" $ do
            -- create an external wallet
            let (_, rootPK) = makeWalletRootKeys SeventhSK
            newExtWallet <- randomExternalWalletWithPublicKey CreateWallet rootPK
            aWallet@Wallet{..} <- createExternalWalletCheck wc newExtWallet

            -- generate and prepare a new address (as if it is generated in external wallet)
            newAddress <- randomAddress
            let newAddressAsBase58 = addrToBase58Text newAddress

            -- external wallet already contains default account (without addresses),
            -- so get this default account
            defaultAccount <- firstAccountInExtWallet wc aWallet

            -- store new address
            _storeAddrResp <- shouldReturnRight $ postStoreAddress wc
                                                                   walId
                                                                   (accIndex defaultAccount)
                                                                   newAddressAsBase58

            -- check if that address is presented in the default account
            defaultAccount' <- firstAccountInExtWallet wc aWallet
            let addressesInDefAcc = accAddresses defaultAccount'
            length addressesInDefAcc `shouldBe` 1
            let (storedWAddress:_) = addressesInDefAcc
                storedAddress = unV1 . addrId $ storedWAddress
            storedAddress `shouldBe` newAddress

        it "Attempt to store invalid address will fail" $ do
            -- create an external wallet
            let (_, rootPK) = makeWalletRootKeys EighthSK
            newExtWallet <- randomExternalWalletWithPublicKey CreateWallet rootPK
            aWallet@Wallet{..} <- createExternalWalletCheck wc newExtWallet

            -- this address is in Base58-form, but it's not Cardano address
            let invalidAddress = "007F1A76C03D639EB901DC3CE71068D677626EED0F8679575F"

            -- external wallet already contains default account (without addresses),
            -- so get this default account
            defaultAccount <- firstAccountInExtWallet wc aWallet

            -- attempt to store invalid address
            let action = postStoreAddress wc
                                          walId
                                          (accIndex defaultAccount)
                                          invalidAddress

            void $ action `shouldFailWith` ClientWalletError
              (InvalidAddressFormat "Invalid base58 representation of address")

  where
    randomAddress :: IO Address
    randomAddress = generate arbitrary
