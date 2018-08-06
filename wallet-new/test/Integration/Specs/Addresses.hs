module Integration.Specs.Addresses (spec) where

import           Universum

import           Cardano.Wallet.Client (WalletClient)
import           Cardano.Wallet.Client.Http
import           Test.Hspec (Spec, it)

import           Integration.Expectations
import           Integration.Fixtures
import           Integration.Util (runSpec)


spec :: WalletClient IO -> Spec
spec apiClient =
    it "Creating an address makes it available" $ runSpec apiClient $ do
        (account, wallet) <- getArbitraryAccount

        let body = NewAddress
                { newaddrSpendingPassword = Nothing
                , newaddrAccountIndex     = accIndex account
                , newaddrWalletId         = walId wallet
                }

        lift (postAddress apiClient body) >>= addressShouldExists
