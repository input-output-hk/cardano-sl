module Integration.Fixtures (MonadIntegrationFixtures(..)) where

import           Universum

import           Cardano.Wallet.Client.Http

import           Integration.Util (asksM, fromResp)

import qualified Prelude


class MonadIntegrationFixtures m where
    getArbitraryAccount :: m (Account, Wallet)

instance MonadIntegrationFixtures (ReaderT (WalletClient IO) IO) where
    getArbitraryAccount = do
        wallets <- asksM getWallets >>= fromResp
        let wallet@Wallet{..} = Prelude.head wallets
        accounts <- asksM (`getAccounts` walId) >>= fromResp
        return (Prelude.head accounts, wallet)
