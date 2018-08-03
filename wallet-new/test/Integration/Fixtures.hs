module Integration.Fixtures
    ( MonadIntegrationFixtures(..)

    -- * Out-of-spec fixtures
    , generateInitialState
    ) where

import           Universum

import           Cardano.Wallet.Client.Http
import           Pos.Core.Configuration (generatedSecrets)
import           Pos.Core.Constants (accountGenesisIndex, wAddressGenesisIndex)
import           Pos.Core.Genesis (GeneratedSecrets (..), PoorSecret,
                     poorSecretToEncKey)
import           Pos.Crypto.Signing (PassPhrase)
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.UserSecret (WalletUserSecret (..))
import           Pos.Wallet.Web.Methods (importWalletDo)
import           Pos.Wallet.Web.Mode (WalletWebMode)

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


--
-- USING 'WalletWebMode'
--

-- | Generate an initial state for the integration tests
generateInitialState :: HasConfigurations => WalletWebMode ()
generateInitialState = do
    wallets <- generatedSecretsToWalletSecrets <$> getGeneratedSecrets
    forM_ wallets (uncurry importWalletDo)
  where
    getGeneratedSecrets :: (MonadFail m) => m GeneratedSecrets
    getGeneratedSecrets = do
        let msg = "Couldn't find GeneratedSecrets. To fix this, make sure you \
                  \run the following program with a `TestnetInitializer`."
        maybe (fail msg) return generatedSecrets

    generatedSecretsToWalletSecrets :: GeneratedSecrets -> [(PassPhrase, WalletUserSecret)]
    generatedSecretsToWalletSecrets secrets =
        map poorSecretToWalletUserSecrets (gsPoorSecrets secrets)

    poorSecretToWalletUserSecrets :: PoorSecret -> (PassPhrase, WalletUserSecret)
    poorSecretToWalletUserSecrets secret =
        let
            walUserSecret = WalletUserSecret
                { _wusRootKey    = poorSecretToEncKey secret
                , _wusWalletName = "Genesis Test Wallet (Poor)"
                , _wusAccounts   = [(accountGenesisIndex, "Genesis Test Account")]
                , _wusAddrs      = [(accountGenesisIndex, wAddressGenesisIndex)]
                }
        in
            (mempty, walUserSecret)
