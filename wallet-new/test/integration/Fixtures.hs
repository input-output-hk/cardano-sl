module Fixtures where

import           Universum

import           Data.Default (Default (..))
import           Data.Time.Units (fromMicroseconds)
import           System.Environment (lookupEnv)
import           System.Wlog (LoggerName (..))
import           Test.Hspec (describe, hspec, it, shouldBe)

import           Pos.Core (Timestamp (..))
import           Pos.Core.Configuration (HasGeneratedSecrets, generatedSecrets)
import           Pos.Core.Constants (accountGenesisIndex, wAddressGenesisIndex)
import           Pos.Core.Genesis (GeneratedSecrets (..), PoorSecret,
                     poorSecretToEncKey)
import           Pos.Crypto.Signing (PassPhrase)
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.UserSecret (WalletUserSecret (..))
import           Pos.Wallet.Web.Methods (importWalletDo)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.Server.Runner (CommonNodeArgs (..),
                     ExtraNodeArgs (..), NodeArgs (..), runWWebMode)


-- | Generate an initial state for the integration tests
generateInitialState :: HasConfigurations => WalletWebMode ()
generateInitialState = do
    wallets <- generatedSecretsToWalletSecrets <$> getGeneratedSecrets
    forM_ wallets (uncurry importWalletDo)
  where
    getGeneratedSecrets :: (MonadFail m, HasGeneratedSecrets) => m GeneratedSecrets
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
