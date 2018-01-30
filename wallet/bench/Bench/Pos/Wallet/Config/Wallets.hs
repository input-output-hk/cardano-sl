{-# LANGUAGE ApplicativeDo #-}

-- | Functions for working with Wallets configuration file.

module Bench.Pos.Wallet.Config.Wallets
    ( getWalletsConfig
    ) where

import           Universum

import qualified Data.Yaml              as Yaml
import           Data.Yaml              ((.:))
import           Data.Aeson.Types       (typeMismatch)

import           Bench.Pos.Wallet.Types (WalletAccount (..), Wallet (..),
                                         WalletsConfig (..))

instance Yaml.FromJSON WalletAccount where
    parseJSON (Yaml.Object o) = WalletAccount
        <$> o .: "AccountId"
        <*> o .: "Addresses"
    parseJSON invalid    = typeMismatch "WalletAccount" invalid

instance Yaml.FromJSON Wallet where
    parseJSON (Yaml.Object o) = Wallet
        <$> o .: "WalletId"
        <*> o .: "Accounts"
    parseJSON invalid    = typeMismatch "Wallet" invalid

instance Yaml.FromJSON WalletsConfig where
    parseJSON (Yaml.Object o) = WalletsConfig
        <$> o .: "Wallets"
    parseJSON invalid    = typeMismatch "WalletsConfig" invalid

-- | Reads Wallets configuration from the local .yaml-file.
getWalletsConfig :: FilePath -> IO WalletsConfig
getWalletsConfig pathToConfig =
    (Yaml.decodeFile pathToConfig :: IO (Maybe WalletsConfig)) >>= \case
        Nothing   -> reportAboutInvalidConfig
        Just conf -> makeSureWalletsAreNonEmpty conf >> return conf
  where
    reportAboutInvalidConfig :: IO a
    reportAboutInvalidConfig = error . toText $
        "Unable to read endpoints configuration " <> pathToConfig

-- | Checks if wallets data isn't empty, otherwise we cannot run benchmarks.
makeSureWalletsAreNonEmpty :: WalletsConfig -> IO ()
makeSureWalletsAreNonEmpty WalletsConfig {..} = do
    when (null wallets) reportAboutWalletsMissing
    when (any null allAccounts) reportAboutAccountsMissing
    when (any null allAddresses) reportAboutAddressesMissing
  where
    allAccounts  = [accs | (Wallet _ accs) <- wallets]
    allAddresses = [addrs | (WalletAccount _ addrs) <- concat allAccounts]

    reportAboutWalletsMissing   = error "At least one wallet must be defined in Wallets.yaml."
    reportAboutAccountsMissing  = error "Each wallet must contain at least one account, check Wallets.yaml."
    reportAboutAddressesMissing = error "Each account must contain at least one address, check Wallets.yaml."
