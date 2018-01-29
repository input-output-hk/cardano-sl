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
        Just conf -> pure conf
  where
    reportAboutInvalidConfig :: IO a
    reportAboutInvalidConfig = error . toText $
        "Unable to read endpoints configuration " <> pathToConfig
