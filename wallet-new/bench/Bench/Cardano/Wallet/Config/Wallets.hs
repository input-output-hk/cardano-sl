{-# LANGUAGE ApplicativeDo #-}

-- | Functions for working with Wallets configuration file.

module Bench.Cardano.Wallet.Config.Wallets
    ( getWalletsConfig
    ) where

import           Universum

import qualified Data.Yaml                  as Yaml
import           Data.Yaml                  ((.:))
import           Data.Aeson.Types           (typeMismatch)

import           Bench.Cardano.Wallet.Types (WalletAccount (..), Wallet (..),
                                             WalletsConfig (..))
import           Pos.Wallet.Web.ClientTypes (CAccountId (..), CId (..), CHash (..))

instance Yaml.FromJSON CAccountId where
    parseJSON (Yaml.String t) = return $ CAccountId t
    parseJSON invalid = typeMismatch "CAccountId" invalid

instance Yaml.FromJSON (CId a) where
    parseJSON (Yaml.String h) = return $ CId (CHash h)
    parseJSON invalid = typeMismatch "CId a" invalid

instance Yaml.FromJSON WalletAccount where
    parseJSON (Yaml.Object o) = WalletAccount
        <$> o .: "AccountId"
        <*> o .: "Addresses"
    parseJSON invalid = typeMismatch "WalletAccount" invalid

instance Yaml.FromJSON Wallet where
    parseJSON (Yaml.Object o) = Wallet
        <$> o .: "WalletId"
        <*> o .: "Accounts"
    parseJSON invalid = typeMismatch "Wallet" invalid

instance Yaml.FromJSON WalletsConfig where
    parseJSON (Yaml.Object o) = WalletsConfig
        <$> o .: "Wallets"
    parseJSON invalid = typeMismatch "WalletsConfig" invalid

-- | Reads Wallets configuration from the local .yaml-file.
getWalletsConfig :: FilePath -> IO WalletsConfig
getWalletsConfig pathToConfig =
    (Yaml.decodeFile pathToConfig :: IO (Maybe WalletsConfig)) >>= \case
        Nothing   -> reportAboutInvalidConfig
        Just conf -> return conf
  where
    reportAboutInvalidConfig :: IO a
    reportAboutInvalidConfig = error . toText $
        "Unable to read endpoints configuration " <> pathToConfig
