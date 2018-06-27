{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Functions for working with Wallets configuration file.

module Bench.Cardano.Wallet.Config.Wallets
    ( getWalletsConfig
    ) where

import           Universum

import           Data.Aeson.Types (typeMismatch)
import           Data.Yaml ((.:))
import qualified Data.Yaml as Yaml

import           Bench.Cardano.Wallet.Types (Wallet (..), WalletAccount (..),
                     WalletsConfig (..))
import           Pos.Wallet.Web.ClientTypes (CAccountId (..), CHash (..),
                     CId (..))

instance Yaml.FromJSON CAccountId where
    parseJSON (Yaml.String t) = return $ CAccountId t
    parseJSON invalid         = typeMismatch "CAccountId" invalid

instance Yaml.FromJSON (CId a) where
    parseJSON (Yaml.String h) = return $ CId (CHash h)
    parseJSON invalid         = typeMismatch "CId a" invalid

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
    (Yaml.decodeFileEither pathToConfig :: IO (Either Yaml.ParseException WalletsConfig)) >>= \case
        Left e     -> reportAboutInvalidConfig (show e)
        Right conf -> return conf
  where
    reportAboutInvalidConfig :: String -> IO a
    reportAboutInvalidConfig str = error . toText $
        "Unable to read endpoints configuration " <> pathToConfig <> ": " <> str
