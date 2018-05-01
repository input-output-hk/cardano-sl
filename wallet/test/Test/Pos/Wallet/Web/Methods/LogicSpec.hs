{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pos.Wallet.Web.Methods.LogicSpec
       ( spec
       ) where

import           Universum

import           Data.Default (def)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)

import           Pos.Launcher (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Wallet.Web.Methods.Logic (getAccounts, getWallets)
import           Pos.Util.QuickCheck.Property (stopProperty)

import           Test.Pos.Configuration (withDefConfigurations)
import           Test.Pos.Wallet.Web.Mode (WalletProperty)

-- TODO remove HasCompileInfo when MonadWalletWebMode will be splitted.
spec :: Spec
spec = withCompileInfo def $
       withDefConfigurations $ \_ ->
       describe "Pos.Wallet.Web.Methods" $ do
    prop emptyWalletOnStarts emptyWallet
  where
    emptyWalletOnStarts = "wallet must be empty on start"

emptyWallet :: (HasCompileInfo, HasConfigurations) => WalletProperty ()
emptyWallet = do
    wallets <- lift getWallets
    unless (null wallets) $
        stopProperty "Wallets aren't empty"
    accounts <- lift $ getAccounts Nothing
    unless (null accounts) $
        stopProperty "Accounts aren't empty"
