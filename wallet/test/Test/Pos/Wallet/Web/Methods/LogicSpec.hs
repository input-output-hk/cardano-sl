{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pos.Wallet.Web.Methods.LogicSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe, runIO)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, generate)

import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import           Pos.Launcher (HasConfigurations)
import           Pos.Wallet.Web.Methods.Logic (getAccounts, getWallets)

import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Util.QuickCheck.Property (stopProperty)
import           Test.Pos.Wallet.Web.Mode (WalletProperty)

-- TODO remove HasCompileInfo when MonadWalletWebMode will be splitted.
spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = withProvidedMagicConfig pm $
       describe "Pos.Wallet.Web.Methods" $ do
    prop emptyWalletOnStarts (emptyWallet (makeNetworkMagic pm))
  where
    emptyWalletOnStarts = "wallet must be empty on start"

emptyWallet :: HasConfigurations => NetworkMagic -> WalletProperty ()
emptyWallet _nm = do
    wallets <- lift getWallets
    unless (null wallets) $
        stopProperty "Wallets aren't empty"
    accounts <- lift $ getAccounts Nothing
    unless (null accounts) $
        stopProperty "Accounts aren't empty"
