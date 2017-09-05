module Test.Pos.Wallet.Web.Methods.LogicSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec                   (Spec, describe)
import           Test.Hspec.QuickCheck        (prop)

import           Pos.Util.CompileInfo         (retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Wallet.Web.Methods.Logic (getAccounts, getWallets)

import           Test.Pos.Util                (stopProperty, withDefConfiguration,
                                               withDefConfiguration,
                                               withDefGtConfiguration,
                                               withDefInfraConfiguration,
                                               withDefNodeConfiguration,
                                               withDefUpdateConfiguration)
import           Test.Pos.Wallet.Web.Mode     (HasWalletSpecConfiguration, WalletProperty)

spec :: Spec
spec = withCompileInfo $(retrieveCompileTimeInfo) $
       withDefConfiguration $
       withDefGtConfiguration $
       withDefInfraConfiguration $
       withDefUpdateConfiguration $
       withDefNodeConfiguration $ do
    describe "Pos.Wallet.Web.Methods" $ do
        prop emptyWalletOnStarts emptyWallet
  where
    emptyWalletOnStarts = "wallet must be empty on start"

emptyWallet :: HasWalletSpecConfiguration => WalletProperty ()
emptyWallet = do
    wallets <- lift getWallets
    unless (null wallets) $
        stopProperty "Wallets aren't empty"
    accounts <- lift $ getAccounts Nothing
    unless (null accounts) $
        stopProperty "Accounts aren't empty"
