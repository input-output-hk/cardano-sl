{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pos.Wallet.Web.Methods.LogicSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe, runIO)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (generate)

import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import           Pos.Launcher (HasConfigurations)
import           Pos.Wallet.Web.Methods.Logic (getAccounts, getWallets)

import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Crypto.Arbitrary (genProtocolMagicUniformWithRNM)
import           Test.Pos.Util.QuickCheck.Property (stopProperty)
import           Test.Pos.Wallet.Web.Mode (WalletProperty)

-- TODO remove HasCompileInfo when MonadWalletWebMode will be splitted.

-- We run the tests this number of times, with different `ProtocolMagics`, to get increased
-- coverage. We should really do this inside of the `prop`, but it is difficult to do that
-- without significant rewriting of the testsuite.
testMultiple :: Int
testMultiple = 1

spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = replicateM_ testMultiple $
    modifyMaxSuccess (`div` testMultiple) $ do
        pm <- runIO (generate (genProtocolMagicUniformWithRNM rnm))
        describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
            specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = withProvidedMagicConfig pm $
       describe "Pos.Wallet.Web.Methods" $ do
    prop emptyWalletOnStarts (emptyWallet (makeNetworkMagic pm))
  where
    emptyWalletOnStarts = "wallet must be empty on start"

emptyWallet :: HasConfigurations => NetworkMagic -> WalletProperty ()
emptyWallet nm = do
    wallets <- lift (getWallets nm)
    unless (null wallets) $
        stopProperty "Wallets aren't empty"
    accounts <- lift $ getAccounts nm Nothing
    unless (null accounts) $
        stopProperty "Accounts aren't empty"
