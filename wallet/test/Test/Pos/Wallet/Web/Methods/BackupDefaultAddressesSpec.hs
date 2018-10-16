{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pos.Wallet.Web.Methods.BackupDefaultAddressesSpec
       ( spec
       ) where

import           Universum

import           Pos.Launcher (HasConfigurations)

import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import           Pos.Wallet.Web.ClientTypes (CWallet (..))
import           Pos.Wallet.Web.Methods.Restore (restoreWalletFromBackup)

import           Test.Hspec (Spec, describe, runIO)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Crypto.Arbitrary (genProtocolMagicUniformWithRNM)
import           Test.Pos.Util.QuickCheck.Property (assertProperty)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)
import           Test.QuickCheck (arbitrary, generate)
import           Test.QuickCheck.Monadic (pick)


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
       describe "restoreAddressFromWalletBackup" $ modifyMaxSuccess (const 10) $ do
           (restoreWalletAddressFromBackupSpec (makeNetworkMagic pm))

restoreWalletAddressFromBackupSpec :: HasConfigurations => NetworkMagic -> Spec
restoreWalletAddressFromBackupSpec nm =
    walletPropertySpec restoreWalletAddressFromBackupDesc $ do
        walletBackup   <- pick arbitrary
        restoredWallet <- lift $ restoreWalletFromBackup nm walletBackup
        let noOfAccounts = cwAccountsNumber restoredWallet
        assertProperty (noOfAccounts > 0) $ "Exported wallet has no accounts!"
  where
    restoreWalletAddressFromBackupDesc =
        "Generate wallet backup; "
            <> "Restore it; "
            <> "Check if the wallet has some accounts; "
