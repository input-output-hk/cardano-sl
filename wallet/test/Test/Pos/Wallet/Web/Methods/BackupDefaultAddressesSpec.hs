{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pos.Wallet.Web.Methods.BackupDefaultAddressesSpec
       ( spec
       ) where

import           Universum

import           Pos.Launcher (HasConfigurations)

import           Pos.Util.Wlog (setupTestLogging)
import           Pos.Wallet.Web.ClientTypes (CWallet (..))
import           Pos.Wallet.Web.Methods.Restore (restoreWalletFromBackup)
import           Test.Hspec (Spec, beforeAll_, describe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.Pos.Chain.Genesis.Dummy (dummyConfig)
import           Test.Pos.Configuration (withDefConfigurations)
import           Test.Pos.Util.QuickCheck.Property (assertProperty)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)
import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Monadic (pick)

spec :: Spec
spec = beforeAll_ setupTestLogging $
            withDefConfigurations $ \_ _ _ ->
                describe "restoreAddressFromWalletBackup" $ modifyMaxSuccess (const 10) $ do
                    restoreWalletAddressFromBackupSpec

restoreWalletAddressFromBackupSpec :: HasConfigurations => Spec
restoreWalletAddressFromBackupSpec =
    walletPropertySpec restoreWalletAddressFromBackupDesc $ do
        walletBackup   <- pick arbitrary
        restoredWallet <- lift
            $ restoreWalletFromBackup dummyConfig walletBackup
        let noOfAccounts = cwAccountsNumber restoredWallet
        assertProperty (noOfAccounts > 0) $ "Exported wallet has no accounts!"
  where
    restoreWalletAddressFromBackupDesc =
        "Generate wallet backup; "
            <> "Restore it; "
            <> "Check if the wallet has some accounts; "
