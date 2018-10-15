{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pos.Wallet.Web.Methods.BackupDefaultAddressesSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, beforeAll_, describe, runIO)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck (arbitrary, generate)
import           Test.QuickCheck.Monadic (pick)

import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.Wlog (setupTestLogging)
import           Pos.Wallet.Web.ClientTypes (CWallet (..))
import           Pos.Wallet.Web.Methods.Restore (restoreWalletFromBackup)

import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Util.QuickCheck.Property (assertProperty)
import           Test.Pos.Wallet.Web.Mode (walletPropertySpec)

spec :: Spec
spec = do
    runWithMagic RequiresNoMagic
    runWithMagic RequiresMagic

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = beforeAll_ setupTestLogging $
    withProvidedMagicConfig pm $ \genesisConfig _ _ ->
        describe "restoreAddressFromWalletBackup" $ modifyMaxSuccess (const 10) $ do
            restoreWalletAddressFromBackupSpec genesisConfig

restoreWalletAddressFromBackupSpec :: HasConfigurations => Genesis.Config -> Spec
restoreWalletAddressFromBackupSpec genesisConfig =
    walletPropertySpec restoreWalletAddressFromBackupDesc $ do
        walletBackup   <- pick arbitrary
        restoredWallet <- lift
            $ restoreWalletFromBackup genesisConfig walletBackup
        let noOfAccounts = cwAccountsNumber restoredWallet
        assertProperty (noOfAccounts > 0) $ "Exported wallet has no accounts!"
  where
    restoreWalletAddressFromBackupDesc =
        "Generate wallet backup; "
            <> "Restore it; "
            <> "Check if the wallet has some accounts; "
