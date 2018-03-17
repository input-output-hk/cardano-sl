{- | QuickCheck Orphans. |-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Cardano.Wallet.Orphans.Arbitrary where

import           Universum

import           Data.Default (def)
import           Pos.Wallet.Web.ClientTypes.Types
import           Pos.Wallet.Web.Methods.Misc (WalletStateSnapshot (..))
import           Pos.Wallet.Web.State.Storage (WalletStorage (..))
import           Servant
import           Test.QuickCheck (Arbitrary (..))

instance Arbitrary NoContent where
    arbitrary = pure NoContent

instance Arbitrary CWalletInit
instance Arbitrary CWalletMeta
instance Arbitrary CFilePath
instance Arbitrary CAccountMeta
instance Arbitrary CAccountInit
instance Arbitrary CProfile
instance Arbitrary CTxMeta
instance Arbitrary CWalletRedeem
instance Arbitrary CPaperVendWalletRedeem
instance Arbitrary CInitialized
instance Arbitrary NewBatchPayment

instance Arbitrary CCoin where
    arbitrary = CCoin <$> arbitrary

instance Arbitrary CUpdateInfo where
    arbitrary = CUpdateInfo <$> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary
                            <*> arbitrary

instance Arbitrary WalletStateSnapshot where
    arbitrary = do
        v <- arbitrary
        WalletStateSnapshot <$> pure (def {_wsReadyUpdates = [v]})
