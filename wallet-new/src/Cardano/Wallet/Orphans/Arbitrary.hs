{- | QuickCheck Orphans. |-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Cardano.Wallet.Orphans.Arbitrary where

import           Universum

import           Pos.Wallet.Web.ClientTypes.Types
import           Servant
import           Test.QuickCheck (Arbitrary (..))
import           Pos.Wallet.Web.Methods.Misc (WalletStateSnapshot (..))
import           Data.Default (def)

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

instance Arbitrary WalletStateSnapshot where
    arbitrary = WalletStateSnapshot <$> pure def
