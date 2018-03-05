{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
module Migrations (tests) where

import           Control.Arrow                        ((***))
import qualified Data.HashMap.Strict                  as HM
import           Data.SafeCopy
import           Pos.Arbitrary.Core                   ()
import           Pos.Wallet.Web.ClientTypes           (AccountId (..), Addr,
                                                       CAccountMeta (..), CCoin (..),
                                                       CHash (..), CId (..),
                                                       CProfile (..), CTxId (..),
                                                       CTxMeta (..), CUpdateInfo (..),
                                                       CWAddressMeta (..),
                                                       CWalletAssurance (..),
                                                       CWalletMeta (..), Wal)
import           Pos.Wallet.Web.ClientTypes.Functions (addressToCId)
import           Pos.Wallet.Web.State.Storage
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Universum

--------------------------------------------------------------------------------
-- Reverse migrations
--
-- These instances serve to allow us to migrate _backwards_ from the current DB
-- version to the previous one. The purpose of this is to verify that the
-- migration does not introduce unwanted changes to the database.
--------------------------------------------------------------------------------

instance Migrate AddressInfo_v0 where
  type MigrateFrom AddressInfo_v0 = AddressInfo
  migrate AddressInfo{..} = AddressInfo_v0
    { _v0_adiCWAddressMeta = wamToCWam adiWAddressMeta
    , _v0_adiSortingKey = adiSortingKey
    }
    where
      wamToCWam (WAddressMeta wid accIdx addrIdx cAddr)
          = CWAddressMeta wid accIdx addrIdx $ addressToCId cAddr

instance Migrate AccountInfo_v0 where
  type MigrateFrom AccountInfo_v0 = AccountInfo
  migrate AccountInfo{..} = AccountInfo_v0
      { _v0_aiMeta = _aiMeta
      , _v0_aiAddresses = mapAddrs _aiAddresses
      , _v0_aiRemovedAddresses = mapAddrs _aiRemovedAddresses
      , _v0_aiUnusedKey = _aiUnusedKey
      }
    where
      mapAddrs =
          HM.fromList
        . fmap (addressToCId *** migrate)
        . HM.toList

newtype WalletStorage_Back_v2 = WalletStorage_Back_v2 WalletStorage_v2

instance Migrate WalletStorage_Back_v2 where
  type MigrateFrom WalletStorage_Back_v2 = WalletStorage
  migrate WalletStorage{..} = WalletStorage_Back_v2 $ WalletStorage_v2
      { _v2_wsWalletInfos = _wsWalletInfos
      , _v2_wsAccountInfos = fmap migrate _wsAccountInfos
      , _v2_wsProfile = _wsProfile
      , _v2_wsReadyUpdates = _wsReadyUpdates
      , _v2_wsTxHistory = _wsTxHistory
      , _v2_wsHistoryCache = _wsHistoryCache
      , _v2_wsUtxo = _wsUtxo
      , _v2_wsBalances = _wsBalances
      , _v2_wsUsedAddresses = mapAddrKeys _wsUsedAddresses
      , _v2_wsChangeAddresses = mapAddrKeys _wsChangeAddresses
      }
    where
      mapAddrKeys = HM.fromList . fmap (first addressToCId) . HM.toList

--------------------------------------------------------------------------------

deriving instance Eq CTxMeta
deriving instance Eq CAccountMeta
deriving instance Eq CProfile
deriving instance Eq CUpdateInfo
deriving instance Eq WalletTip
deriving instance Eq WalletInfo
deriving instance Eq AccountInfo_v0
deriving instance Eq AddressInfo_v0
deriving instance Eq WalletStorage_v2

deriving instance Show WalletTip
deriving instance Show WalletInfo
deriving instance Show AccountInfo_v0
deriving instance Show AddressInfo_v0
deriving instance Show WalletStorage_v2

deriving instance Arbitrary CHash
deriving instance Arbitrary (CId Wal)
deriving instance Arbitrary CTxId
deriving instance Arbitrary CCoin

instance Arbitrary CProfile where
  arbitrary = CProfile <$> arbitrary

instance Arbitrary CTxMeta where
  arbitrary = CTxMeta <$> arbitrary

instance Arbitrary CUpdateInfo where
  arbitrary = CUpdateInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary (CId Addr) where
  arbitrary = addressToCId <$> arbitrary

instance Arbitrary CAccountMeta where
  arbitrary = CAccountMeta <$> arbitrary

instance Arbitrary CWalletAssurance where
  arbitrary = oneof
    [ pure CWAStrict
    , pure CWANormal
    ]

instance Arbitrary WAddressMeta where
  arbitrary = WAddressMeta
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary CWAddressMeta where
  arbitrary = CWAddressMeta
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary CWalletMeta where
  arbitrary = CWalletMeta
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary AccountId where
  arbitrary = AccountId
    <$> arbitrary
    <*> arbitrary

instance Arbitrary WalletTip where
  arbitrary = oneof
    [ pure NotSynced
    , SyncedWith <$> arbitrary
    ]

instance Arbitrary AddressInfo where
  arbitrary = AddressInfo
    <$> arbitrary
    <*> arbitrary
instance Arbitrary AddressInfo_v0 where
  arbitrary = AddressInfo_v0
    <$> arbitrary
    <*> arbitrary

instance Arbitrary AccountInfo where
  arbitrary = AccountInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary AccountInfo_v0 where
  arbitrary = AccountInfo_v0
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary WalletInfo where
  arbitrary = WalletInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure HM.empty
    <*> arbitrary

instance Arbitrary WalletStorage_v2 where
  arbitrary = WalletStorage_v2
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure HM.empty
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

tests :: TestTree
tests = testGroup "Migration"
    [ testProperty "migration to latest version can be reversed" $ prop_backMigrate
    ]
  where
    -- This test verifies that the migration to version 2 of the wallet storage is
    -- reversible, and as such that we don't accidentally cause any data loss in
    -- the conversion.
    prop_backMigrate :: WalletStorage_v2 -> Bool
    prop_backMigrate ws = let
        WalletStorage_Back_v2 ws' = migrate . migrate $ ws
      in ws == ws'
