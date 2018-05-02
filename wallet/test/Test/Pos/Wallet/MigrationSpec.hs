{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.Pos.Wallet.MigrationSpec (spec) where

import           Control.Arrow ((***))
import           Data.Default (def)
import qualified Data.HashMap.Strict as HM
import           Data.SafeCopy
import           Pos.Arbitrary.Core ()
import           Pos.Core (HasConfiguration)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CAccountMeta (..), CCoin (..),
                                             CHash (..), CId (..), CProfile (..), CTxId (..),
                                             CTxMeta (..), CUpdateInfo (..), CWAddressMeta (..),
                                             CWalletAssurance (..), CWalletMeta (..), Wal)
import           Pos.Wallet.Web.ClientTypes.Functions (addressToCId)
import           Pos.Wallet.Web.State.Storage
import           Test.Hspec (Spec, describe, it)
import           Test.Pos.Configuration (withDefConfigurations)
import           Test.QuickCheck
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

instance Migrate WalletTip_v0 where
    type MigrateFrom WalletTip_v0 = WalletSyncState
    migrate  NotSynced          = V0_NotSynced
    migrate (RestoringFrom _ _) = V0_NotSynced
    migrate (SyncedWith h)      = V0_SyncedWith h

instance Migrate WalletInfo_v0 where
    type MigrateFrom WalletInfo_v0 = WalletInfo
    migrate WalletInfo{..} = WalletInfo_v0
        { _v0_wiMeta           = _wiMeta
        , _v0_wiPassphraseLU   = _wiPassphraseLU
        , _v0_wiCreationTime   = _wiCreationTime
        , _v0_wiSyncTip        = migrate _wiSyncState
        , _v0_wsPendingTxs     = _wsPendingTxs
        , _v0_wiIsReady        = _wiIsReady
        }

newtype WalletStorage_Back_v2 = WalletStorage_Back_v2 WalletStorage_v2

instance Migrate WalletStorage_Back_v2 where
  type MigrateFrom WalletStorage_Back_v2 = WalletStorage_v3
  migrate WalletStorage_v3{..} = WalletStorage_Back_v2 $ WalletStorage_v2
      { _v2_wsWalletInfos = _v3_wsWalletInfos
      , _v2_wsAccountInfos = fmap migrate _v3_wsAccountInfos
      , _v2_wsProfile = _v3_wsProfile
      , _v2_wsReadyUpdates = _v3_wsReadyUpdates
      , _v2_wsTxHistory = _v3_wsTxHistory
      , _v2_wsHistoryCache = _v3_wsHistoryCache
      , _v2_wsUtxo = _v3_wsUtxo
      , _v2_wsBalances = _v3_wsBalances
      , _v2_wsUsedAddresses = mapAddrKeys _v3_wsUsedAddresses
      , _v2_wsChangeAddresses = mapAddrKeys _v3_wsChangeAddresses
      }
    where
      mapAddrKeys = HM.fromList . fmap (first addressToCId) . HM.toList

newtype WalletStorage_Back_v3 = WalletStorage_Back_v3 WalletStorage_v3

instance Migrate WalletStorage_Back_v3 where
  type MigrateFrom WalletStorage_Back_v3 = WalletStorage
  migrate WalletStorage{..} = WalletStorage_Back_v3 $ WalletStorage_v3
      { _v3_wsWalletInfos = migrateMapElements _wsWalletInfos
      , _v3_wsAccountInfos = _wsAccountInfos
      , _v3_wsProfile = _wsProfile
      , _v3_wsReadyUpdates = _wsReadyUpdates
      , _v3_wsTxHistory = _wsTxHistory
      , _v3_wsHistoryCache = _wsHistoryCache
      , _v3_wsUtxo = _wsUtxo
      , _v3_wsBalances = _wsBalances
      , _v3_wsUsedAddresses = _wsUsedAddresses
      , _v3_wsChangeAddresses = _wsChangeAddresses
      }
    where
      migrateMapElements = HM.fromList . fmap (second migrate) . HM.toList

--------------------------------------------------------------------------------

deriving instance Eq AccountInfo_v0
deriving instance Eq AddressInfo_v0
deriving instance Eq WalletStorage_v2
deriving instance Eq WalletStorage_v3
deriving instance Eq WalletInfo_v0

deriving instance Show WalletSyncState
deriving instance Show SyncStatistics
deriving instance Show WalletInfo
deriving instance Show AccountInfo_v0
deriving instance Show AccountInfo
deriving instance Show AddressInfo_v0
deriving instance Show AddressInfo
deriving instance Show WalletInfo_v0
deriving instance Show WalletTip_v0
deriving instance Show WalletStorage_v2
deriving instance Show WalletStorage_v3

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

instance Arbitrary RestorationBlockDepth where
    arbitrary = RestorationBlockDepth <$> arbitrary

instance HasConfiguration => Arbitrary WalletTip_v0 where
  arbitrary = oneof
    [ pure V0_NotSynced
    , V0_SyncedWith <$> arbitrary
    ]

instance HasConfiguration => Arbitrary WalletSyncState where
  arbitrary = oneof
    [ pure NotSynced
    , SyncedWith <$> arbitrary
    , RestoringFrom <$> arbitrary <*> arbitrary
    ]

instance Arbitrary SyncStatistics where
  arbitrary = SyncStatistics <$> arbitrary <*> arbitrary

instance Arbitrary SyncThroughput where
  arbitrary = SyncThroughput <$> arbitrary

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

instance HasConfiguration => Arbitrary WalletInfo_v0 where
  arbitrary = WalletInfo_v0
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure HM.empty
    <*> arbitrary

instance HasConfiguration => Arbitrary WalletInfo where
  arbitrary = WalletInfo
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure HM.empty
    <*> arbitrary

instance HasConfiguration => Arbitrary WalletStorage_v2 where
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

instance HasConfiguration => Arbitrary WalletStorage_v3 where
  arbitrary = WalletStorage_v3
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


spec :: Spec
spec = withCompileInfo def $ withDefConfigurations $ \_ ->
    describe "Migration to latest version can be reversed" $ do
      it "(WalletStorage_v2) migrating back results in the original" $ property prop_backMigrate_v2
      it "(WalletStorage_v3) migrating back results in the original" $ property prop_backMigrate_v3
  where
    -- This test verifies that the migration to version 2 of the wallet storage is
    -- reversible, and as such that we don't accidentally cause any data loss in
    -- the conversion.
    prop_backMigrate_v2 :: WalletStorage_v2 -> Bool
    prop_backMigrate_v2 ws = let
        WalletStorage_Back_v2 ws' = migrate . migrate $ ws
      in ws == ws'

    prop_backMigrate_v3 :: WalletStorage_v3 -> Bool
    prop_backMigrate_v3 ws = let
        WalletStorage_Back_v3 ws' = migrate . migrate $ ws
      in ws == ws'
