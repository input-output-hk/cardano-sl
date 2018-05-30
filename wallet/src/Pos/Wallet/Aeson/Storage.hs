{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | These instances are used for `test/printDB` method.
-- Only for debugging and testing purposes.

module Pos.Wallet.Aeson.Storage
       (
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..),
                             ToJSONKey (..))
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Aeson.Types (toJSONKeyText)
import qualified Data.Text as T

import           Pos.Aeson.Crypto ()
import           Pos.Aeson.Txp ()
import           Pos.Client.Txp.History (TxHistoryEntry)
import           Pos.Util.Util (toAesonError)

import           Pos.Wallet.Aeson.ClientTypes ()
import           Pos.Wallet.Web.ClientTypes (AccountId (..), CHash (..), CId (..), CTxId (..))
import           Pos.Wallet.Web.Pending.Types (PendingTx, PtxCondition, PtxSubmitTiming)
import           Pos.Wallet.Web.State.Storage (AccountInfo, AddressInfo, RestorationBlockDepth,
                                               SyncStatistics, SyncThroughput, WAddressMeta,
                                               WalletInfo, WalletStorage, WalletSyncState)

instance FromJSON (CId a) => FromJSONKey (CId a) where
    fromJSONKey = FromJSONKeyTextParser parser
      where
        parser = pure . CId . CHash

instance ToJSON (CId a) => ToJSONKey (CId a) where
    toJSONKey = toJSONKeyText (\(CId (CHash t)) -> t)

instance FromJSON CTxId => FromJSONKey CTxId where
    fromJSONKey = FromJSONKeyTextParser parser
      where
        parser = pure . CTxId . CHash

instance ToJSON CTxId => ToJSONKey CTxId where
    toJSONKey = toJSONKeyText (\(CTxId (CHash t)) -> t)


accountIdFromText :: Text -> Either Text AccountId
accountIdFromText t = case T.splitOn "@" t of
    [walId, idx] -> AccountId (CId $ CHash walId) <$> readEither idx
    _            -> Left $ "Invalid AccountId " <> t

instance FromJSON AccountId => FromJSONKey AccountId where
    fromJSONKey = FromJSONKeyTextParser (toAesonError . accountIdFromText)

instance ToJSON AccountId => ToJSONKey AccountId where
    toJSONKey = toJSONKeyText pretty

deriveJSON defaultOptions ''PtxSubmitTiming
deriveJSON defaultOptions ''PendingTx
deriveJSON defaultOptions ''PtxCondition
deriveJSON defaultOptions ''TxHistoryEntry
deriveJSON defaultOptions ''WalletSyncState
deriveJSON defaultOptions ''AddressInfo
deriveJSON defaultOptions ''AccountInfo
deriveJSON defaultOptions ''AccountId
deriveJSON defaultOptions ''WAddressMeta
deriveJSON defaultOptions ''WalletInfo
deriveJSON defaultOptions ''RestorationBlockDepth
deriveJSON defaultOptions ''WalletStorage
deriveJSON defaultOptions ''SyncThroughput
deriveJSON defaultOptions ''SyncStatistics
