-- | These instances are used for `test/printDB` method.
-- Only for debugging and testing purposes.

module Pos.Aeson.Storage where

import           Universum

import           Data.Aeson                   (FromJSON (..), FromJSONKey (..),
                                               FromJSONKeyFunction (..), ToJSON (..),
                                               ToJSONKey (..))
import           Data.Aeson.TH                (defaultOptions, deriveJSON)
import           Data.Aeson.Types             (toJSONKeyText)
import qualified Data.Text                    as T
import           Formatting                   (build, int, sformat, (%))
import qualified Serokell.Util.Base16         as B16
import           Serokell.Util.Base64         (JsonByteString (..))

import           Pos.Aeson.ClientTypes        ()
import           Pos.Aeson.Crypto             ()
import           Pos.Client.Txp.History       (TxHistoryEntry)
import           Pos.Core                     (Script)
import           Pos.Crypto                   (decodeAbstractHash, hashHexF)
import           Pos.Txp                      (Tx, TxAux, TxIn (..), TxInWitness,
                                               TxOutAux, TxSigData)
import           Pos.Util.Util                (eitherToFail)

import           Pos.Wallet.Web.ClientTypes   (AccountId (..), CHash (..), CId (..),
                                               CTxId (..))
import           Pos.Wallet.Web.Pending.Types (PendingTx, PtxCondition, PtxSubmitTiming)
import           Pos.Wallet.Web.State.Storage (AccountInfo, AddressInfo, WalletInfo,
                                               WalletStorage, WalletTip)

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

txInFromText :: Text -> Either Text TxIn
txInFromText t = case T.splitOn "_" t of
    ["TxInUtxo", h, idx]     -> TxInUtxo <$> decodeAbstractHash h <*> readEither idx
    ["TxInUnknown", tag, bs] -> TxInUnknown <$> readEither tag <*> B16.decode bs
    _                        -> fail $ toString $ "Invalid TxIn " <> t

instance FromJSON TxIn => FromJSONKey TxIn where
    fromJSONKey = FromJSONKeyTextParser (eitherToFail . txInFromText)

txInToText :: TxIn -> Text
txInToText TxInUtxo {..} = sformat ("TxInUtxo_"%hashHexF%"_"%int) txInHash txInIndex
txInToText (TxInUnknown tag bs) = sformat ("TxInUnknown_"%int%"_"%B16.base16F) tag bs

instance ToJSONKey TxIn where
    toJSONKey = toJSONKeyText txInToText

accountIdFromText :: Text -> Either Text AccountId
accountIdFromText t = case T.splitOn "@" t of
    [walId, idx] -> AccountId (CId $ CHash walId) <$> readEither idx
    _            -> fail $ toString $ "Invalid AccountId " <> t

instance FromJSON AccountId => FromJSONKey AccountId where
  fromJSONKey = FromJSONKeyTextParser (eitherToFail . accountIdFromText)

instance ToJSON AccountId => ToJSONKey AccountId where
    toJSONKey = toJSONKeyText (sformat build)

-- TODO move to core and to txp
-- here because of FromJSON/ToJSON for ByteString
-- I didn't want to move these instances to core or txp

deriveJSON defaultOptions ''Script
-- Txp

instance ToJSON ByteString where
    toJSON = toJSON . JsonByteString

instance FromJSON ByteString where
    parseJSON v = getJsonByteString <$> parseJSON v

deriveJSON defaultOptions ''TxIn
deriveJSON defaultOptions ''Tx
deriveJSON defaultOptions ''TxOutAux
deriveJSON defaultOptions ''TxAux
deriveJSON defaultOptions ''TxSigData
deriveJSON defaultOptions ''TxInWitness

-- Wallet
deriveJSON defaultOptions ''PtxSubmitTiming
deriveJSON defaultOptions ''PendingTx
deriveJSON defaultOptions ''PtxCondition
deriveJSON defaultOptions ''TxHistoryEntry
deriveJSON defaultOptions ''WalletTip
deriveJSON defaultOptions ''AddressInfo
deriveJSON defaultOptions ''AccountInfo
deriveJSON defaultOptions ''AccountId
deriveJSON defaultOptions ''WalletInfo
deriveJSON defaultOptions ''WalletStorage
