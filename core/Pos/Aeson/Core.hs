-- | Aeson instances for core

module Pos.Aeson.Core
       (
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..),
                             ToJSON (toJSON), ToJSONKey (..), object, withObject, (.:), (.=))
import           Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import           Data.Aeson.Types (toJSONKeyText)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import           Data.Time.Units (Microsecond, Millisecond, Second)
import           Formatting (sformat)
import qualified Serokell.Aeson.Options as S (defaultOptions)
import           Serokell.Util.Base64 (JsonByteString (..))

import           Pos.Aeson.Crypto ()
import           Pos.Aeson.Fee ()
import           Pos.Binary.Class (AsBinary (..))
import           Pos.Binary.Core ()
import           Pos.Core.Common (Address, BlockCount (..), ChainDifficulty, Coin, CoinPortion,
                                  Script (..), SharedSeed (..), addressF, coinPortionToDouble,
                                  decodeTextAddress, mkCoin, unsafeCoinPortionFromDouble,
                                  unsafeGetCoin)
import           Pos.Core.Delegation (HeavyDlgIndex (..))
import           Pos.Core.Slotting.Types (EpochIndex (..), LocalSlotIndex, SlotCount (..), SlotId,
                                          Timestamp (..))
import           Pos.Core.Ssc.Types (VssCertificate)
import           Pos.Core.Update.Types (ApplicationName (..), BlockVersion, BlockVersionData,
                                        SoftforkRule, SoftwareVersion (..))
import           Pos.Data.Attributes (Attributes, UnparsedFields (..))
import           Pos.Util.Util (toAesonError)

instance ToJSON SharedSeed where
    toJSON = toJSON . JsonByteString . getSharedSeed

instance FromJSON SharedSeed where
    parseJSON v = SharedSeed . getJsonByteString <$> parseJSON v

instance ToJSON (AsBinary w) where
    toJSON = toJSON . JsonByteString . getAsBinary

instance FromJSON (AsBinary w) where
    parseJSON v = AsBinary . getJsonByteString <$> parseJSON v

deriving instance ToJSON SlotCount

instance FromJSON CoinPortion where
    parseJSON v = unsafeCoinPortionFromDouble <$> parseJSON v

instance ToJSON CoinPortion where
    toJSON = toJSON . coinPortionToDouble

deriveJSON S.defaultOptions ''VssCertificate
deriveJSON S.defaultOptions ''Millisecond
deriveJSON S.defaultOptions ''Microsecond
deriveJSON S.defaultOptions ''Second
deriveJSON S.defaultOptions ''SoftforkRule
deriveJSON S.defaultOptions ''BlockVersionData
deriveJSON defaultOptions ''SoftwareVersion

deriving instance FromJSON Timestamp
deriving instance ToJSON Timestamp

instance ToJSON Script where
    toJSON Script{..} = object [
        "version"    .= scrVersion,
        "script" .= JsonByteString scrScript ]

instance FromJSON Script where
    parseJSON = withObject "Script" $ \obj -> do
        scrVersion <- obj .: "version"
        scrScript  <- getJsonByteString <$> obj .: "script"
        pure $ Script {..}

instance FromJSON UnparsedFields where
    parseJSON v = UnparsedFields . Map.map (LBS.fromStrict . getJsonByteString) <$> parseJSON v

instance ToJSON UnparsedFields where
    toJSON (UnparsedFields fields) = toJSON (Map.map (JsonByteString . LBS.toStrict) fields)

deriveJSON defaultOptions ''Attributes

instance FromJSONKey Address where
    fromJSONKey = FromJSONKeyTextParser (toAesonError . decodeTextAddress)

instance ToJSONKey Address where
    toJSONKey = toJSONKeyText (sformat addressF)

instance FromJSON Address where
    parseJSON = toAesonError . decodeTextAddress <=< parseJSON

instance ToJSON Address where
    toJSON = toJSON . sformat addressF

-- NOTE: some of these types below are used on frontend (PureScript).
-- We are automatically deriving instances there and they are
-- compitable now with `deriveToJSON defaultOptions ''Y`.
-- If datatype is used on frontend, please use this instead of
-- any other way of deriving if possible.

deriveJSON defaultOptions ''BlockCount

instance FromJSON ApplicationName where
    -- FIXME does the defaultOptions derived JSON encode directly as text? Or
    -- as an object with a single key?
    parseJSON v = ApplicationName <$> parseJSON v

deriveToJSON defaultOptions ''ApplicationName

deriveJSON defaultOptions ''ChainDifficulty
deriveJSON defaultOptions ''SlotId
deriveJSON defaultOptions ''LocalSlotIndex
deriveJSON defaultOptions ''BlockVersion

instance FromJSON Coin where
    parseJSON v = mkCoin <$> parseJSON v
instance ToJSON Coin where
    toJSON = toJSON . unsafeGetCoin

deriving instance FromJSON EpochIndex
deriving instance ToJSON EpochIndex

instance FromJSON HeavyDlgIndex where
    parseJSON v = HeavyDlgIndex <$> parseJSON v

instance ToJSON HeavyDlgIndex where
    toJSON = toJSON . getHeavyDlgIndex
