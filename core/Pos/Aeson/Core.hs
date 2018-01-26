-- | Aeson instances for core

module Pos.Aeson.Core
       (
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..),
                             ToJSON (toJSON), ToJSONKey (..), object, withObject, (.:), (.=))
import           Data.Aeson.TH (defaultOptions, deriveFromJSON, deriveJSON, deriveToJSON)
import           Data.Aeson.Types (toJSONKeyText)
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
import           Pos.Core.Slotting.Types (EpochIndex (..), LocalSlotIndex, SlotCount (..), SlotId,
                                          Timestamp (..))
import           Pos.Core.Ssc.Types (VssCertificate)
import           Pos.Core.Update.Types (ApplicationName (..), BlockVersion, BlockVersionData,
                                        SoftforkRule, SoftwareVersion (..), mkApplicationName)
import           Pos.Data.Attributes (Attributes, UnparsedFields (..))
import           Pos.Util.Util (toAesonError)

instance ToJSON SharedSeed where
    toJSON = toJSON . JsonByteString . getSharedSeed

instance FromJSON SharedSeed where
    parseJSON v = SharedSeed . getJsonByteString <$> parseJSON v

instance FromJSON (AsBinary w) where
    parseJSON v = AsBinary . getJsonByteString <$> parseJSON v

deriving instance ToJSON SlotCount

instance FromJSON CoinPortion where
    parseJSON v = unsafeCoinPortionFromDouble <$> parseJSON v

instance ToJSON CoinPortion where
    toJSON = toJSON . coinPortionToDouble

deriveFromJSON S.defaultOptions ''VssCertificate
deriveFromJSON S.defaultOptions ''Millisecond
deriveFromJSON S.defaultOptions ''Microsecond
deriveFromJSON S.defaultOptions ''Second
deriveFromJSON S.defaultOptions ''SoftforkRule
deriveFromJSON S.defaultOptions ''BlockVersionData
deriveToJSON   S.defaultOptions ''Microsecond
deriveJSON       defaultOptions ''SoftwareVersion

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
    parseJSON v = UnparsedFields . Map.map getJsonByteString <$> parseJSON v

instance ToJSON UnparsedFields where
    toJSON (UnparsedFields fields) = toJSON (Map.map JsonByteString fields)

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
    -- mkApplicationName will validate the text to be an appropriate app name
    --
    -- FIXME does the defaultOptions derived JSON encode directly as text? Or
    -- as an object with a single key?
    parseJSON v = parseJSON v >>= toAesonError . mkApplicationName

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
