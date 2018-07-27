{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Aeson instances for core

module Pos.Aeson.Core
       (
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), FromJSONKey (..),
                     FromJSONKeyFunction (..), ToJSON (toJSON), ToJSONKey (..),
                     object, withObject, (.:), (.=))
import qualified Data.Aeson.Options as S (defaultOptions)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.Aeson.Types (toJSONKeyText)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import           Data.Time.Units (Microsecond, Millisecond, Second)
import           Formatting (sformat)
import           Serokell.Util.Base64 (JsonByteString (..))

import           Pos.Aeson.Fee ()
import           Pos.Core.Attributes (Attributes, UnparsedFields (..))
import           Pos.Core.Binary ()
import           Pos.Core.Common (Address, BlockCount (..), ChainDifficulty,
                     Script (..), addressF,
                     decodeTextAddress)
import           Pos.Core.Slotting (LocalSlotIndex,
                     SlotCount (..), SlotId, Timestamp (..))
import           Pos.Util.Util (toAesonError)

deriving instance ToJSON SlotCount

deriveJSON S.defaultOptions ''Millisecond
deriveJSON S.defaultOptions ''Microsecond
deriveJSON S.defaultOptions ''Second

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

deriveJSON defaultOptions ''ChainDifficulty
deriveJSON defaultOptions ''SlotId
deriveJSON defaultOptions ''LocalSlotIndex
