-- | Aeson instances for core

module Pos.Aeson.Core
       (
       ) where

import           Universum

import           Data.Aeson             (FromJSON (..), ToJSON (toJSON))
import           Data.Aeson.TH          (defaultOptions, deriveFromJSON, deriveJSON,
                                         deriveToJSON)
import           Data.Time.Units        (Microsecond, Millisecond)
import qualified Serokell.Aeson.Options as S (defaultOptions)
import           Serokell.Util.Base64   (JsonByteString (..))

import           Pos.Aeson.Crypto       ()
import           Pos.Aeson.Fee          ()
import           Pos.Binary.Class       (AsBinary (..))
import           Pos.Core.Types         (ApplicationName (..), BlockCount (..),
                                         BlockVersionData, ChainDifficulty, Coin,
                                         CoinPortion, EpochIndex (..), LocalSlotIndex,
                                         SharedSeed (..), SlotCount (..), SlotId,
                                         SoftforkRule, Timestamp,
                                         unsafeCoinPortionFromDouble)
import           Pos.Core.Vss           (VssCertificate)

instance ToJSON SharedSeed where
    toJSON = toJSON . JsonByteString . getSharedSeed

instance FromJSON SharedSeed where
    parseJSON v = SharedSeed . getJsonByteString <$> parseJSON v

instance FromJSON (AsBinary w) where
    parseJSON v = AsBinary . getJsonByteString <$> parseJSON v

deriving instance ToJSON SlotCount

instance FromJSON CoinPortion where
    parseJSON v = unsafeCoinPortionFromDouble <$> parseJSON v

deriveFromJSON S.defaultOptions ''VssCertificate
deriveFromJSON S.defaultOptions ''Millisecond
deriveFromJSON S.defaultOptions ''Microsecond
deriveFromJSON S.defaultOptions ''Timestamp
deriveFromJSON S.defaultOptions ''SoftforkRule
deriveFromJSON S.defaultOptions ''BlockVersionData

-- NOTE: some of these types below are used on frontend (PureScript).
-- We are automatically deriving instances there and they are
-- compitable now with `deriveToJSON defaultOptions ''Y`.
-- If datatype is used on frontend, please use this instead of
-- any other way of deriving if possible.

deriveToJSON defaultOptions ''BlockCount
deriveToJSON defaultOptions ''ApplicationName
deriveToJSON defaultOptions ''ChainDifficulty
deriveToJSON defaultOptions ''SlotId
deriveToJSON defaultOptions ''LocalSlotIndex
deriveJSON defaultOptions ''Coin
deriveJSON defaultOptions ''EpochIndex
deriveJSON defaultOptions ''Coin
