{-# LANGUAGE TemplateHaskell #-}

module Pos.Aeson.Types
       (
       ) where

import           Data.Aeson          (FromJSON, ToJSON (toJSON))
import           Data.Aeson.TH       (defaultOptions, deriveToJSON)
import           Pos.Aeson.Crypto    ()
import           Pos.Data.Attributes (Attributes (..))
import           Pos.Types           (Address, ApplicationName (..), ChainDifficulty,
                                      Coin, EpochIndex, LocalSlotIndex, SharedSeed,
                                      SlotId)
import           Pos.Web.Types       (GodTossingStage)
import           Universum

instance ToJSON SharedSeed where
    toJSON = toJSON . pretty

instance ToJSON a => ToJSON (Attributes a) where
    toJSON = toJSON . attrData

-- NOTE: some of these types are used on frontend (PureScript).
-- We are automatically deriving instances there and they are
-- compitable now with `deriveToJSON defaultOptions ''Y`.
-- If datatype is used on frontend, please use this instead of
-- any other way of deriving if possible.

deriveToJSON defaultOptions ''ApplicationName
deriveToJSON defaultOptions ''ChainDifficulty
deriveToJSON defaultOptions ''Address
deriveToJSON defaultOptions ''SlotId
deriveToJSON defaultOptions ''LocalSlotIndex
deriveToJSON defaultOptions ''EpochIndex
deriveToJSON defaultOptions ''Coin
deriveToJSON defaultOptions ''GodTossingStage
