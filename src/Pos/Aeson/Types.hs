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

deriving instance ToJSON ApplicationName
deriving instance FromJSON ApplicationName

deriveToJSON defaultOptions ''ChainDifficulty
deriveToJSON defaultOptions ''Address
deriveToJSON defaultOptions ''SlotId
deriveToJSON defaultOptions ''LocalSlotIndex
deriveToJSON defaultOptions ''EpochIndex
deriveToJSON defaultOptions ''Coin
deriveToJSON defaultOptions ''GodTossingStage
