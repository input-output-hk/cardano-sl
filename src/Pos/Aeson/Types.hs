{-# LANGUAGE TemplateHaskell #-}

module Pos.Aeson.Types
       (
       ) where

import           Data.Aeson          (ToJSON (toJSON))
import           Data.Aeson.TH       (defaultOptions, deriveToJSON)
import           Pos.Aeson.Crypto    ()
import           Pos.Data.Attributes (Attributes (..))
import           Pos.Types.Types     (Address, Coin, EpochIndex, LocalSlotIndex,
                                      SharedSeed, SlotId, Tx, TxIn, TxOut)
import           Pos.Web.Types       (GodTossingStage)

import           Universum

instance ToJSON SharedSeed where
    toJSON = toJSON . pretty

instance ToJSON a => ToJSON (Attributes a) where
    toJSON = toJSON . attrData

deriveToJSON defaultOptions ''Address
deriveToJSON defaultOptions ''Tx
deriveToJSON defaultOptions ''TxOut
deriveToJSON defaultOptions ''TxIn
deriveToJSON defaultOptions ''SlotId
deriveToJSON defaultOptions ''LocalSlotIndex
deriveToJSON defaultOptions ''EpochIndex
deriveToJSON defaultOptions ''Coin
deriveToJSON defaultOptions ''GodTossingStage
