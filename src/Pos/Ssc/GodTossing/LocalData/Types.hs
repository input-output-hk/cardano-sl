{-# LANGUAGE TemplateHaskell #-}

module Pos.Ssc.GodTossing.LocalData.Types
       ( GtLocalData (..)
       , ldModifier
       , ldEpoch
       ) where

import           Control.Lens                  (makeLenses)
-- import           Universum

import           Pos.Ssc.GodTossing.Toss.Types (TossModifier)
import           Pos.Types                     (EpochIndex)

data GtLocalData = GtLocalData
    { -- | 'TossModifier' which also serves as mempool of GT data,
      -- because for GodTossing modifier and mempool are same.
      _ldModifier :: !TossModifier
    , -- | Epoch for which this mempool can be used to form payload.
      _ldEpoch    :: !EpochIndex
    }

makeLenses ''GtLocalData
