{-# LANGUAGE TemplateHaskell #-}

module Pos.Ssc.GodTossing.LocalData.Types
       ( GtLocalData (..)
       , ldModifier
       , ldEpoch
       , ldSize
       ) where

import           Control.Lens                  (makeLenses)
import           Serokell.Data.Memory.Units    (Byte)

import           Pos.Core                      (EpochIndex)
import           Pos.Ssc.GodTossing.Toss.Types (TossModifier)

data GtLocalData = GtLocalData
    { -- | 'TossModifier' which also serves as mempool of GT data,
      -- because for GodTossing modifier and mempool are same.
      _ldModifier :: !TossModifier
    , -- | Epoch for which this mempool can be used to form payload.
      _ldEpoch    :: !EpochIndex
    , -- | Approximate size of this mempool (raw bytes).
      _ldSize     :: !Byte
    }

makeLenses ''GtLocalData
