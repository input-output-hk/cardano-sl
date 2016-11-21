{-# LANGUAGE TypeFamilies #-}

-- | This module defines instance of Ssc for SscGodTossing.

module Pos.Ssc.GodTossing.Types.Instance
       ( -- * Instances
         -- ** instance Ssc SscGodTossing
       ) where

import           Data.Tagged                        (Tagged (..))

import           Pos.Ssc.Class.Types                (Ssc (..))
import           Pos.Ssc.GodTossing.Error           (SeedError)
import           Pos.Ssc.GodTossing.LocalData.Types (GtLocalData)
import           Pos.Ssc.GodTossing.Storage.Types   (GtStorage)
import           Pos.Ssc.GodTossing.Types.Type      (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types     (GtMessage, GtPayload, GtProof,
                                                     mkGtProof)

instance Ssc SscGodTossing where
    type SscStorage   SscGodTossing = GtStorage
    type SscLocalData SscGodTossing = GtLocalData
    type SscPayload   SscGodTossing = GtPayload
    type SscProof     SscGodTossing = GtProof
    type SscMessage   SscGodTossing = GtMessage
    type SscSeedError SscGodTossing = SeedError
    mkSscProof = Tagged mkGtProof
