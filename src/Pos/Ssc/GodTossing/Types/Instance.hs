{-# LANGUAGE TypeFamilies #-}

-- | This module defines instance of Ssc for SscGodTossing.

module Pos.Ssc.GodTossing.Types.Instance
       ( -- * Instances
         -- ** instance Ssc SscGodTossing
       ) where

import           Data.Tagged                        (Tagged (..))

import           Pos.Ssc.Class.Types                (Ssc (..))
import           Pos.Ssc.GodTossing.Error           (SeedError)
import           Pos.Ssc.GodTossing.Functions       (filterLocalPayload)
import           Pos.Ssc.GodTossing.LocalData.Types (GtLocalData)
import           Pos.Ssc.GodTossing.Storage.Types   (GtStorage)
import           Pos.Ssc.GodTossing.Types.Type      (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types     (GtContext, GtGlobalState, GtPayload,
                                                     GtProof, createGtContext,
                                                     mkGtProof)

instance Ssc SscGodTossing where
    type SscStorage     SscGodTossing = GtStorage
    type SscLocalData   SscGodTossing = GtLocalData
    type SscPayload     SscGodTossing = GtPayload
    type SscGlobalState SscGodTossing = GtGlobalState
    type SscProof       SscGodTossing = GtProof
    type SscSeedError   SscGodTossing = SeedError
    type SscNodeContext SscGodTossing = GtContext
    mkSscProof = Tagged mkGtProof
    sscFilterPayload = filterLocalPayload
    sscCreateNodeContext = createGtContext
