{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines instance of Ssc for SscGodTossing.

module Pos.Modern.Ssc.GodTossing.Types.Instance
       ( -- * Instances
         -- ** instance Ssc SscGodTossing
       ) where

import           Data.Tagged                               (Tagged (..))

import           Pos.Binary.Types                          ()
import           Pos.Modern.Ssc.GodTossing.LocalData.Types (GtLocalData)
import           Pos.Modern.Ssc.GodTossing.Storage.Types   (GtGlobalState)
import           Pos.Ssc.Class.Helpers                     (SscHelpersClass (..))
import           Pos.Ssc.Class.Types                       (Ssc (..))
import           Pos.Ssc.GodTossing.Error                  (SeedError)
import           Pos.Ssc.GodTossing.Functions              (filterLocalPayload,
                                                            verifyGtPayload)
import           Pos.Ssc.GodTossing.Storage.Types          (GtStorage)
import           Pos.Ssc.GodTossing.Types.Type             (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types            (GtContext, GtParams,
                                                            GtPayload, GtProof, SscBi,
                                                            createGtContext, mkGtProof)


instance SscBi => Ssc SscGodTossing where
    type SscStorage     SscGodTossing = GtStorage
    type SscLocalData   SscGodTossing = GtLocalData
    type SscPayload     SscGodTossing = GtPayload
    type SscGlobalState SscGodTossing = GtGlobalState
    type SscProof       SscGodTossing = GtProof
    type SscSeedError   SscGodTossing = SeedError
    type SscNodeContext SscGodTossing = GtContext
    type SscParams      SscGodTossing = GtParams
--    mkSscProof = Tagged mkGtProof
--    sscFilterPayload = filterLocalPayload
--    sscCreateNodeContext = createGtContext

instance SscBi => SscHelpersClass SscGodTossing where
    sscVerifyPayload = Tagged verifyGtPayload
