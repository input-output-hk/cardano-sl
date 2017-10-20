{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.GodTossing.Instance
       ( -- * Instances
         -- ** instance Ssc
       ) where

import           Universum

import           Pos.Binary.Core                    ()
import           Pos.Binary.GodTossing              ()
import           Pos.Core                           (HasConfiguration)
import           Pos.Ssc.Class.Helpers              (SscHelpersClass (..))
import           Pos.Ssc.Class.Types                (Ssc (..))
import           Pos.Ssc.GodTossing.Core            (GtPayload, GtProof, defaultGtPayload,
                                                     mkGtProof, stripGtPayload)
import           Pos.Ssc.GodTossing.Error           (SeedError)
import           Pos.Ssc.GodTossing.Functions       (sanityChecksGtPayload)
import           Pos.Ssc.GodTossing.LocalData.Types (GtLocalData)
import           Pos.Ssc.GodTossing.Toss.Failure    (TossVerFailure (..))
import           Pos.Ssc.GodTossing.Types.Types     (GtContext, GtGlobalState, GtParams,
                                                     createGtContext)

instance HasConfiguration => Ssc where
    type SscLocalData   = GtLocalData
    type SscPayload     = GtPayload
    type SscGlobalState = GtGlobalState
    type SscProof       = GtProof
    type SscSeedError   = SeedError
    type SscNodeContext = GtContext
    type SscParams      = GtParams
    type SscVerifyError = TossVerFailure
    mkSscProof = mkGtProof
    sscCreateNodeContext = createGtContext

instance HasConfiguration => SscHelpersClass where
    sscVerifyPayload = sanityChecksGtPayload
    sscStripPayload = stripGtPayload
    sscDefaultPayload = defaultGtPayload
    sscIsCriticalError =
        \case
            TossInternallError {} -> True
            _ -> False
