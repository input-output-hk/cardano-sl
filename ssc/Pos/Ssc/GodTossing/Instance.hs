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
import           Pos.Ssc.Core                       (defaultSscPayload,
                                                     stripSscPayload)
import           Pos.Ssc.GodTossing.Functions       (sanityChecksSscPayload)
import           Pos.Ssc.GodTossing.LocalData.Types (GtLocalData)
import           Pos.Ssc.GodTossing.Toss.Failure    (TossVerFailure (..))
import           Pos.Ssc.GodTossing.Types.Types     (GtContext, GtGlobalState, GtParams,
                                                     createGtContext)

instance HasConfiguration => Ssc where
    type SscLocalData   = GtLocalData
    type SscGlobalState = GtGlobalState
    type SscNodeContext = GtContext
    type SscParams      = GtParams
    type SscVerifyError = TossVerFailure
    sscCreateNodeContext = createGtContext

instance HasConfiguration => SscHelpersClass where
    sscVerifyPayload = sanityChecksSscPayload
    sscStripPayload = stripSscPayload
    sscDefaultPayload = defaultSscPayload
    sscIsCriticalError =
        \case
            TossInternallError {} -> True
            _ -> False
