{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.GodTossing.Type
       ( SscGodTossing
         -- * Instances
         -- ** instance Ssc SscGodTossing
       ) where

import           Data.Tagged                        (Tagged (..))
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

-- | Data type which represents shared seed calculation tag
-- in -XTypeApplication hacks with type families.
data SscGodTossing
    deriving (Generic)

deriving instance Show SscGodTossing
deriving instance Eq SscGodTossing

instance (HasConfiguration, ssc ~ SscGodTossing) => Ssc ssc where
    type SscLocalData   ssc = GtLocalData
    type SscPayload     ssc = GtPayload
    type SscGlobalState ssc = GtGlobalState
    type SscProof       ssc = GtProof
    type SscSeedError   ssc = SeedError
    type SscNodeContext ssc = GtContext
    type SscParams      ssc = GtParams
    type SscVerifyError ssc = TossVerFailure
    mkSscProof = Tagged mkGtProof
    sscCreateNodeContext = Tagged createGtContext

instance (HasConfiguration, ssc ~ SscGodTossing) => SscHelpersClass ssc where
    sscVerifyPayload = sanityChecksGtPayload
    sscStripPayload = stripGtPayload
    sscDefaultPayload = defaultGtPayload
    sscIsCriticalError =
        \case
            TossInternallError {} -> True
            _ -> False
