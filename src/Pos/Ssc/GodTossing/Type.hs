{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Ssc.GodTossing.Type
       ( SscGodTossing
         -- * Instances
         -- ** instance Ssc SscGodTossing
       ) where

import           Data.Tagged                        (Tagged (..))
import           Universum

import           Pos.Binary.Ssc                     ()
import           Pos.Binary.Types                   ()
import           Pos.Ssc.Class.Helpers              (SscHelpersClass (..))
import           Pos.Ssc.Class.Types                (Ssc (..))
import           Pos.Ssc.GodTossing.Error           (SeedError)
import           Pos.Ssc.GodTossing.Functions       (verifyGtPayload)
import           Pos.Ssc.GodTossing.LocalData.Types (GtLocalData)
import           Pos.Ssc.GodTossing.Types.Types     (GtContext, GtGlobalState, GtParams,
                                                     GtPayload, GtProof, TossVerFailure,
                                                     createGtContext, mkGtProof)

-- | Data type which represents shared seed calculation tag
-- in -XTypeApplication hacks with type families.
data SscGodTossing
    deriving (Generic)

deriving instance Show SscGodTossing
deriving instance Eq SscGodTossing

instance Ssc SscGodTossing where
    type SscLocalData   SscGodTossing = GtLocalData
    type SscPayload     SscGodTossing = GtPayload
    type SscGlobalState SscGodTossing = GtGlobalState
    type SscProof       SscGodTossing = GtProof
    type SscSeedError   SscGodTossing = SeedError
    type SscNodeContext SscGodTossing = GtContext
    type SscParams      SscGodTossing = GtParams
    type SscVerifyError SscGodTossing = TossVerFailure
    mkSscProof = Tagged mkGtProof
    sscCreateNodeContext = createGtContext

instance SscHelpersClass SscGodTossing where
    sscVerifyPayload = Tagged verifyGtPayload
