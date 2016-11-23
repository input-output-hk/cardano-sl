{-# LANGUAGE TypeFamilies #-}
module Pos.Ssc.GodTossing.Types.Instance
       (
       ) where

import           Data.Tagged                      (Tagged (..))
import           Pos.Ssc.Class.Types              (Ssc (..))
import           Pos.Ssc.GodTossing.Error         (SeedError)
import           Pos.Ssc.GodTossing.Storage.Types (GtStorage)
import           Pos.Ssc.GodTossing.Types.Type    (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types   (GtMessage, GtPayload, GtProof,
                                                   mkGtProof)

instance Ssc SscGodTossing where
    type SscStorage   SscGodTossing = GtStorage
    type SscPayload   SscGodTossing = GtPayload
    type SscProof     SscGodTossing = GtProof
    type SscMessage   SscGodTossing = GtMessage
    type SscSeedError SscGodTossing = SeedError
    mkSscProof = Tagged mkGtProof
