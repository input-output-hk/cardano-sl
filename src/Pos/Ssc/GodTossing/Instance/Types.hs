{-# LANGUAGE TypeFamilies #-}

-- | Instance of SscTypes.

module Pos.Ssc.GodTossing.Instance.Types
       ( -- * Instances
         -- ** instance SscTypes SscGodTossing
       ) where

import           Data.Tagged                      (Tagged (Tagged))

import           Pos.Crypto                       (PublicKey)
import           Pos.Ssc.Class.Types              (Ssc (..))
import           Pos.Ssc.GodTossing.Base          (Opening, SignedCommitment)
import           Pos.Ssc.GodTossing.Error         (SeedError)
import           Pos.Ssc.GodTossing.Instance.Type (SscGodTossing)
import           Pos.Ssc.GodTossing.Storage       (GtStorage)
import           Pos.Ssc.GodTossing.Types         (GtMessage, GtPayload, GtProof,
                                                   mkGtProof)

instance Ssc SscGodTossing where
    type SscStorage   SscGodTossing = GtStorage
    type SscPayload   SscGodTossing = GtPayload
    type SscProof     SscGodTossing = GtProof
    type SscMessage   SscGodTossing = GtMessage
    type SscSeedError SscGodTossing = SeedError
    type SscToken     SscGodTossing = (PublicKey, SignedCommitment, Opening)

    mkSscProof = Tagged mkGtProof
