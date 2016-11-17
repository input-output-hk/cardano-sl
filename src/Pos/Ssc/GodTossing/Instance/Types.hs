{-# LANGUAGE TypeFamilies #-}

-- | Instance of SscTypes.

module Pos.Ssc.GodTossing.Instance.Types
       ( -- * Instances
         -- ** instance SscTypes SscGodTossing
       ) where

import           Data.Tagged                        (Tagged (Tagged))

import           Pos.Crypto                         (PublicKey)
import           Pos.Ssc.Class.Types                (SscTypes (..))
import           Pos.Ssc.GodTossing.Base          (Opening, SignedCommitment)
import           Pos.Ssc.GodTossing.Error         (SeedError)
import           Pos.Ssc.GodTossing.Instance.Type (SscGodTossing)
import           Pos.Ssc.GodTossing.Storage       (DSStorage)
import           Pos.Ssc.GodTossing.Types         (DSMessage, DSPayload, DSProof,
                                                     mkDSProof)

instance SscTypes SscGodTossing where
    type SscStorage   SscGodTossing = DSStorage
    type SscPayload   SscGodTossing = DSPayload
    type SscProof     SscGodTossing = DSProof
    type SscMessage   SscGodTossing = DSMessage
    type SscSeedError SscGodTossing = SeedError
    type SscToken     SscGodTossing = (PublicKey, SignedCommitment, Opening)

    mkSscProof = Tagged mkDSProof
