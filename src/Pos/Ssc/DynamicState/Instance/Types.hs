{-# LANGUAGE TypeFamilies #-}

-- | Instance of SscTypes.

module Pos.Ssc.DynamicState.Instance.Types
       ( -- * Instances
         -- ** instance SscTypes SscDynamicState
       ) where

import           Data.Tagged                        (Tagged (Tagged))

import           Pos.Crypto                         (PublicKey)
import           Pos.Ssc.Class.Types                (SscTypes (..))
import           Pos.Ssc.DynamicState.Base          (Opening, SignedCommitment)
import           Pos.Ssc.DynamicState.Error         (SeedError)
import           Pos.Ssc.DynamicState.Instance.Type (SscDynamicState)
import           Pos.Ssc.DynamicState.Storage       (DSStorage)
import           Pos.Ssc.DynamicState.Types         (DSMessage, DSPayload, DSProof,
                                                     mkDSProof)

instance SscTypes SscDynamicState where
    type SscStorage   SscDynamicState = DSStorage
    type SscPayload   SscDynamicState = DSPayload
    type SscProof     SscDynamicState = DSProof
    type SscMessage   SscDynamicState = DSMessage
    type SscSeedError SscDynamicState = SeedError
    type SscToken     SscDynamicState = (PublicKey, SignedCommitment, Opening)

    mkSscProof = Tagged mkDSProof
