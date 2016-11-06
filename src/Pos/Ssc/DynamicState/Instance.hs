{-# LANGUAGE TypeFamilies #-}

-- | Instance of SscTypes.

module Pos.Ssc.DynamicState.Instance
       ( SscDynamicState
       ) where

import           Data.Tagged                (Tagged (..))

import           Pos.Crypto                 (PublicKey)
import           Pos.Ssc.Class.Types        (SscTypes (..))
import           Pos.Ssc.DynamicState.Base  (Opening, SignedCommitment)
import           Pos.Ssc.DynamicState.Error (SeedError)
import           Pos.Ssc.DynamicState.Types (DSMessage, DSPayload, DSProof, DSStorage,
                                             mkDSProof)


data SscDynamicState

instance SscTypes SscDynamicState where
    type SscStorage   SscDynamicState = DSStorage
    type SscPayload   SscDynamicState = DSPayload
    type SscProof     SscDynamicState = DSProof
    type SscMessage   SscDynamicState = DSMessage
    type SscSeedError SscDynamicState = SeedError
    type SscToken     SscDynamicState = (PublicKey, SignedCommitment, Opening)

    mkSscProof = Tagged mkDSProof
