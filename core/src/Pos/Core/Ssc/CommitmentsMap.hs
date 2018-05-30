module Pos.Core.Ssc.CommitmentsMap
       ( CommitmentsMap (getCommitmentsMap)
       , mkCommitmentsMap
       , mkCommitmentsMapUnsafe
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Core.Common (StakeholderId, addressHash)

import           Pos.Core.Ssc.Commitment (SignedCommitment)

-- | 'CommitmentsMap' is a wrapper for 'HashMap StakeholderId SignedCommitment'
-- which ensures that keys are consistent with values, i. e. 'PublicKey'
-- from 'SignedCommitment' corresponds to key which is 'StakeholderId'.
newtype CommitmentsMap = CommitmentsMap
    { getCommitmentsMap :: HashMap StakeholderId SignedCommitment
    } deriving (Generic, Semigroup, Monoid, Show, Eq, ToList, NFData)

type instance Element CommitmentsMap = SignedCommitment

-- | Safe constructor of 'CommitmentsMap'.
mkCommitmentsMap :: [SignedCommitment] -> CommitmentsMap
mkCommitmentsMap = CommitmentsMap . HM.fromList . map toCommPair
  where
    toCommPair signedComm@(pk, _, _) = (addressHash pk, signedComm)

-- | Unsafe straightforward constructor of 'CommitmentsMap'.
mkCommitmentsMapUnsafe :: HashMap StakeholderId SignedCommitment
                       -> CommitmentsMap
mkCommitmentsMapUnsafe = CommitmentsMap
