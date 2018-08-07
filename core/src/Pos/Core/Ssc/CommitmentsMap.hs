module Pos.Core.Ssc.CommitmentsMap
       ( CommitmentsMap (getCommitmentsMap)
       , mkCommitmentsMap
       , mkCommitmentsMapUnsafe
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Serokell.Util (allDistinct)

import           Pos.Binary.Class (Bi (..), Decoder, Encoding)
import           Pos.Core.Common (StakeholderId, addressHash)
import           Pos.Core.Ssc.Commitment (SignedCommitment)
import           Pos.Crypto (PublicKey)
import           Pos.Util.Util (cborError)

-- | 'CommitmentsMap' is a wrapper for 'HashMap StakeholderId SignedCommitment'
-- which ensures that keys are consistent with values, i. e. 'PublicKey'
-- from 'SignedCommitment' corresponds to key which is 'StakeholderId'.
newtype CommitmentsMap = CommitmentsMap
    { getCommitmentsMap :: HashMap StakeholderId SignedCommitment
    } deriving (Generic, Semigroup, Monoid, Show, Eq, NFData, Container)

-- | Safe constructor of 'CommitmentsMap'.
mkCommitmentsMap :: [SignedCommitment] -> CommitmentsMap
mkCommitmentsMap = CommitmentsMap . HM.fromList . map toCommPair
  where
    toCommPair signedComm@(pk, _, _) = (addressHash pk, signedComm)

-- | Unsafe straightforward constructor of 'CommitmentsMap'.
mkCommitmentsMapUnsafe :: HashMap StakeholderId SignedCommitment
                       -> CommitmentsMap
mkCommitmentsMapUnsafe = CommitmentsMap

instance Bi CommitmentsMap where
    encode = encodeCommitments
    decode = decodeCommitments


{-
'CommitmentsMap' is simply sets of values, indexed
by stakeholder id *for performance only*; the invariant is that the key
(stakeholder id) corresponds to the key stored in the value. This means that
the keys are redundant and putting them into encoded data is bad for two
reasons:

  * it takes more space
  * we have to do an extra invariant check after decoding

Instead, we serialize those maps as sets, and we make sure to check that
there are no values with duplicate stakeholder ids.
-}

encodeCommitments :: CommitmentsMap -> Encoding
encodeCommitments = encode . HS.fromList . toList

decodeCommitments :: Decoder s CommitmentsMap
decodeCommitments = do
    comms <- toList <$> decode @(HashSet SignedCommitment)
    unless (allDistinct (map (view _1) comms :: [PublicKey])) $ cborError $
        "decodeCommitments: two commitments have the same signing key"
    pure (mkCommitmentsMap comms)

deriveSafeCopySimple 0 'base ''CommitmentsMap
