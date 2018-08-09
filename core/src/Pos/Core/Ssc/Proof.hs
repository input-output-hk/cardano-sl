module Pos.Core.Ssc.Proof
       ( SscProof (..)
       , mkSscProof
       , VssCertificatesHash
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Fmt (genericF)

import           Pos.Binary.Class (Bi)
import           Pos.Core.Common (StakeholderId)
import           Pos.Crypto (Hash, hash)

import           Pos.Core.Ssc.CommitmentsMap
import           Pos.Core.Ssc.Opening
import           Pos.Core.Ssc.OpeningsMap
import           Pos.Core.Ssc.Payload
import           Pos.Core.Ssc.SharesMap
import           Pos.Core.Ssc.VssCertificate
import           Pos.Core.Ssc.VssCertificatesMap

-- Note: we can't use 'VssCertificatesMap', because we serialize it as
-- a 'HashSet', but in the very first version of mainnet this map was
-- serialized as a 'HashMap' (and 'VssCertificatesMap' was just a type
-- alias for that 'HashMap').
--
-- Alternative approach would be to keep 'instance Bi VssCertificatesMap'
-- the same as it was in mainnet.
type VssCertificatesHash = Hash (HashMap StakeholderId VssCertificate)

-- | Proof that SSC payload is correct (it's included into block header)
data SscProof
    = CommitmentsProof
        { sprComms :: !(Hash CommitmentsMap)
        , sprVss   :: !VssCertificatesHash }
    | OpeningsProof
        { sprOpenings :: !(Hash OpeningsMap)
        , sprVss      :: !VssCertificatesHash }
    | SharesProof
        { sprShares :: !(Hash SharesMap)
        , sprVss    :: !VssCertificatesHash }
    | CertificatesProof
        { sprVss    :: !VssCertificatesHash }
    deriving (Eq, Show, Generic)

instance Buildable SscProof where
    build = genericF

instance NFData SscProof

-- | Create proof (for inclusion into block header) from 'SscPayload'.
mkSscProof
    :: ( Bi CommitmentsMap
       , Bi Opening
       , Bi VssCertificate
       ) => SscPayload -> SscProof
mkSscProof payload =
    case payload of
        CommitmentsPayload comms certs ->
            proof CommitmentsProof comms certs
        OpeningsPayload openings certs ->
            proof OpeningsProof openings certs
        SharesPayload shares certs     ->
            proof SharesProof shares certs
        CertificatesPayload certs      ->
            CertificatesProof (hash $ getVssCertificatesMap certs)
  where
    proof constr hm (getVssCertificatesMap -> certs) =
        constr (hash hm) (hash certs)
