-- | Vss related types and constructors for VssCertificate and VssCertificatesMap

module Pos.Core.Vss.Types
       ( -- * Vss certificates
         VssCertificate (..)
       , VssCertificatesMap
       ) where

import           Universum

import           Data.Hashable       (Hashable (..))
import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, build, int, (%))

import           Pos.Binary.Class    (AsBinary (..), Bi)
import           Pos.Core.Types      (EpochIndex, StakeholderId)
import           Pos.Crypto.Signing.Types (PublicKey, Signature)
import           Pos.Crypto.SecretSharing (VssPublicKey)

----------------------------------------------------------------------------
-- Vss certificates
----------------------------------------------------------------------------

-- | VssCertificate allows VssPublicKey to participate in MPC. Each
-- stakeholder should create a Vss keypair, sign VSS public key with signing
-- key and send it into blockchain.
--
-- A public key of node is included in certificate in order to enable
-- validation of it using only node's P2PKH address. Expiry epoch is last
-- epoch when certificate is valid, expiry epoch is included in certificate
-- and signature.
--
-- Other nodes accept this certificate if it is valid and if node has enough
-- stake.
--
-- Invariant: 'checkSig vcSigningKey (vcVssKey, vcExpiryEpoch) vcSignature'.
data VssCertificate = VssCertificate
    { vcVssKey      :: !(AsBinary VssPublicKey)
    , vcExpiryEpoch :: !EpochIndex
    -- ^ Epoch up to which certificates is valid.
    , vcSignature   :: !(Signature (AsBinary VssPublicKey, EpochIndex))
    , vcSigningKey  :: !PublicKey
    } deriving (Show, Eq, Generic)

instance NFData VssCertificate

instance Ord VssCertificate where
    compare a b = toTuple a `compare` toTuple b
      where
        toTuple VssCertificate {..} =
            (vcExpiryEpoch, vcVssKey, vcSigningKey, vcSignature)

instance Bi PublicKey => Buildable VssCertificate where
    build VssCertificate {..} = bprint
        ("vssCert:"%build%":"%int) vcSigningKey vcExpiryEpoch

instance Hashable VssCertificate where
    hashWithSalt s VssCertificate{..} =
        hashWithSalt s (vcExpiryEpoch, vcVssKey, vcSigningKey, vcSignature)

-- | VssCertificatesMap contains all valid certificates collected
-- during some period of time.
type VssCertificatesMap = HashMap StakeholderId VssCertificate
