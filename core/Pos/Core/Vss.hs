-- | Vss related types and constructors for VssCertificate and VssCertificatesMap

module Pos.Core.Vss
       ( -- * Vss certificates
         VssCertificate (vcVssKey, vcExpiryEpoch, vcSignature, vcSigningKey)
       , mkVssCertificate
       , recreateVssCertificate
       , getCertId

       , VssCertificatesMap
       , mkVssCertificatesMap
       ) where

import           Universum

import           Data.Hashable       (Hashable (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Buildable as Buildable
import           Formatting          (bprint, build, int, (%))

import           Pos.Binary.Class    (AsBinary (..))
import           Pos.Core.Address    (addressHash)
import           Pos.Core.Types      (EpochIndex, StakeholderId)
import           Pos.Crypto          (PublicKey, SecretKey, SignTag (SignVssCert),
                                      Signature, VssPublicKey, checkSig, sign, toPublic)

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

instance Buildable VssCertificate where
    build VssCertificate {..} = bprint
        ("vssCert:"%build%":"%int) vcSigningKey vcExpiryEpoch

instance Hashable VssCertificate where
    hashWithSalt s VssCertificate{..} =
        hashWithSalt s (vcExpiryEpoch, vcVssKey, vcSigningKey, vcSignature)

-- | VssCertificatesMap contains all valid certificates collected
-- during some period of time.
type VssCertificatesMap = HashMap StakeholderId VssCertificate

-- | Make VssCertificate valid up to given epoch using 'SecretKey' to sign
-- data.
mkVssCertificate :: SecretKey -> AsBinary VssPublicKey -> EpochIndex -> VssCertificate
mkVssCertificate sk vk expiry =
    VssCertificate vk expiry signature (toPublic sk)
  where
    signature = sign SignVssCert sk (vk, expiry)

-- | Recreate 'VssCertificate' from its contents. This function main
-- 'fail' if data is invalid.
recreateVssCertificate
    :: MonadFail m
    => AsBinary VssPublicKey
    -> EpochIndex
    -> Signature (AsBinary VssPublicKey, EpochIndex)
    -> PublicKey
    -> m VssCertificate
recreateVssCertificate vssKey epoch sig pk =
    res <$
    (unless (checkCertSign res) $ fail "recreateVssCertificate: invalid sign")
  where
    res =
        VssCertificate
        { vcVssKey = vssKey
        , vcExpiryEpoch = epoch
        , vcSignature = sig
        , vcSigningKey = pk
        }

-- CHECK: @checkCertSign
-- | Check that the VSS certificate is signed properly
-- #checkPubKeyAddress
-- #checkSig
checkCertSign :: VssCertificate -> Bool
checkCertSign VssCertificate {..} =
    checkSig SignVssCert vcSigningKey (vcVssKey, vcExpiryEpoch) vcSignature

getCertId :: VssCertificate -> StakeholderId
getCertId = addressHash . vcSigningKey

-- | Safe constructor of 'VssCertificatesMap'. TODO: wrap into newtype.
mkVssCertificatesMap :: [VssCertificate] -> VssCertificatesMap
mkVssCertificatesMap = HM.fromList . map toCertPair
  where
    toCertPair vc = (getCertId vc, vc)
