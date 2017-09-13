-- | Vss related types and constructors for VssCertificate and VssCertificatesMap

module Pos.Core.Vss
       ( mkVssCertificate
       , recreateVssCertificate
       , checkCertSign
       , getCertId
       , mkVssCertificatesMap

       , module Pos.Core.Vss.Types
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Binary.Class    (AsBinary (..), Bi)
import           Pos.Core.Configuration.Protocol (HasProtocolConstants)
import           Pos.Core.Address    (addressHash)
import           Pos.Core.Types      (EpochIndex, StakeholderId)
import           Pos.Crypto          (PublicKey, SecretKey, SignTag (SignVssCert),
                                      Signature, VssPublicKey, checkSig, sign, toPublic)
import           Pos.Core.Vss.Types

-- | Make VssCertificate valid up to given epoch using 'SecretKey' to sign
-- data.
mkVssCertificate
    :: (HasProtocolConstants, Bi EpochIndex)
    => SecretKey
    -> AsBinary VssPublicKey
    -> EpochIndex
    -> VssCertificate
mkVssCertificate sk vk expiry =
    VssCertificate vk expiry signature (toPublic sk)
  where
    signature = sign SignVssCert sk (vk, expiry)

-- | Recreate 'VssCertificate' from its contents. This function main
-- 'fail' if data is invalid.
recreateVssCertificate
    :: (HasProtocolConstants, Bi EpochIndex, MonadFail m)
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
checkCertSign :: (HasProtocolConstants, Bi EpochIndex) => VssCertificate -> Bool
checkCertSign VssCertificate {..} =
    checkSig SignVssCert vcSigningKey (vcVssKey, vcExpiryEpoch) vcSignature

getCertId :: VssCertificate -> StakeholderId
getCertId = addressHash . vcSigningKey

-- | Safe constructor of 'VssCertificatesMap'. TODO: wrap into newtype.
mkVssCertificatesMap :: [VssCertificate] -> VssCertificatesMap
mkVssCertificatesMap = HM.fromList . map toCertPair
  where
    toCertPair vc = (getCertId vc, vc)
