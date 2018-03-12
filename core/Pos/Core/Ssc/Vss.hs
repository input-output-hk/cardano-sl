-- | VSS related functions.

module Pos.Core.Ssc.Vss
       (
       -- * Types
         VssCertificate (..)
       , VssCertificatesMap (..)

       -- * Certificates
       , mkVssCertificate
       , getCertId

       -- * Certificate maps
       -- ** Creating maps
       , mkVssCertificatesMap
       , mkVssCertificatesMapLossy
       , mkVssCertificatesMapSingleton
       -- ** Working with maps
       , memberVss
       , lookupVss
       , insertVss
       , deleteVss
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Data.List.Extra (nubOrdOn)
import           Formatting (build, sformat, (%))
import           Serokell.Util (allDistinct)

import           Pos.Binary.Class (AsBinary (..), Bi)
import           Pos.Core.Common (StakeholderId, addressHash)
import           Pos.Core.Slotting.Types (EpochIndex)
import           Pos.Core.Ssc.Types (VssCertificate (..), VssCertificatesMap (..))
import           Pos.Crypto (HasCryptoConfiguration, SecretKey, SignTag (SignVssCert), VssPublicKey,
                             checkSig, sign, toPublic)
import           Pos.Util.Verification (PVerifiable (..), PVerifiableSub (..), pverFail)

----------------------------------------------------------------------------
-- Certificates
----------------------------------------------------------------------------

-- | Make VssCertificate valid up to given epoch using 'SecretKey' to sign
-- data.
mkVssCertificate
    :: (HasCryptoConfiguration, Bi EpochIndex)
    => SecretKey
    -> AsBinary VssPublicKey
    -> EpochIndex
    -> VssCertificate
mkVssCertificate sk vk expiry =
    UncheckedVssCertificate vk expiry signature (toPublic sk)
  where
    signature = sign SignVssCert sk (vk, expiry)

-- CHECK: @checkCertSign
-- | Check that the VSS certificate is signed properly
-- #checkPubKeyAddress
-- #checkSig
checkCertSign :: (HasCryptoConfiguration, Bi EpochIndex) => VssCertificate -> Bool
checkCertSign UncheckedVssCertificate {..} =
    checkSig SignVssCert vcSigningKey (vcVssKey, vcExpiryEpoch) vcSignature

instance (HasCryptoConfiguration, Bi EpochIndex) => PVerifiable VssCertificate where
    pverifySelf vssCert =
        unless (checkCertSign vssCert) $
        pverFail "VssCertificate: invalid sign"

----------------------------------------------------------------------------
-- Certificate maps creation/validation
----------------------------------------------------------------------------

getCertId :: VssCertificate -> StakeholderId
getCertId = addressHash . vcSigningKey

-- Unexported but useful in the three functions below
toCertPair :: VssCertificate -> (StakeholderId, VssCertificate)
toCertPair vc = (getCertId vc, vc)

-- | Guard against certificates with duplicate signing keys or with
-- duplicate 'vcVssKey's.
instance (HasCryptoConfiguration, Bi EpochIndex) => PVerifiable VssCertificatesMap where
    pverifySelf (UncheckedVssCertificatesMap vm) = do
        let certs = HM.elems vm
        unless (allDistinct (map vcSigningKey certs)) $
            pverFail "VssCertificatesMap: two certs have the same signing key"
        unless (allDistinct (map vcVssKey certs)) $
            pverFail "VssCertificatesMap: two certs have the same VSS key"
        forM_ (HM.toList vm) $ \(k, v) ->
            when (getCertId v /= k) $
                pverFail $ sformat
                    ("wrong issuerPk set as key for delegation map: "%
                     "issuer id = "%build%", cert id = "%build)
                    k (getCertId v)
    pverifyFields (UncheckedVssCertificatesMap vm) =
        map (PVerifiableSub "vssCertMap.elem") $ HM.elems vm

-- | Construct a 'VssCertificatesMap' from a list of certs by making a
-- hashmap on certificate identifiers.
mkVssCertificatesMap :: [VssCertificate] -> VssCertificatesMap
mkVssCertificatesMap = UncheckedVssCertificatesMap . HM.fromList . map toCertPair

-- | A convenient constructor of 'VssCertificatesMap' that throws away
-- certificates with duplicate signing keys or with duplicate 'vcVssKey's.
mkVssCertificatesMapLossy :: [VssCertificate] -> VssCertificatesMap
mkVssCertificatesMapLossy =
    UncheckedVssCertificatesMap . HM.fromList .
    map toCertPair . nubOrdOn vcVssKey

-- | A map with a single certificate is always valid so this function is
-- safe to use in case you have one certificate and want to create a map
-- from it.
mkVssCertificatesMapSingleton :: VssCertificate -> VssCertificatesMap
mkVssCertificatesMapSingleton =
    UncheckedVssCertificatesMap . uncurry HM.singleton . toCertPair

----------------------------------------------------------------------------
-- Operations on maps
----------------------------------------------------------------------------

memberVss :: StakeholderId -> VssCertificatesMap -> Bool
memberVss id (UncheckedVssCertificatesMap m) = HM.member id m

lookupVss :: StakeholderId -> VssCertificatesMap -> Maybe VssCertificate
lookupVss id (UncheckedVssCertificatesMap m) = HM.lookup id m

-- | Insert a certificate into the map.
--
-- In order to preserve invariants, this function removes certificates with
-- our certificate's signing key / VSS key, if they exist. It also returns a
-- list of deleted certificates' keys.
insertVss :: VssCertificate
          -> VssCertificatesMap
          -> (VssCertificatesMap, [StakeholderId])
insertVss c (UncheckedVssCertificatesMap m) =
    ( UncheckedVssCertificatesMap $
      HM.insert (getCertId c) c $
      HM.filter (not . willBeDeleted) m
    , deleted
    )
  where
    willBeDeleted c2 = vcVssKey     c2 == vcVssKey     c
                    || vcSigningKey c2 == vcSigningKey c
    deleted = HM.keys $ HM.filter willBeDeleted m

deleteVss :: StakeholderId -> VssCertificatesMap -> VssCertificatesMap
deleteVss id (UncheckedVssCertificatesMap m) = UncheckedVssCertificatesMap (HM.delete id m)
