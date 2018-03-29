-- | VSS related functions.

module Pos.Core.Ssc.Vss
       (
         -- * Types
         VssCertificate (..)
       , VssCertificatesMap (..)
       -- * Certificates
       , mkVssCertificate
       , checkVssCertificate
       , checkCertSign
       , getCertId

       -- * Certificate maps
       -- ** Creating maps
       , checkVssCertificatesMap
       , mkVssCertificatesMap
       , mkVssCertificatesMapLossy
       , mkVssCertificatesMapSingleton
       -- ** Working with maps
       , validateVssCertificatesMap
       , memberVss
       , lookupVss
       , insertVss
       , deleteVss
       ) where

import           Universum hiding (id)

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import           Data.List.Extra (nubOrdOn)
import           Formatting (build, sformat, (%))
import           Serokell.Util (allDistinct)

import           Pos.Binary.Class (AsBinary (..), Bi)
import           Pos.Core.Common (StakeholderId, addressHash)
import           Pos.Core.Slotting.Types (EpochIndex)
import           Pos.Core.Ssc.Types (VssCertificate (..), VssCertificatesMap (..))
import           Pos.Crypto (ProtocolMagic, SecretKey, SignTag (SignVssCert),
                             VssPublicKey, checkSig, sign, toPublic)

-- | Make VssCertificate valid up to given epoch using 'SecretKey' to sign
-- data.
mkVssCertificate
    :: (Bi EpochIndex)
    => ProtocolMagic
    -> SecretKey
    -> AsBinary VssPublicKey
    -> EpochIndex
    -> VssCertificate
mkVssCertificate pm sk vk expiry =
    UnsafeVssCertificate vk expiry signature (toPublic sk)
  where
    signature = sign pm SignVssCert sk (vk, expiry)

-- | Check a 'VssCertificate' for validity.
checkVssCertificate
    :: (Bi EpochIndex, MonadError Text m)
    => ProtocolMagic
    -> VssCertificate
    -> m ()
checkVssCertificate pm it =
    unless (checkCertSign pm it) $ throwError "checkVssCertificate: invalid sign"

-- CHECK: @checkCertSign
-- | Check that the VSS certificate is signed properly
-- #checkPubKeyAddress
-- #checkSig
checkCertSign :: (Bi EpochIndex) => ProtocolMagic -> VssCertificate -> Bool
checkCertSign pm UnsafeVssCertificate {..} =
    checkSig pm SignVssCert vcSigningKey (vcVssKey, vcExpiryEpoch) vcSignature

getCertId :: VssCertificate -> StakeholderId
getCertId = addressHash . vcSigningKey

-- Unexported but useful in the three functions below
toCertPair :: VssCertificate -> (StakeholderId, VssCertificate)
toCertPair vc = (getCertId vc, vc)

-- | Construct a 'VssCertificatesMap' from a list of certs by making a
-- hashmap on certificate identifiers.
mkVssCertificatesMap :: [VssCertificate] -> VssCertificatesMap
mkVssCertificatesMap = UnsafeVssCertificatesMap . HM.fromList . map toCertPair

-- | Guard against certificates with duplicate signing keys or with duplicate
-- 'vcVssKey's. Also checks every VssCertificate in the map (see
-- 'checkVssCertificate').
checkVssCertificatesMap
    :: (Bi EpochIndex, MonadError Text m)
    => ProtocolMagic
    -> VssCertificatesMap
    -> m ()
checkVssCertificatesMap pm vssCertsMap = do
    forM_ certs (checkVssCertificate pm)
    unless (allDistinct (map vcSigningKey certs))
        (throwError "VssCertificatesMap: two certs have the same signing key")
    unless (allDistinct (map vcVssKey certs))
        (throwError "VssCertificatesMap: two certs have the same VSS key")
  where
    certs = HM.elems (getVssCertificatesMap vssCertsMap)

-- | A convenient constructor of 'VssCertificatesMap' that throws away
-- certificates with duplicate signing keys or with duplicate 'vcVssKey's.
mkVssCertificatesMapLossy :: [VssCertificate] -> VssCertificatesMap
mkVssCertificatesMapLossy =
    UnsafeVssCertificatesMap . HM.fromList .
    map toCertPair . nubOrdOn vcVssKey

-- | A map with a single certificate is always valid so this function is
-- safe to use in case you have one certificate and want to create a map
-- from it.
mkVssCertificatesMapSingleton :: VssCertificate -> VssCertificatesMap
mkVssCertificatesMapSingleton =
    UnsafeVssCertificatesMap . uncurry HM.singleton . toCertPair

-- | Return given 'VssCertificatesMap' if it's valid or an error if
-- it's not.
validateVssCertificatesMap ::
       MonadError Text m
    => VssCertificatesMap
    -> m VssCertificatesMap
validateVssCertificatesMap (UnsafeVssCertificatesMap certs) = do
    forM_ (HM.toList certs) $ \(k, v) ->
        when (getCertId v /= k) $
            throwError $ sformat
                ("wrong issuerPk set as key for delegation map: "%
                 "issuer id = "%build%", cert id = "%build)
                k (getCertId v)
    unless (allDistinct (map vcVssKey (toList certs))) $
        throwError "two certs have the same VSS key"
    pure (UnsafeVssCertificatesMap certs)

memberVss :: StakeholderId -> VssCertificatesMap -> Bool
memberVss id (UnsafeVssCertificatesMap m) = HM.member id m

lookupVss :: StakeholderId -> VssCertificatesMap -> Maybe VssCertificate
lookupVss id (UnsafeVssCertificatesMap m) = HM.lookup id m

-- | Insert a certificate into the map.
--
-- In order to preserve invariants, this function removes certificates with
-- our certificate's signing key / VSS key, if they exist. It also returns a
-- list of deleted certificates' keys.
insertVss :: VssCertificate
          -> VssCertificatesMap
          -> (VssCertificatesMap, [StakeholderId])
insertVss c (UnsafeVssCertificatesMap m) =
    ( UnsafeVssCertificatesMap $
      HM.insert (getCertId c) c $
      HM.filter (not . willBeDeleted) m
    , deleted
    )
  where
    willBeDeleted c2 = vcVssKey     c2 == vcVssKey     c
                    || vcSigningKey c2 == vcSigningKey c
    deleted = HM.keys $ HM.filter willBeDeleted m

deleteVss :: StakeholderId -> VssCertificatesMap -> VssCertificatesMap
deleteVss id (UnsafeVssCertificatesMap m) = UnsafeVssCertificatesMap (HM.delete id m)
