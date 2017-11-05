-- | Vss related types and constructors for VssCertificate and VssCertificatesMap

module Pos.Core.Vss
       (
       -- * Reexports
         module Pos.Core.Vss.Types

       -- * Certificates
       , mkVssCertificate
       , recreateVssCertificate
       , checkCertSign
       , getCertId

       -- * Certificate maps
       -- ** Creating maps
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

import           Universum

import           Control.Monad.Except            (MonadError (throwError))
import qualified Data.HashMap.Strict             as HM
import           Data.List.Extra                 (nubOrdOn)
import           Formatting                      (build, sformat, (%))
import           Serokell.Util                   (allDistinct)

import           Pos.Binary.Class                (AsBinary (..), Bi)
import           Pos.Core.Address                (addressHash)
import           Pos.Core.Configuration.Protocol (HasProtocolConstants)
import           Pos.Core.Types                  (EpochIndex, StakeholderId)
import           Pos.Core.Vss.Types
import           Pos.Crypto                      (PublicKey, SecretKey,
                                                  SignTag (SignVssCert), Signature,
                                                  VssPublicKey, checkSig, sign, toPublic)

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

-- | Recreate 'VssCertificate' from its contents. This function can
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

-- Unexported but useful in the three functions below
toCertPair :: VssCertificate -> (StakeholderId, VssCertificate)
toCertPair vc = (getCertId vc, vc)

-- | Safe constructor of 'VssCertificatesMap'. It doesn't allow certificates
-- with duplicate signing keys or with duplicate 'vcVssKey's.
mkVssCertificatesMap
    :: MonadFail m
    => [VssCertificate] -> m VssCertificatesMap
mkVssCertificatesMap certs = do
    unless (allDistinct (map vcSigningKey certs)) $
        fail "mkVssCertificatesMap: two certs have the same signing key"
    unless (allDistinct (map vcVssKey certs)) $
        fail "mkVssCertificatesMap: two certs have the same VSS key"
    pure $ UnsafeVssCertificatesMap (HM.fromList (map toCertPair certs))

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
