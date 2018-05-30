module Pos.Core.Ssc.VssCertificatesMap
       ( VssCertificatesMap (..)

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

import           Universum

import           Control.Lens (makeWrapped)
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.List.Extra (nubOrdOn)
import           Formatting (build, sformat, (%))
import           Serokell.Util (allDistinct)

import           Pos.Binary.Class (Bi)
import           Pos.Core.Common (StakeholderId)
import           Pos.Core.Slotting (EpochIndex)
import           Pos.Crypto (ProtocolMagic)

import           Pos.Core.Ssc.VssCertificate (VssCertificate (..), checkVssCertificate, getCertId,
                                              toCertPair)

-- | VssCertificatesMap contains all valid certificates collected
-- during some period of time.
--
-- Invariants:
--   * stakeholder ids correspond to 'vcSigningKey's of associated certs
--   * no two certs have the same 'vcVssKey'
newtype VssCertificatesMap = UnsafeVssCertificatesMap
    { getVssCertificatesMap :: HashMap StakeholderId VssCertificate }
    deriving (Eq, Show, Generic, NFData, ToList, Container)

type instance Element VssCertificatesMap = VssCertificate

makeWrapped ''VssCertificatesMap

-- | A left-biased instance
instance Semigroup VssCertificatesMap where
    (UnsafeVssCertificatesMap a) <> (UnsafeVssCertificatesMap b) =
        UnsafeVssCertificatesMap $
        a <> HM.filter (not . (`HS.member` lVssKeys) . vcVssKey) b
      where
        lVssKeys = HS.fromList (map vcVssKey (toList a))

instance Monoid VssCertificatesMap where
    mempty = UnsafeVssCertificatesMap mempty
    mappend = (<>)

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
