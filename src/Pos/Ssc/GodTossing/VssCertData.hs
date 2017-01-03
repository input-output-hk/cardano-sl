{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module Pos.Ssc.GodTossing.VssCertData
       ( VssCertData (..)
       , empty
       , insert
       , delete
       , lookup
       , lookupExpiryEpoch
       , setLastKnownSlot
       , keys
       , member
       ) where

import qualified Data.HashMap.Strict           as HM
import           Data.SafeCopy                 (base, deriveSafeCopySimple)
import qualified Data.Set                      as S
import           Universum                     hiding (empty)

import           Pos.Constants                 (k)
import           Pos.Ssc.GodTossing.Types.Base (VssCertificate (..), VssCertificatesMap)
import           Pos.Types                     (EpochIndex (..), FlatSlotId, SlotId,
                                                StakeholderId, flattenSlotId)

-- | Wrapper around VSS certificates with ttl.
-- Every VSS certificate has own ttl.
-- Wrapper supports simple map operations.
-- Wrapper holds VssCertificatesMap
-- and set of certificates sorted by expiry epoch.
data VssCertData = VssCertData
    {
      lastKnownSlot :: !FlatSlotId                 -- ^ Last known slot, every element of bySlot > lastKnownSlot
    , certs         :: !VssCertificatesMap         -- ^ Not expired certificates
    , bySlot        :: !(S.Set (FlatSlotId, StakeholderId)) -- ^ Set of pairs (expiry slot, address hash).
                                                   --   Expiry slot is first slot when certificate expires.
                                                   --   Pairs are sorted by expiry slot
                                                   --   (in increase order, so the oldest certificate is first element)
    } deriving (Show, Eq)

deriveSafeCopySimple 0 'base ''VssCertData

-- | Create empty VssCertData
empty :: VssCertData
empty = VssCertData 0 mempty mempty

-- | Remove old certificate corresponding to the specified address hash
-- and insert new certificate.
insert :: StakeholderId -> VssCertificate -> VssCertData -> VssCertData
insert ahpk cert mp@VssCertData{..}
    | expirySlot cert <= lastKnownSlot = mp
    | otherwise                        = addInt ahpk cert mp

-- | Lookup certificate corresponding to the specified address hash.
lookup :: StakeholderId -> VssCertData -> Maybe VssCertificate
lookup ahpk VssCertData{..} = HM.lookup ahpk certs

-- | Lookup expiry epoch of certificate corresponding to the specified address hash.
lookupExpiryEpoch :: StakeholderId -> VssCertData -> Maybe EpochIndex
lookupExpiryEpoch ahpk mp = vcExpiryEpoch <$> lookup ahpk mp

-- | Delete certificate corresponding to the specified address hash.
delete :: StakeholderId -> VssCertData -> VssCertData
delete ahpk mp@VssCertData{..} =
    case lookup ahpk mp of
        Nothing                   -> mp
        Just (expirySlot -> slot) ->
            VssCertData lastKnownSlot
                   (HM.delete ahpk certs)
                   (S.delete (slot, ahpk) bySlot)

-- | Set last known slot (lks). If new lks bigger than lastKnownSlot
-- then some expired certificates will be removed.
setLastKnownSlot :: SlotId -> VssCertData -> VssCertData
setLastKnownSlot (flattenSlotId -> nlks) mp@VssCertData{..}
    | nlks > lastKnownSlot = setBiggerLKS nlks mp
    | otherwise            = setSmallerLKS nlks mp

-- | Address hashes of certificates.
keys :: VssCertData -> [StakeholderId]
keys VssCertData{..} = HM.keys certs

-- | Return True if the specified address hash is present in the map, False otherwise.
member :: StakeholderId -> VssCertData -> Bool
member ahpk VssCertData{..} = HM.member ahpk certs

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------
-- | Helper for insert.
-- Expiry epoch will be converted to expiry slot.
addInt :: StakeholderId -> VssCertificate -> VssCertData -> VssCertData
addInt ahpk cert (delete ahpk -> VssCertData{..}) =
    VssCertData lastKnownSlot
           (HM.insert ahpk cert certs)
           (S.insert (expirySlot cert, ahpk) bySlot)

-- | Remove elements from beginning of the set @bySlot@
-- until first element more than lks, update lastKnownSlot also.
setBiggerLKS :: FlatSlotId -> VssCertData -> VssCertData
setBiggerLKS lks VssCertData{..}
    | Just ((sl, h), rest) <- S.minView bySlot
    , sl <= lks = setBiggerLKS lks $
          VssCertData lastKnownSlot
                 (HM.delete h certs)
                 rest
    | otherwise = VssCertData lks certs bySlot

-- | Update lastKnownSlot
setSmallerLKS :: FlatSlotId -> VssCertData -> VssCertData
setSmallerLKS lks VssCertData{..} = VssCertData lks certs bySlot

-- | Convert expiry epoch of certificate to FlatSlotId
expirySlot :: VssCertificate -> FlatSlotId
expirySlot cert = (1 + getEpochIndex (vcExpiryEpoch cert)) * 6 * k
