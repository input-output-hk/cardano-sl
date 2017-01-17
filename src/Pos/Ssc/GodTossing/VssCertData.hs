{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module Pos.Ssc.GodTossing.VssCertData
       ( VssCertData (..)
       , empty
       , insert
       , lookup
       , lookupExpiryEpoch
       , setLastKnownSlot
       , keys
       , member
       , expiryFlatSlot

       -- * Functions which delete certificates. Be careful
       , delete
       , difference
       , filter
       ) where

import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as List
import           Data.SafeCopy                 (base, deriveSafeCopySimple)
import qualified Data.Set                      as S
import           Universum                     hiding (empty, filter)

import           Pos.Constants                 (epochSlots)
import           Pos.Ssc.GodTossing.Types.Base (VssCertificate (..), VssCertificatesMap)
import           Pos.Types                     (EpochIndex (..), FlatSlotId, SlotId,
                                                StakeholderId, flattenSlotId)

-- | Wrapper around 'VssCertificate' with TTL.
-- Every 'VssCertificate' has own TTL.
-- Wrapper supports simple 'HashMap' operations.
-- Wrapper holds 'VssCertificatesMap'
-- and 'S.Set' of certificates sorted by expiry epoch.
data VssCertData = VssCertData
    { -- | Last known slot, every element of bySlot > lastKnownSlot
      lastKnownSlot :: !FlatSlotId
      -- | Not expired certificates
    , certs         :: !VssCertificatesMap
      -- | Slot when certs was inserted.
      --   It is needed for deletion from 'insSlotSet' (by 'StakeholderId').
    , certsIns      :: !(HashMap StakeholderId FlatSlotId)
      -- | Set of pairs (insertion slot, address hash)
    , insSlotSet    :: !(Set (FlatSlotId, StakeholderId))
      -- | Set of pairs (expiry slot, address hash).
      --   Expiry slot is first slot when certificate expires.
      --   Pairs are sorted by expiry slot
      --   (in increasing order, so the oldest certificate is first element).
    , expirySlotSet :: !(Set (FlatSlotId, StakeholderId))
      -- | Set of expired certs for current 'lastKnownSlot'.
      --   We store only certificates which expried no earlier than
      --   in previous epoch.
    , expiredCerts  :: !(Set (FlatSlotId, (StakeholderId, FlatSlotId, VssCertificate)))
    } deriving (Show, Eq)

deriveSafeCopySimple 0 'base ''VssCertData

-- | Create empty 'VssCertData'.
empty :: VssCertData
empty = VssCertData 0 mempty mempty mempty mempty mempty

-- | Remove old certificate corresponding to the specified 'StakeholderId'
-- and insert new certificate.
insert :: StakeholderId -> VssCertificate -> VssCertData -> VssCertData
insert id cert mp@VssCertData{..}
    | expiryFlatSlot cert <= lastKnownSlot = mp
    | otherwise                            = addInt id cert mp

-- | Lookup certificate corresponding to the specified 'StakeholderId'.
lookup :: StakeholderId -> VssCertData -> Maybe VssCertificate
lookup id VssCertData{..} = HM.lookup id certs

-- | Lookup expiry epoch of certificate corresponding to the specified
-- 'StakeholderId'.
lookupExpiryEpoch :: StakeholderId -> VssCertData -> Maybe EpochIndex
lookupExpiryEpoch id mp = vcExpiryEpoch <$> lookup id mp

-- | Delete certificate corresponding to the specified 'StakeholderId'.
-- This function is dangerous, because after using it you can't rollback
-- deleted certificates. Use carefully.
delete :: StakeholderId -> VssCertData -> VssCertData
delete id mp@VssCertData{..} =
    case lookupAux id mp of
        Nothing         -> mp
        Just (ins, expiry, cert) -> VssCertData
            lastKnownSlot
            (HM.delete id certs)
            (HM.delete id certsIns)
            (S.delete (ins, id) insSlotSet)
            (S.delete (expiry, id) expirySlotSet)
            (S.delete (expiry + epochSlots, (id, ins, cert)) expiredCerts)

-- | Set last known slot (lks).
--   1. If new lks is bigger than 'lastKnownSlot' then some expired certificates
--      will be removed.
--   2. If new lks is less than 'lastKnownSlot' then some inserted after @nlks@
--      certificates will be removed (and 'expirySlotSet') also will be updated.
setLastKnownSlot :: SlotId -> VssCertData -> VssCertData
setLastKnownSlot (flattenSlotId -> nlks) mp@VssCertData{..}
    | nlks > lastKnownSlot = setBiggerLKS nlks mp
    | nlks < lastKnownSlot = setSmallerLKS nlks mp
    | otherwise            = mp

-- | Ids of stakeholders issued certificates.
keys :: VssCertData -> [StakeholderId]
keys VssCertData{..} = HM.keys certs

-- | Filtering the certificates.
-- This function is dangerous, because after you using it you can't rollback
-- deleted certificates. Use carefully.
filter :: (StakeholderId -> Bool) -> VssCertData -> VssCertData
filter predicate vcd =
    foldl' (flip delete) vcd $ List.filter (not . predicate) $ keys vcd

-- | Return True if the specified address hash is present in the map, False otherwise.
member :: StakeholderId -> VssCertData -> Bool
member id VssCertData{..} = HM.member id certs

-- | This function is dangerous, because after you using it you can't rollback
-- deleted certificates. Use carefully.
difference :: VssCertData -> HM.HashMap StakeholderId a -> VssCertData
difference mp hm = foldl' (flip delete) mp . HM.keys $ hm

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Helper for insert.
-- Expiry epoch will be converted to expiry slot.
addInt :: StakeholderId -> VssCertificate -> VssCertData -> VssCertData
addInt id cert (delete id -> VssCertData{..}) = VssCertData
    lastKnownSlot
    (HM.insert id cert certs)
    (HM.insert id lastKnownSlot certsIns)
    (S.insert (lastKnownSlot, id) insSlotSet)
    (S.insert (expiryFlatSlot cert, id) expirySlotSet)
    expiredCerts

-- | Remove elements from beginning of the set @expirySlot@
-- until first element more than lks, update lastKnownSlot also.
setBiggerLKS :: FlatSlotId -> VssCertData -> VssCertData
setBiggerLKS lks vcd@VssCertData{..}
    | Just ((sl, id), rest) <- S.minView expirySlotSet
    , sl <= lks = setBiggerLKS lks $
      let insSlot = HM.lookupDefault (panic "No such id in the certsIns") id certsIns
          cert = HM.lookupDefault (panic "No such id in the certs") id certs in
      VssCertData
          lastKnownSlot
          (HM.delete id certs)
          (HM.delete id certsIns)
          (S.delete (insSlot, id) insSlotSet)
          rest
          (S.insert (sl + epochSlots, (id, insSlot, cert)) expiredCerts)
    | Just ((sl, _), restExp) <- S.minView expiredCerts
    , sl <= lks = setBiggerLKS lks $ vcd { expiredCerts = restExp }
    | otherwise = vcd { lastKnownSlot = lks }

-- | Update 'lastKnownSlot'.
setSmallerLKS :: FlatSlotId -> VssCertData -> VssCertData
setSmallerLKS lks vcd@VssCertData{..}
    | Just ((sl, id), rest) <- S.maxView insSlotSet
    , sl > lks = setSmallerLKS lks $ VssCertData
          lastKnownSlot
          (HM.delete id certs)
          (HM.delete id certsIns)
          rest
          (S.delete
             (fromMaybe (panic "No such id in certs") (expiryFlatSlot <$> HM.lookup id certs), id)
             expirySlotSet)
          expiredCerts
    | Just ((sl, (id, insSlot, cert)), restExp) <- S.maxView expiredCerts
    , sl > lks + epochSlots = setSmallerLKS lks $ VssCertData
          lastKnownSlot
          (HM.insert id cert certs)
          (HM.insert id insSlot certsIns)
          (S.insert (insSlot, id) insSlotSet)
          (S.insert (expiryFlatSlot cert, id) expirySlotSet)
          restExp
    | otherwise = vcd { lastKnownSlot = lks }

-- | Convert expiry epoch of certificate to 'FlatSlotId'.
expiryFlatSlot :: VssCertificate -> FlatSlotId
expiryFlatSlot cert = (1 + getEpochIndex (vcExpiryEpoch cert)) * epochSlots

lookupAux :: StakeholderId -> VssCertData -> Maybe (FlatSlotId, FlatSlotId, VssCertificate)
lookupAux id VssCertData{..} =
    (,,) <$> HM.lookup id certsIns
         <*> (expiryFlatSlot <$> stakeholderCertificate)
         <*> stakeholderCertificate
  where
    stakeholderCertificate = HM.lookup id certs
