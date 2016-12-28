{-# LANGUAGE ViewPatterns #-}

module Pos.Ssc.GodTossing.VssMap
       (
         VssMap (..)
       , insert
       , delete
       , lookup
       , lookupSlot
       , lookupFull
       , setLastKnownSlot
       , keys
       , member
       , unionUsingSlot
       , createFromTTL
       ) where

import           Control.Lens                  ((^.), _1, _2)
import qualified Data.HashMap.Strict           as HM
import qualified Data.Set                      as S
import           Universum

import           Pos.Crypto                    (PublicKey)
import           Pos.Ssc.GodTossing.Types.Base (VssCertificate, VssCertificatesMap)
import           Pos.Types                     (AddressHash, FlatSlotId, SlotId,
                                                flattenSlotId, unflattenSlotId)

type AhPk = (AddressHash PublicKey)

type SlotsMap = HashMap AhPk FlatSlotId

data VssMap = VssMap
    {
      ttl           :: !FlatSlotId
    , lastKnownSlot :: !FlatSlotId
    , certs         :: !VssCertificatesMap
    , slots         :: !SlotsMap
    , bySlot        :: !(S.Set (FlatSlotId, AhPk))
    } deriving (Show)

createFromTTL :: FlatSlotId -> VssMap
createFromTTL ttl = VssMap ttl 0 mempty mempty mempty

insert :: AhPk -> (VssCertificate, SlotId) -> VssMap -> VssMap
insert ahpk val@(_, flattenSlotId -> slot) mp@VssMap{..} =
    if (slot > lastKnownSlot) then
        -- if vss's slot > lastKnownSlot then update lastKnownSlot and add vss
        let newMP = setBiggerLKS slot mp in
            addInt ahpk val newMP
    else if (slot + ttl > lastKnownSlot) then
        -- if vss's slot belongs [lastKnownSlot - ttl + 1; lastKnownSlot] then add vss
        addInt ahpk val mp
    else
        -- if vss's slot < lastKnownSlot - ttl + 1 then ignore it
        mp

lookup :: AhPk -> VssMap -> Maybe VssCertificate
lookup ahpk mp = (^. _1) <$> lookupFull ahpk mp

lookupSlot :: AhPk -> VssMap -> Maybe SlotId
lookupSlot ahpk mp = (^. _2) <$> lookupFull ahpk mp

lookupFull :: AhPk -> VssMap -> Maybe (VssCertificate, SlotId)
lookupFull ahpk VssMap{..} = (,) <$> (HM.lookup ahpk certs)
                                 <*> (unflattenSlotId <$> HM.lookup ahpk slots)

delete :: AhPk -> VssMap -> VssMap
delete ahpk mp@VssMap{..} =
    case lookupSlot ahpk mp of
        Nothing                      -> mp
        Just (flattenSlotId -> slot) ->
            VssMap ttl
                   lastKnownSlot
                   (HM.delete ahpk certs)
                   (HM.delete ahpk slots)
                   (S.delete (slot, ahpk) bySlot)

setLastKnownSlot :: SlotId -> VssMap -> VssMap
setLastKnownSlot (flattenSlotId -> nlks) mp@VssMap{..}
    | nlks > lastKnownSlot = setBiggerLKS nlks mp
    | otherwise            = setSmallerLKS nlks mp

keys :: VssMap -> [AhPk]
keys VssMap{..} = HM.keys certs

member :: AhPk -> VssMap -> Bool
member ahpk VssMap{..} = HM.member ahpk certs

unionUsingSlot :: SlotId -> VssCertificatesMap -> VssMap -> VssMap
unionUsingSlot slotId vssMap (setLastKnownSlot slotId -> newMP) =
    foldl' (\res (h, vss) -> addInt h (vss, slotId) res) newMP (HM.toList vssMap)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------
addInt :: AhPk -> (VssCertificate, SlotId) -> VssMap -> VssMap
addInt ahpk (cert, flattenSlotId -> slot) VssMap{..} =
    VssMap ttl
           lastKnownSlot
           (HM.insert ahpk cert certs)
           (HM.insert ahpk slot slots)
           (S.insert (slot, ahpk) bySlot)

setBiggerLKS :: FlatSlotId -> VssMap -> VssMap
setBiggerLKS lks VssMap{..}
    | lastKnownSlot + ttl <= lks = VssMap ttl lks mempty mempty mempty -- for optimisation only
    | Just ((sl, h), rest) <- S.minView bySlot
    , sl + ttl <= lks = setBiggerLKS lks $
        VssMap ttl
               lastKnownSlot
               (HM.delete h certs)
               (HM.delete h slots)
               rest
    | otherwise = VssMap ttl lks certs slots bySlot

setSmallerLKS :: FlatSlotId -> VssMap -> VssMap
setSmallerLKS lks VssMap{..}
    | lks + ttl <= lastKnownSlot = VssMap ttl lks mempty mempty mempty
    | Just ((sl, h), rest) <- S.maxView bySlot
    , sl > lks = setSmallerLKS lks $
        VssMap ttl
               lastKnownSlot
               (HM.delete h certs)
               (HM.delete h slots)
               rest
    | otherwise = VssMap ttl lks certs slots bySlot
