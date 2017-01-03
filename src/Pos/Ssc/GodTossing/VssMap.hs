{-# LANGUAGE ViewPatterns #-}

module Pos.Ssc.GodTossing.VssMap
       ( VssMap (..)
       , empty
       , insert
       , delete
       , lookup
       , lookupEpoch
       , setLastKnownSlot
       , keys
       , member
       ) where

import qualified Data.HashMap.Strict           as HM
import qualified Data.Set                      as S
import           Universum                     hiding (empty)

import           Pos.Constants                 (k)
import           Pos.Crypto                    (PublicKey)
import           Pos.Ssc.GodTossing.Types.Base (VssCertificate (..), VssCertificatesMap)
import           Pos.Types                     (AddressHash, EpochIndex (..), FlatSlotId,
                                                SlotId, flattenSlotId)

type AhPk = (AddressHash PublicKey)

data VssMap = VssMap
    {
      lastKnownSlot :: !FlatSlotId
    , certs         :: !VssCertificatesMap
    , bySlot        :: !(S.Set (FlatSlotId, AhPk))
    } deriving (Show)

empty :: VssMap
empty = VssMap 0 mempty mempty

insert :: AhPk -> VssCertificate -> VssMap -> VssMap
insert ahpk cert mp@VssMap{..} =
    -- if cert is expired then return mp
    if (expirySlot cert <= lastKnownSlot) then mp
    else addInt ahpk cert mp

lookup :: AhPk -> VssMap -> Maybe VssCertificate
lookup ahpk VssMap{..} = HM.lookup ahpk certs

lookupEpoch :: AhPk -> VssMap -> Maybe EpochIndex
lookupEpoch ahpk mp = expiryEpoch <$> lookup ahpk mp

delete :: AhPk -> VssMap -> VssMap
delete ahpk mp@VssMap{..} =
    case lookup ahpk mp of
        Nothing                   -> mp
        Just (expirySlot -> slot) ->
            VssMap lastKnownSlot
                   (HM.delete ahpk certs)
                   (S.delete (slot, ahpk) bySlot)

setLastKnownSlot :: SlotId -> VssMap -> VssMap
setLastKnownSlot (flattenSlotId -> nlks) mp@VssMap{..}
    | nlks > lastKnownSlot = setBiggerLKS nlks mp
    | otherwise            = setSmallerLKS nlks mp

keys :: VssMap -> [AhPk]
keys VssMap{..} = HM.keys certs

member :: AhPk -> VssMap -> Bool
member ahpk VssMap{..} = HM.member ahpk certs

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------
addInt :: AhPk -> VssCertificate -> VssMap -> VssMap
addInt ahpk cert VssMap{..} =
    VssMap lastKnownSlot
           (HM.insert ahpk cert certs)
           (S.insert (expirySlot cert, ahpk) bySlot)

setBiggerLKS :: FlatSlotId -> VssMap -> VssMap
setBiggerLKS lks VssMap{..}
    | Just ((sl, h), rest) <- S.minView bySlot
    , sl <= lks = setBiggerLKS lks $
          VssMap lastKnownSlot
                 (HM.delete h certs)
                 rest
    | otherwise = VssMap lks certs bySlot

setSmallerLKS :: FlatSlotId -> VssMap -> VssMap
setSmallerLKS lks VssMap{..} = VssMap lks certs bySlot

expirySlot :: VssCertificate -> FlatSlotId
expirySlot cert = (1 + getEpochIndex (expiryEpoch cert)) * 6 * k
