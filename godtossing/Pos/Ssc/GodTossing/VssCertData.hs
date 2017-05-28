module Pos.Ssc.GodTossing.VssCertData
       ( VssCertData (..)
       , empty
       , insert
       , lookup
       , lookupExpiryEpoch
       , setLastKnownSlot
       , setLastKnownEoS
       , keys
       , member
       , expiryEpoch
       , expiryEoS
       , fromList

       -- * Functions which delete certificates. Be careful
       , delete
       , difference
       , filter
       ) where

import qualified Data.HashMap.Strict     as HM
import qualified Data.List               as List
import qualified Data.Set                as S
import           Formatting              (build, sformat, (%))
import           Universum               hiding (empty, filter)

import           Pos.Core                (EpochIndex (..), EpochOrSlot (..), SlotId (..),
                                          StakeholderId)
import           Pos.Ssc.GodTossing.Core (VssCertificate (..), VssCertificatesMap,
                                          getCertId)

-- | Wrapper around 'VssCertificate' with TTL.
-- Every 'VssCertificate' has own TTL.
-- Wrapper supports simple 'HashMap' operations.
-- Wrapper holds 'VssCertificatesMap'
-- and 'S.Set' of certificates sorted by expiry epoch.
data VssCertData = VssCertData
    { -- | Last known slot, every element of expirySlotSet > lastKnownEoS
      lastKnownEoS :: !EpochOrSlot
      -- | Not expired certificates
    , certs        :: !VssCertificatesMap
      -- | Slot when certs was inserted.
      --   It is needed for deletion from 'insSlotSet' (by 'StakeholderId').
    , whenInsMap   :: !(HashMap StakeholderId EpochOrSlot)
      -- | Set of pairs (insertion slot, address hash)
      -- Every element of insSlotSet <= lastKnownEoS
    , whenInsSet   :: !(Set (EpochOrSlot, StakeholderId))
      -- | Set of pairs (expiry slot, address hash).
      --   Expiry slot is first slot when certificate expires.
      --   Pairs are sorted by expiry slot
      --   (in increasing order, so the oldest certificate is first element).
    , whenExpire   :: !(Set (EpochOrSlot, StakeholderId))
      -- | Set of expired certs for current 'lastKnownEoS'.
      --   We store only certificates which expried no earlier than
      --   in previous epoch.
      -- Set (full expired slot, (id, insertion slot, cert))
    , expiredCerts :: !(Set (EpochOrSlot, (StakeholderId, EpochOrSlot, VssCertificate)))
    } deriving (Show, Eq)

-- | Create empty 'VssCertData'.
empty :: VssCertData
empty = VssCertData (EpochOrSlot $ Left $ EpochIndex 0) mempty mempty mempty mempty mempty

-- | Remove old certificate corresponding to the specified 'StakeholderId'
-- and insert new certificate.
insert :: VssCertificate -> VssCertData -> VssCertData
insert (first getCertId . join (,) -> (id, cert)) mp@VssCertData{..}
    | expiryEoS cert <= lastKnownEoS = mp
    | otherwise                      = addInt id cert mp

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
delete id mp@VssCertData{..}
    | Just (ins, expiry) <- lookupEoSes id mp = VssCertData
          lastKnownEoS
          (HM.delete id certs)
          (HM.delete id whenInsMap)
          (S.delete (ins, id) whenInsSet)
          (S.delete (expiry, id) whenExpire)
          expiredCerts
    | otherwise = mp

-- | Set last known slot (lks).
--   1. If new lks is bigger than 'lastKnownEoS' then some expired certificates
--      will be removed.
--   2. If new lks is less than 'lastKnownEoS' then some inserted after @nlks@
--      certificates will be removed (and 'whenExpire') also will be updated.
setLastKnownEoS :: EpochOrSlot -> VssCertData -> VssCertData
setLastKnownEoS nlks mp@VssCertData{..}
    | nlks > lastKnownEoS = setBiggerLKS nlks mp
    | nlks < lastKnownEoS = setSmallerLKS nlks mp
    | otherwise           = mp

setLastKnownSlot :: SlotId -> VssCertData -> VssCertData
setLastKnownSlot = setLastKnownEoS . EpochOrSlot . Right

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

fromList :: [VssCertificate] -> VssCertData
fromList = foldr' insert empty

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Helper for insert.
-- Expiry epoch will be converted to expiry slot.
addInt :: StakeholderId -> VssCertificate -> VssCertData -> VssCertData
addInt id cert vcd =
    insertRaw $ expireById False id (addEpoch $ lastKnownEoS vcd) vcd
  where
    insertRaw VssCertData{..} = VssCertData
        lastKnownEoS
        (HM.insert id cert certs)
        (HM.insert id lastKnownEoS whenInsMap)
        (S.insert (lastKnownEoS, id) whenInsSet)
        (S.insert (expiryEoS cert, id) whenExpire)
        expiredCerts

-- | Expire certificate with specified id and EoS when it should be
-- removed from expiredCerts.  If given id isn't found in
-- 'VssCertData', behavior depends on 'contains' flag.  If it's true,
-- this function 'panic's, otherwise it returns passed 'VssCertData'.
expireById :: Bool -> StakeholderId -> EpochOrSlot -> VssCertData -> VssCertData
expireById contains id wExp vcd@VssCertData{..}
    | Just (ins, expiry) <- lookupEoSes id vcd
    , Just cert <- HM.lookup id certs = VssCertData
        lastKnownEoS
        (HM.delete id certs)
        (HM.delete id whenInsMap)
        (S.delete (ins, id) whenInsSet)
        (S.delete (expiry, id) whenExpire)
        (S.insert (wExp, (id, ins, cert)) expiredCerts)
     | contains =
        error $ sformat ("Not found cert with id = "%build%" but expected") id
     | otherwise = vcd

-- | Remove elements from beginning of the set @expirySlot@
-- until first element more than lks, update lastKnownEoS also.
setBiggerLKS :: EpochOrSlot -> VssCertData -> VssCertData
setBiggerLKS lks vcd@VssCertData{..}
    | Just ((sl, id), _) <- S.minView whenExpire
    , sl <= lks = setBiggerLKS lks $ expireById True id (addEpoch sl) vcd
    | Just ((sl, _), restExp) <- S.minView expiredCerts
    , sl <= lks = setBiggerLKS lks $ vcd { expiredCerts = restExp }
    | otherwise = vcd { lastKnownEoS = lks }

-- | Update 'lastKnownEoS'.
setSmallerLKS :: EpochOrSlot -> VssCertData -> VssCertData
setSmallerLKS lks vcd@VssCertData{..}
    | Just ((sl, id), rest) <- S.maxView whenInsSet
    , sl > lks = setSmallerLKS lks $ VssCertData
          lastKnownEoS
          (HM.delete id certs)
          (HM.delete id whenInsMap)
          rest
          (S.delete
             (fromMaybe (error "No such id in VCD")
                        (expiryEoS <$> HM.lookup id certs), id)
             whenExpire)
          expiredCerts
    | Just ((sl, (id, insSlot, cert)), restExp) <- S.maxView expiredCerts
    , sl > addEpoch lks = setSmallerLKS lks $ VssCertData
          lastKnownEoS
          (HM.insert id cert certs)
          (HM.insert id insSlot whenInsMap)
          (S.insert (insSlot, id) whenInsSet)
          (S.insert (expiryEoS cert, id) whenExpire)
          restExp
    | otherwise = vcd { lastKnownEoS = lks }

addEpoch :: EpochOrSlot -> EpochOrSlot
addEpoch (EpochOrSlot (Left (EpochIndex epoch))) =
    EpochOrSlot $ Left $ EpochIndex $ epoch + 1
addEpoch (EpochOrSlot (Right (SlotId ep sl))) =
    EpochOrSlot $ Right $ SlotId (ep + 1) sl

-- | Convert expiry epoch of certificate to 'FlatSlotId'.
expiryEpoch :: VssCertificate -> EpochIndex
expiryEpoch cert = vcExpiryEpoch cert + EpochIndex 1

expiryEoS :: VssCertificate -> EpochOrSlot
expiryEoS = EpochOrSlot . Left . expiryEpoch

lookupEoSes :: StakeholderId -> VssCertData -> Maybe (EpochOrSlot, EpochOrSlot)
lookupEoSes id VssCertData{..} =
    (,) <$> HM.lookup id whenInsMap
        <*> (expiryEoS <$> HM.lookup id certs)
