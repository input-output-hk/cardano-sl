-- | Tests for 'VssCertData': certificates with TTL.

module Test.Pos.Ssc.GodTossing.VssCertDataSpec
       ( spec
       ) where

import           Universum             hiding (empty, filter)

import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet          as HS
import qualified Data.Set              as S
import           Data.Tuple            (swap)

import           Pos.Constants         (epochSlots)
import           Pos.Ssc.GodTossing    (VssCertData (..), VssCertificate (..), delete,
                                        empty, expiryFlatSlot, filter, insert, keys,
                                        member, setLastKnownSlot)
import           Pos.Types             (FlatSlotId, SlotId, StakeholderId)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (..), Property, choose, sized, suchThat,
                                        vectorOf, (==>))

spec :: Spec
spec = describe "Ssc.GodTossing.VssCertData" $ do
    describe "verifyInsertVssCertData" $
        prop description_verifyInsertVssCertData verifyInsertVssCertData
    describe "verifyDeleteVssCertData" $
        prop description_verifyDeleteVssCertData verifyDeleteVssCertData
    describe "verifyCorrectVssCertDataIsConsistent" $
        prop description_verifyCorrectVssCertDataIsConsistent isConsistent
    describe "verifySetLastKnownSlot" $
        prop description_verifySetLastKnownSlot verifySetLastKnownSlot
    describe "verifyDeleteAndFilter" $
        prop description_verifyDeleteAndFilter verifyDeleteAndFilter
  where
    description_verifyInsertVssCertData =
        "successfully verifies if certificate is in certificate data\
        \ after insertion this certificate in data"
    description_verifyDeleteVssCertData =
        "successfully verifies if certificate is not in certificate data\
        \ after deletion of this certificate from data"
    description_verifyCorrectVssCertDataIsConsistent =
        "successfully verifies if inserts create consistent VssCertData"
    description_verifySetLastKnownSlot =
        "successfully verifies if new last known slot is set properly"
    description_verifyDeleteAndFilter =
        "successfully verifies if filter saves consintency"

----------------------------------------------------------------------------
-- Utility functions not present in VssCertData
----------------------------------------------------------------------------

expiresAfter :: VssCertificate -> FlatSlotId -> Bool
expiresAfter certificate expirySlot = expiryFlatSlot certificate > expirySlot

canBeIn :: VssCertificate -> VssCertData -> Bool
canBeIn certificate certData = certificate `expiresAfter` lastKnownSlot certData

----------------------------------------------------------------------------
-- Wrapper around VssCertData which Arbitrary instance should be consistent
----------------------------------------------------------------------------

newtype CorrectVssCertData = CorrectVssCertData
    { getVssCertData :: VssCertData
    } deriving (Show)

instance Arbitrary CorrectVssCertData where
    arbitrary = (CorrectVssCertData <$>) $ sized $ \n -> do
        certificatesToAdd <- choose (0, n)
        lks               <- choose (0, fromIntegral n)  -- boundaries can be chosen more wisely
        let notExpiredGen  = arbitrary `suchThat` (`expiresAfter` lks)
        vssCertificates   <- vectorOf @VssCertificate certificatesToAdd notExpiredGen
        stakeholders      <- vectorOf @StakeholderId  certificatesToAdd arbitrary
        let dataUpdaters   = zipWith insert stakeholders vssCertificates
        pure $ foldl' (&) empty dataUpdaters

----------------------------------------------------------------------------
-- Properties for VssCertData
----------------------------------------------------------------------------

verifyInsertVssCertData :: StakeholderId -> VssCertificate -> VssCertData -> Property
verifyInsertVssCertData shid certificate certData =
    certificate `canBeIn` certData ==> member shid (insert shid certificate certData)

verifyDeleteVssCertData :: StakeholderId -> VssCertificate -> VssCertData -> Bool
verifyDeleteVssCertData shid certificate certData =
    let certWithShid    = insert shid certificate certData
        certWithoutShid = delete shid certWithShid
    in not $ member shid certWithoutShid

-- | This function checks all imaginable properties for correctly created 'VssCertdata'.
-- TODO: some checks are not assimptotically efficient but nobody cares untill time is reasonable
isConsistent :: CorrectVssCertData -> Bool
isConsistent (getVssCertData -> VssCertData{..}) =
       -- (1) all certificates inserted not later than lastknownslot
       all (<= lastKnownSlot) insertedSlots
       -- (2) all expiredslots greater than lastKnownSlot
    && all (>  lastKnownSlot) expiredSlots
       -- (3) @certs@ keys and @certsIns@ keys are equal
    && certsStakeholders == certsInsStakeholders
       -- (4) @insSlotset@ equals to hashmap of @certsInts@
    && slotsFromCertsIns == insSlotSet
       -- (5) there is expiry slot for every inserted certificate
    && insSlotSetStakeholders == expirySlotSetStakeholders
       -- (*) every expire slot strictly greater than corresponding inserted slot
       -- consequence of (1) && (2) && (5)
       -- && all (\(expireSlot, shid) -> certsIns ! shid < expireSlot) expirySlotPairs
       -- (6) intersection of expired certificates and not expired is empty
    && null (notExpiredCertificates `S.intersection` expiredCertificates)
       -- (7) all expired certificates are stored for no longer than +epochSlots from lks
    && all (<= lastKnownSlot + epochSlots) expiredCertificatesSlots
  where
    insSlotSetPairs           = S.toList insSlotSet
    expirySlotSetPairs        = S.toList expirySlotSet
    insertedSlots             = map fst insSlotSetPairs
    expiredSlots              = map fst expirySlotSetPairs
    certsStakeholders         = S.fromList $ HM.keys certs
    certsInsStakeholders      = S.fromList $ HM.keys certsIns
    slotsFromCertsIns         = S.fromList $ map swap $ HM.toList certsIns
    insSlotSetStakeholders    = S.fromList $ map snd insSlotSetPairs
    expirySlotSetStakeholders = S.fromList $ map snd expirySlotSetPairs
    notExpiredCertificates    = S.fromList $ HM.elems certs
    expiredCertificatesData   = S.toList expiredCerts
    expiredCertificatesSlots  = map fst expiredCertificatesData
    expiredCertificates       = S.fromList $ map (view _3 . snd) expiredCertificatesData

verifySetLastKnownSlot :: SlotId -> CorrectVssCertData -> Bool
verifySetLastKnownSlot newLks (CorrectVssCertData vssCertData) =
    isConsistent $ CorrectVssCertData $ setLastKnownSlot newLks vssCertData

-- | Verifies that filter (and 'delete' as consequences) save consintency.
-- TODO: add more checks here?
verifyDeleteAndFilter :: CorrectVssCertData -> Bool
verifyDeleteAndFilter (getVssCertData -> vcd@VssCertData{..}) =
    let certificatesHolders = keys vcd
        holdersLength       = length certificatesHolders
        halfOfHolders       = take (holdersLength `div` 2) certificatesHolders
        setFromHalf         = HS.fromList halfOfHolders
        resultVcd           = filter (`HS.member` setFromHalf) vcd
        resultCorrectVcd    = CorrectVssCertData resultVcd
    in isConsistent resultCorrectVcd
