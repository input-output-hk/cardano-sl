-- | Tests for 'VssCertData': certificates with TTL.

module Test.Pos.Ssc.GodTossing.VssCertDataSpec
       ( spec
       ) where

import           Universum             hiding (empty, filter)

import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet          as HS
import qualified Data.Set              as S
import           Data.Tuple            (swap)

import           Pos.Core.Constants    (slotSecurityParam)
import           Pos.Core.Slotting     (flattenEpochOrSlot, unflattenSlotId)
import           Pos.Ssc.GodTossing    (GtGlobalState (..), MultiRichmenStake,
                                        VssCertData (..), VssCertificate (..), delete,
                                        empty, expiryEoS, filter, getCertId,
                                        gsVssCertificates, insert, keys, lookup, member,
                                        mkVssCertificate, rollbackGT, runPureToss,
                                        setLastKnownSlot)
import           Pos.Types             (EpochIndex (..), EpochOrSlot (..), SlotId,
                                        SlotId (..))
import           Pos.Util.Chrono       (NewestFirst (..))

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (..), Gen, Property, choose, conjoin,
                                        suchThat, vectorOf, (==>))

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
    describe "verifyRollback" $
        prop description_verifyRollback verifyRollback
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
        "successfully verifies if filter saves consistency"
    description_verifyRollback =
        "successfully rollsback certificates older than slot to which state was rolled\
        \ back to"

----------------------------------------------------------------------------
-- Utility functions not present in VssCertData
----------------------------------------------------------------------------

expiresAfter :: VssCertificate -> EpochOrSlot -> Bool
expiresAfter certificate expirySlot = expiryEoS certificate > expirySlot

canBeIn :: VssCertificate -> VssCertData -> Bool
canBeIn certificate certData = certificate `expiresAfter` lastKnownEoS certData

----------------------------------------------------------------------------
-- Wrapper around VssCertData which Arbitrary instance should be consistent
----------------------------------------------------------------------------

newtype CorrectVssCertData = CorrectVssCertData
    { getVssCertData :: VssCertData
    } deriving (Show)

instance Arbitrary CorrectVssCertData where
    arbitrary = (CorrectVssCertData <$>) $ do
        n <- choose (0, 100)
        certificatesToAdd <- choose (0, n)
        lkeos             <- arbitrary :: Gen EpochOrSlot
        let notExpiredGen  = arbitrary `suchThat` (`expiresAfter` lkeos)
        vssCertificates   <- vectorOf @VssCertificate certificatesToAdd notExpiredGen
        let dataUpdaters   = map insert vssCertificates
        pure $ foldl' (&) (empty {lastKnownEoS = lkeos}) dataUpdaters
----------------------------------------------------------------------------
-- Properties for VssCertData
----------------------------------------------------------------------------

verifyInsertVssCertData :: VssCertificate -> VssCertData -> Property
verifyInsertVssCertData certificate certData =
    certificate `canBeIn` certData ==> member shid (insert certificate certData)
  where
    shid = getCertId certificate

verifyDeleteVssCertData :: VssCertificate -> VssCertData -> Bool
verifyDeleteVssCertData certificate certData =
    let shid = getCertId certificate
        certWithShid    = insert certificate certData
        certWithoutShid = delete shid certWithShid
    in not $ member shid certWithoutShid

-- | This function checks all imaginable properties for correctly created 'VssCertdata'.
-- TODO: some checks are not assimptotically efficient but nobody cares untill time is reasonable
isConsistent :: CorrectVssCertData -> Bool
isConsistent (getVssCertData -> VssCertData{..}) =
       -- (1) all certificates inserted not later than lastknownslot
       all (<= lastKnownEoS) insertedSlots
       -- (2) all expiredslots greater than lastKnownSlot
    && all (>  lastKnownEoS) expiredSlots
       -- (3) @certs@ keys and @certsIns@ keys are equal
    && certsStakeholders == certsInsStakeholders
       -- (4) @insSlotset@ equals to hashmap of @certsInts@
    && slotsFromCertsIns == whenInsSet
       -- (5) there is expiry slot for every inserted certificate
    && insSlotSetStakeholders == expirySlotSetStakeholders
       -- (*) every expire slot strictly greater than corresponding inserted slot
       -- consequence of (1) && (2) && (5)
       -- && all (\(expireSlot, shid) -> certsIns ! shid < expireSlot) expirySlotPairs
       -- (6) intersection of expired certificates and not expired is empty
    && null (notExpiredCertificates `S.intersection` expiredCertificates)
       -- (7) all expired certificates are stored for no longer than +epochSlots from lks
    && all (<= addEpoch lastKnownEoS) expiredCertificatesSlots
  where
    insSlotSetPairs           = S.toList whenInsSet
    expirySlotSetPairs        = S.toList whenExpire
    insertedSlots             = map fst insSlotSetPairs
    expiredSlots              = map fst expirySlotSetPairs
    certsStakeholders         = S.fromList $ HM.keys certs
    certsInsStakeholders      = S.fromList $ HM.keys whenInsMap
    slotsFromCertsIns         = S.fromList $ map swap $ HM.toList whenInsMap
    insSlotSetStakeholders    = S.fromList $ map snd insSlotSetPairs
    expirySlotSetStakeholders = S.fromList $ map snd expirySlotSetPairs
    notExpiredCertificates    = S.fromList $ HM.elems certs
    expiredCertificatesData   = S.toList expiredCerts
    expiredCertificatesSlots  = map fst expiredCertificatesData
    expiredCertificates       = S.fromList $ map (view _3 . snd) expiredCertificatesData

    addEpoch :: EpochOrSlot -> EpochOrSlot
    addEpoch (EpochOrSlot (Left (EpochIndex epoch))) =
        EpochOrSlot $ Left $ EpochIndex $ epoch + 1
    addEpoch (EpochOrSlot (Right (SlotId ep sl))) =
        EpochOrSlot $ Right $ SlotId (ep + 1) sl

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

data RollbackData = Rollback MultiRichmenStake GtGlobalState EpochOrSlot [VssCertificate]
    deriving (Show, Eq)

instance Arbitrary RollbackData where
    arbitrary = do
        goodVssCertData@(VssCertData {..}) <- getVssCertData <$> arbitrary
        certsToRollbackN <- choose (0, 100) >>= choose . (0,)
        slotsToRollback <- choose (1, slotSecurityParam :: Word64)
        let lastKEoSWord = flattenEpochOrSlot lastKnownEoS
            rollbackFrom = slotsToRollback + lastKEoSWord
            rollbackGen = do
                sk <- arbitrary
                binVssPK <- arbitrary
                thisEpoch  <-
                    siEpoch . unflattenSlotId <$>
                        choose (succ lastKEoSWord, rollbackFrom)
                return $ mkVssCertificate sk binVssPK thisEpoch
        certsToRollback <- vectorOf @VssCertificate certsToRollbackN rollbackGen
        return $ Rollback mempty
                          (GtGlobalState mempty mempty mempty goodVssCertData)
                          lastKnownEoS
                          certsToRollback

verifyRollback
    :: RollbackData -> Property
verifyRollback (Rollback mrs oldGtGlobalState rollbackEoS vssCerts) =
    let certAdder vcd = foldl' (flip insert) vcd vssCerts
        newGtGlobalState@(GtGlobalState _ _ _ newVssCertData) =
            oldGtGlobalState & gsVssCertificates %~ certAdder
        (_, GtGlobalState _ _ _ rolledVssCertData, _) =
            runPureToss mrs newGtGlobalState $ rollbackGT rollbackEoS (NewestFirst [])
    in conjoin $ fmap (\cert ->
                          isJust (lookup (getCertId cert) newVssCertData) &&
                          (/= Just cert)
                              (lookup (getCertId cert) rolledVssCertData))
                 vssCerts
