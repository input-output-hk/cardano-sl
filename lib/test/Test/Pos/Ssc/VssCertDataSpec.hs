-- | Tests for 'VssCertData': certificates with TTL.

module Test.Pos.Ssc.VssCertDataSpec
       ( spec
       ) where

import           Universum hiding (empty, filter, id, keys)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.List.Extra (nubOrdOn)
import qualified Data.Set as S
import           Data.Tuple (swap)
import           Test.Hspec (Spec, describe, runIO)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), Gen, Property, arbitrary, choose, conjoin,
                                  counterexample, generate, suchThat, vectorOf, (.&&.), (==>))

import           Pos.Arbitrary.Ssc ()
import           Pos.Core (EpochIndex (..), EpochOrSlot (..), HasConfiguration, SlotId (..),
                           VssCertificate (..), getCertId, getVssCertificatesMap, mkVssCertificate,
                           slotSecurityParam)
import           Pos.Core.Chrono (NewestFirst (..))
import           Pos.Core.Slotting (flattenEpochOrSlot, unflattenSlotId)
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))
import           Pos.Ssc (SscGlobalState (..), VssCertData (..), delete, empty, expiryEoS, filter,
                          insert, keys, lookup, member, rollbackSsc, runPureToss, setLastKnownSlot,
                          sgsVssCertificates)

import           Test.Pos.Configuration (withProvidedMagicConfig)
import           Test.Pos.Core.Arbitrary ()
import           Test.Pos.Crypto.Dummy (dummyProtocolMagic)
import           Test.Pos.Util.QuickCheck.Property (qcIsJust)

spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = withProvidedMagicConfig pm $ describe "Ssc.VssCertData" $ do
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

instance HasConfiguration => Arbitrary CorrectVssCertData where
    arbitrary = (CorrectVssCertData <$>) $ do
        certificatesToAdd <- choose (0, 100)
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
    certificate `canBeIn` certData ==>
    counterexample
        ("expected " <> show shid <> " to be in certdata")
        (shid `member` insert certificate certData)
  where
    shid = getCertId certificate

verifyDeleteVssCertData :: VssCertificate -> VssCertData -> Property
verifyDeleteVssCertData certificate certData =
    let shid = getCertId certificate
        certWithShid    = insert certificate certData
        certWithoutShid = delete shid certWithShid
    in  counterexample
            ("expected " <> show shid <> " not to be in certdata")
            (not (shid `member` certWithoutShid))

-- | This function checks all imaginable properties for correctly created 'VssCertData'.
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
    certsStakeholders         = S.fromList $ HM.keys $ getVssCertificatesMap certs
    certsInsStakeholders      = S.fromList $ HM.keys whenInsMap
    slotsFromCertsIns         = S.fromList $ map swap $ HM.toList whenInsMap
    insSlotSetStakeholders    = S.fromList $ map snd insSlotSetPairs
    expirySlotSetStakeholders = S.fromList $ map snd expirySlotSetPairs
    notExpiredCertificates    = S.fromList $ toList certs
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

data RollbackData = Rollback SscGlobalState EpochOrSlot [VssCertificate]
    deriving (Show, Eq)

instance HasConfiguration => Arbitrary RollbackData where
    arbitrary = do
        goodVssCertData@(VssCertData {..}) <- getVssCertData <$> arbitrary
        certsToRollbackN <- choose (0, 100) >>= choose . (0,)
        slotsToRollback <- choose (1, slotSecurityParam)
        let lastKEoSWord = flattenEpochOrSlot lastKnownEoS
            rollbackFrom = fromIntegral slotsToRollback + lastKEoSWord
            rollbackGen = do
                sk <- arbitrary
                binVssPK <- arbitrary
                thisEpoch <-
                    siEpoch . unflattenSlotId <$>
                        choose (succ lastKEoSWord, rollbackFrom)
                return $ mkVssCertificate dummyProtocolMagic sk binVssPK thisEpoch
        certsToRollback <- nubOrdOn vcVssKey <$>
            vectorOf @VssCertificate certsToRollbackN rollbackGen
        return $ Rollback (SscGlobalState mempty mempty mempty goodVssCertData)
                          lastKnownEoS
                          certsToRollback

verifyRollback
    :: HasConfiguration => RollbackData -> Gen Property
verifyRollback (Rollback oldSscGlobalState rollbackEoS vssCerts) = do
    let certAdder vcd = foldl' (flip insert) vcd vssCerts
        newSscGlobalState@(SscGlobalState _ _ _ newVssCertData) =
            oldSscGlobalState & sgsVssCertificates %~ certAdder
    (_, SscGlobalState _ _ _ rolledVssCertData, _) <-
        runPureToss newSscGlobalState $
        rollbackSsc rollbackEoS (NewestFirst [])
    pure $ conjoin $ vssCerts <&> \cert ->
        let id = getCertId cert in
        counterexample ("haven't found cert with id " <>
                        show id <> " in newVssCertData")
            (qcIsJust (lookup id newVssCertData))
        .&&.
        counterexample ("expected a " <> show (Just cert) <>
                        ", got " <> show (lookup id rolledVssCertData) <>
                        " in rolledVssCertData")
            ((/= Just cert) (lookup id rolledVssCertData))
