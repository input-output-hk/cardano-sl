-- | Specification of Pos.Ssc.GodTossing.Types.Base

module Test.Pos.Ssc.GodTossing.Toss.BaseSpec
       ( spec
       ) where

import qualified Data.HashMap.Strict   as HM

import           Pos.Binary            ()
import           Pos.Crypto            (PublicKey, SecretKey, SignTag (SignCommitment),
                                        sign, toPublic)
import           Pos.Lrc.Types         (RichmenStake)
import           Pos.Ssc.GodTossing    (BadCommAndOpening (..), BadCommitment (..),
                                        BadSignedCommitment (..), Commitment,
                                        CommitmentSignature,
                                        CommitmentsMap (..), CommitmentOpening (..),
                                        GtGlobalState (..), MultiRichmenStake, Opening,
                                        OpeningsMap, PureToss, SharesMap,
                                        SignedCommitment, TossVerFailure (..),
                                        VssCertData (certs),
                                        VssCertificate (vcSigningKey), VssCertificatesMap,
                                        checkCertificatesPayload, checkOpeningsPayload,
                                        gsCommitments, gsOpenings,
                                        gsVssCertificates, mkCommitmentsMapUnsafe,
                                        runPureToss, verifyCommitment,
                                        verifyCommitmentSignature, verifyOpening)
import           Pos.Types             (Coin, EpochIndex, StakeholderId, addressHash)

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (..), Gen, NonEmptyList (..), Property,
                                        elements, listOf, sublistOf, suchThat, (==>))
import           Universum

spec :: Spec
spec = describe "Ssc.GodTossing.Base" $ do
    describe "verifyCommitment" $ do
        prop description_verifiesOkComm verifiesOkComm
        prop description_notVerifiesBadComm notVerifiesBadComm
    describe "verifyCommitmentSignature" $ do
        prop description_verifiesOkCommSig verifiesOkCommSig
        prop description_notVerifiesBadSig notVerifiesBadCommSig
    describe "verifyOpening" $ do
        prop description_verifiesOkOpening verifiesOkOpening
        prop description_notVerifiesBadOpening notVerifiesBadOpening
--    describe "checkCommitmentsPayload" $ do
--        prop description_checksGoodCommsPayload checksGoodCommsPayload
    describe "checkOpeningsPayload" $ do
        prop description_checksGoodOpensPayload checksGoodOpeningsPayload
        prop description_checksBadOpeningsPayload checksBadOpeningsPayload
    describe "checkCertificatesPayload" $ do
        prop description_checksGoodCertsPayload checksGoodCertsPayload
        prop description_checksBadCertsPayload checksBadCertsPayload
  where
    description_verifiesOkComm =
        "successfully verifies a correct commitment commitment"
    description_notVerifiesBadComm =
        "unsuccessfully verifies an incorrect commitment"
    description_verifiesOkCommSig =
        "successfully verifies a signed commitment for a given epoch and secret key"
    description_notVerifiesBadSig =
        "unsuccessfully verifies a commitment signature and a mismatching epoch"
    description_verifiesOkOpening =
        "successfully verifies that an opening corresponding to the given commitment\
\ does indeed belong to it"
    description_notVerifiesBadOpening =
        "unsuccessfully verifies a mismatching commitment and opening"
--    description_checksGoodCommsPayload =
--        "successfully checks payload of commitments with a proper epoch index"
    description_checksGoodOpensPayload =
        "successfully checks payload of openings when the global state is correct"
    description_checksBadOpeningsPayload =
        "unsuccessfully checks payload of openings with the right exception for each\
        \ failure case"
    description_checksGoodCertsPayload =
        "successfully checks valid payload of VSS certificates"
    description_checksBadCertsPayload =
        "unsuccessfully checks payload of certificates with the right exception for each\
        \ failure case"

verifiesOkComm :: CommitmentOpening -> Bool
verifiesOkComm CommitmentOpening{..} =
    verifyCommitment coCommitment

notVerifiesBadComm :: BadCommitment -> Bool
notVerifiesBadComm (getBadComm -> badComm) =
    not . verifyCommitment $ badComm

verifiesOkCommSig :: SecretKey -> Commitment -> EpochIndex -> Bool
verifiesOkCommSig sk comm epoch =
    let commSig = (toPublic sk, comm, sign SignCommitment sk (epoch, comm))
    in verifyCommitmentSignature epoch commSig

notVerifiesBadCommSig :: BadSignedCommitment -> EpochIndex -> Bool
notVerifiesBadCommSig (getBadSignedC -> badSignedComm) epoch =
    not $ verifyCommitmentSignature epoch badSignedComm

verifiesOkOpening :: CommitmentOpening -> Bool
verifiesOkOpening CommitmentOpening{..} =
    verifyOpening coCommitment coOpening

notVerifiesBadOpening :: BadCommAndOpening -> Bool
notVerifiesBadOpening (getBadCAndO -> badCommsAndOp) =
    not . uncurry verifyOpening $ badCommsAndOp

tossRunner :: MultiRichmenStake -> GtGlobalState -> ExceptT e PureToss a -> Either e a
tossRunner mrs gtgs = view _1 . runPureToss mrs gtgs . runExceptT

{-checksGoodCommsPayload
    :: EpochIndex
    -> MultiRichmenStake
    -> GtGlobalState
    -> CommitmentsMap
    -> Bool
checksGoodCommsPayload epoch mrs gtgs =
    isRight . tossRunner mrs gtgs . checkCommitmentsPayload epoch-}

newtype GoodOpeningPayload = GoodOpens
    { getGoodOpens :: (GtGlobalState, OpeningsMap)
    } deriving (Show, Eq)

instance Arbitrary GoodOpeningPayload where
    arbitrary = GoodOpens <$> do
        _gsShares <- arbitrary :: Gen SharesMap
        _gsVssCertificates <- arbitrary :: Gen VssCertData
        commsAndOpens <- arbitrary
            :: Gen [(StakeholderId, (PublicKey, CommitmentOpening, CommitmentSignature))]
        let fun (s, (pk, p, cs)) = (s, ((pk, coCommitment p, cs), coOpening p))
            stakeHsAndCOs
                :: [(StakeholderId,
                    ((PublicKey, Commitment, CommitmentSignature), Opening))]
            stakeHsAndCOs = map fun commsAndOpens
            _gsCommitments =
                mkCommitmentsMapUnsafe .
                HM.fromList $ fmap (over _2 $ view _1)  stakeHsAndCOs
        openingPldList <- sublistOf stakeHsAndCOs
        _gsOpenings <- HM.fromList <$> (listOf $
            (,) <$> arbitrary `suchThat` (not . flip elem (map fst openingPldList))
                <*> (arbitrary :: Gen Opening))
        let opensPayload = HM.fromList $ fmap (over _2 $ view _2)  openingPldList
        return (GtGlobalState {..}, opensPayload)

checksGoodOpeningsPayload :: MultiRichmenStake -> GoodOpeningPayload -> Bool
checksGoodOpeningsPayload mrs (getGoodOpens -> (gtgs, openPayload)) =
    isRight . tossRunner mrs gtgs $ checkOpeningsPayload openPayload

checksBadOpeningsPayload
    :: StakeholderId
    -> Opening
    -> SignedCommitment
    -> MultiRichmenStake
    -> GoodOpeningPayload
    -> Property
checksBadOpeningsPayload
    sid
    op
    sig@(_, comm, _)
    mrs
    (getGoodOpens -> (gtgs@GtGlobalState {..}, openPayload)) =
    let newOpenPayload = HM.insert sid op openPayload

        openingAlreadySent =
            tossRunner mrs (gtgs & gsOpenings %~ HM.insert sid op) $
            checkOpeningsPayload newOpenPayload
        res1 = case openingAlreadySent of
            Left (OpeningAlreadySent _) -> True
            _ -> False

        openingWithoutComm =
            tossRunner mrs gtgs $ checkOpeningsPayload newOpenPayload
        res2 = case openingWithoutComm of
            Left (OpeningWithoutCommitment _) -> True
            _ -> False

        alterCommitsMap = mkCommitmentsMapUnsafe . HM.insert sid sig . getCommitmentsMap
        payloadWithBadOpening = HM.insert sid op openPayload
        openingNotMatchComm =
            tossRunner mrs (gtgs & gsCommitments %~ alterCommitsMap) $
            checkOpeningsPayload payloadWithBadOpening
        res3 = case openingNotMatchComm of
            Left (OpeningNotMatchCommitment _) -> True
            _ -> False

    in (not (HM.member sid _gsOpenings) &&
        not (verifyOpening comm op)) ==> res1 && res2 && res3

-- This newtype is going to be used in the 'checkSharesPayload' test, but that's
-- work in progress.
{-newtype GoodSharesPayload = GoodSharesPayload
    { getGoodShares :: (EpochIndex, GtGlobalState, SharesMap, MultiRichmenStake)
    } deriving (Show, Eq)

instance Arbitrary GoodSharesPayload where
    arbitrary = do-}

newtype GoodCertsPayload = GoodCertsPayload
    { getGoodCerts :: (EpochIndex, GtGlobalState, VssCertificatesMap, MultiRichmenStake)
    } deriving (Show, Eq)

instance Arbitrary GoodCertsPayload where
    arbitrary = GoodCertsPayload <$> do

        -- These fields of 'GtGlobalState' are irrelevant for the
        -- 'checkCertificatesPayload' function.
        _gsShares <- arbitrary
        _gsOpenings <- arbitrary
        _gsCommitments <- arbitrary

        -- We'll need an 'EpochIndex' to run 'checkCertificatesPayload'. This epoch index
        -- will also need an accompanying 'RichmenStake', but because we'll need the
        -- public keys to generate valid 'VssCertificates' w.r.t.
        -- 'checkCertificatesPayload', a list with public keys in tuples in generated
        -- as an intermediate step.
        (epoch, NonEmpty richKeys, m) <-
            arbitrary
                :: Gen (EpochIndex, NonEmptyList (PublicKey, Coin), MultiRichmenStake)
        let richmenPks :: [PublicKey]
            richmenPks = map fst richKeys
            richmen :: RichmenStake
            richmen = HM.fromList $ map (over _1 addressHash) richKeys
            -- The 'epoch' epoch is guaranteed to exist in 'richmen', so we can use '(!)'
            -- to search for it in later tests.
            mrs :: MultiRichmenStake
            mrs = HM.insert epoch richmen m

        -- This is the list of participants in the 'GodTossing' protocol that will
        -- have a certificate in the 'VssCertificatesMap' which'll be returned.
        participants <- sublistOf $ HM.keys richmen

        -- This 'VssCertificatesMap' satisfies:
        --   * Every 'vcSigningKey' in all its certificates has a corresponding
        --    'StakeholderId' in 'mrs HM.! epoch'
        --   * The set of its 'StakeholderId' keys is a subset of all the
        --    'StakeholderId's in 'mrs'
        certsMap <- do
            let vssGen :: Gen VssCertificate
                vssGen = do
                    -- This list is guaranteed to be non-empty
                    richman <- elements richmenPks
                    vssCert <- arbitrary
                    return $ vssCert {vcSigningKey = richman}
                gen :: StakeholderId -> Gen (StakeholderId, VssCertificate)
                gen sid = (,) <$> pure sid <*> vssGen
            participantsMap <- HM.fromList <$> mapM gen participants
            fillerMap <- HM.fromList <$> (listOf $ (,) <$> arbitrary <*> vssGen)
            return $ HM.union participantsMap fillerMap

        -- The 'VssCertificatesMap' field of this 'VssCertData' value satisfies:
        --   * None of its 'StakeholderId' keys is a richman
        --    (i.e. is a  member of 'richmen')
        _gsVssCertificates <- do
            certs <- HM.fromList <$>
                 (listOf $ (,) <$> (arbitrary `suchThat` (not . flip HM.member richmen))
                               <*> arbitrary)
            vssData <- arbitrary
            return $ vssData {certs = certs}

        return (epoch, GtGlobalState {..}, certsMap, mrs)

checksGoodCertsPayload :: GoodCertsPayload -> Bool
checksGoodCertsPayload (getGoodCerts -> (epoch, gtgs, certsMap, mrs)) =
    isRight . tossRunner mrs gtgs $ checkCertificatesPayload epoch certsMap

checksBadCertsPayload :: GoodCertsPayload -> StakeholderId -> VssCertificate -> Property
checksBadCertsPayload (getGoodCerts -> (epoch, gtgs, certsMap, mrs)) sid cert =
    let mrsWithMissingEpoch = HM.delete epoch mrs
        noRichmen =
            tossRunner mrsWithMissingEpoch  gtgs $ checkCertificatesPayload epoch certsMap
        res1 = case noRichmen of
            Left (NoRichmen _) -> True
            _ -> False

        insCert = HM.insert sid cert
        newCertsMap = insCert certsMap
        newGtgs = gtgs & gsVssCertificates %~ (\vcd -> let crt = insCert $ certs vcd
                                                       in vcd { certs = crt })
        certAlreadySent =
            tossRunner mrs newGtgs  $ checkCertificatesPayload epoch newCertsMap
        res2 = case certAlreadySent of
            Left (CertificateAlreadySent _) -> True
            _ -> False

        sid' = addressHash . vcSigningKey $ cert
        newerCertsMap = HM.insert sid' cert certsMap
        certNoRichmen = tossRunner mrs gtgs $ checkCertificatesPayload epoch newerCertsMap
        res3 = case certNoRichmen of
            Left (CertificateNotRichmen _) -> True
            _ -> False

    in not (HM.member sid' $ mrs HM.! epoch) ==> res1 && res2 && res3
