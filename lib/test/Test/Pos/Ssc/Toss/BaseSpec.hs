-- | Specification of Pos.Ssc.Toss.Base

module Test.Pos.Ssc.Toss.BaseSpec
       ( spec
       ) where

import           Universum

import           Control.Lens (ix, _Wrapped)
import qualified Crypto.Random as Rand
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.List.Extra (nubOrdOn)
import           System.Random (mkStdGen, randomR)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), Gen, NonEmptyList (..), Property, elements,
                                  listOf, property, sublistOf, suchThat, vector, (.&&.), (===),
                                  (==>))

import           Pos.Arbitrary.Lrc (GenesisMpcThd, ValidRichmenStakes (..))
import           Pos.Arbitrary.Ssc (BadCommAndOpening (..), BadSignedCommitment (..),
                                    CommitmentOpening (..))
import           Pos.Binary (AsBinary)
import           Pos.Core (Coin, EpochIndex, EpochOrSlot (..), HasConfiguration, StakeholderId,
                           VssCertificate (..), VssCertificatesMap (..), addressHash, crucialSlot,
                           genesisBlockVersionData, insertVss, mkCoin, _vcVssKey)
import           Pos.Core.Ssc (Commitment, CommitmentSignature, CommitmentsMap (..), InnerSharesMap,
                               Opening, OpeningsMap, SharesMap, SignedCommitment,
                               mkCommitmentsMapUnsafe)
import           Pos.Crypto (DecShare, PublicKey, SecretKey, SignTag (SignCommitment), sign,
                             toPublic, protocolMagic)
import           Pos.Lrc.Types (RichmenStakes)
import           Pos.Ssc (MultiRichmenStakes, PureTossWithEnv, SscGlobalState (..),
                          SscVerifyError (..), VssCertData (..), checkCertificatesPayload,
                          checkCommitmentsPayload, checkOpeningsPayload, checkSharesPayload,
                          runPureToss, sgsCommitments, sgsOpenings, sgsShares, sgsVssCertificates,
                          supplyPureTossEnv)
import           Pos.Ssc.Base (deleteSignedCommitment, verifyCommitment, verifyCommitmentSignature,
                               verifyOpening)
import           Pos.Util.QuickCheck.Property (qcElem, qcFail, qcIsRight)

import           Test.Pos.Configuration (withDefConfiguration)

spec :: Spec
spec = withDefConfiguration $ describe "Ssc.Base" $ do
    describe "verifyCommitment" $ do
        prop description_verifiesOkComm verifiesOkComm
    describe "verifyCommitmentSignature" $ do
        prop description_verifiesOkCommSig verifiesOkCommSig
        prop description_notVerifiesBadSig notVerifiesBadCommSig
    describe "verifyOpening" $ do
        prop description_verifiesOkOpening verifiesOkOpening
        prop description_notVerifiesBadOpening notVerifiesBadOpening
    describe "checkCommitmentsPayload" $ do
        prop description_emptyPayload emptyPayloadComms
        prop description_checksGoodCommsPayload checksGoodCommsPayload
        prop description_checksBadCommsPayload checksBadCommsPayload
    describe "checkOpeningsPayload" $ do
        prop description_emptyPayload (emptyPayload checkOpeningsPayload)
        prop description_checksGoodOpensPayload checksGoodOpeningsPayload
        prop description_checksBadOpeningsPayload checksBadOpeningsPayload
    describe "checkSharesPayload" $ do
        prop description_emptyPayload
            (\e mrs hm -> emptyPayload (checkSharesPayload e) $ HM.insert e hm mrs)
        prop description_checksGoodSharesPayload checksGoodSharesPayload
        prop description_checksBadSharesPayload checksBadSharesPayload
    describe "checkCertificatesPayload" $ do
        prop description_emptyPayload
            (\e mrs hm -> emptyPayload (checkCertificatesPayload e) $ HM.insert e hm mrs)
        prop description_checksGoodCertsPayload checksGoodCertsPayload
        prop description_checksBadCertsPayload checksBadCertsPayload
  where
    description_verifiesOkComm =
        "successfully verifies a correct commitment commitment"
    description_verifiesOkCommSig =
        "successfully verifies a signed commitment for a given epoch and secret key"
    description_notVerifiesBadSig =
        "unsuccessfully verifies a commitment signature and a mismatching epoch"
    description_verifiesOkOpening =
        "successfully verifies that an opening corresponding to the given commitment\
\ does indeed belong to it"
    description_notVerifiesBadOpening =
        "unsuccessfully verifies a mismatching commitment and opening"
    description_emptyPayload =
        "an empty payload always leads to successful verification"
    description_checksGoodCommsPayload =
        "successfully checks payload of commitments with a proper epoch index"
    description_checksBadCommsPayload =
        "unsuccessfully checks payload of commitments with the right exception for each\
        \ failure case"
    description_checksGoodOpensPayload =
        "successfully checks payload of openings when the global state is correct"
    description_checksBadOpeningsPayload =
        "unsuccessfully checks payload of openings with the right exception for each\
        \ failure case"
    description_checksGoodSharesPayload =
        "successfully checks payload of shares"
    description_checksBadSharesPayload =
        "unsuccessfully checks payload of shares with the right exception for each\
        \ failure case"
    description_checksGoodCertsPayload =
        "successfully checks valid payload of VSS certificates"
    description_checksBadCertsPayload =
        "unsuccessfully checks payload of certificates with the right exception for each\
        \ failure case"

verifiesOkComm :: CommitmentOpening -> Bool
verifiesOkComm CommitmentOpening{..} =
    verifyCommitment coCommitment

verifiesOkCommSig :: HasConfiguration => SecretKey -> Commitment -> EpochIndex -> Bool
verifiesOkCommSig sk comm epoch =
    let commSig = (toPublic sk, comm, sign protocolMagic SignCommitment sk (epoch, comm))
    in verifyCommitmentSignature epoch commSig

notVerifiesBadCommSig :: HasConfiguration => BadSignedCommitment -> EpochIndex -> Bool
notVerifiesBadCommSig (getBadSignedC -> badSignedComm) epoch =
    not $ verifyCommitmentSignature epoch badSignedComm

verifiesOkOpening :: CommitmentOpening -> Bool
verifiesOkOpening CommitmentOpening{..} =
    verifyOpening coCommitment coOpening

notVerifiesBadOpening :: BadCommAndOpening -> Bool
notVerifiesBadOpening (getBadCAndO -> badCommsAndOp) =
    not . uncurry verifyOpening $ badCommsAndOp

emptyPayload
    :: (HasConfiguration, Monoid container, Show e)
    => (container -> ExceptT e PureTossWithEnv a)
    -> MultiRichmenStakes
    -> SscGlobalState
    -> Property
emptyPayload pureToss mrs sgs =
    qcIsRight $ tossRunner mrs sgs $ pureToss mempty

emptyPayloadComms
    :: HasConfiguration
    => GoodCommsPayload
    -> SscGlobalState
    -> Property
    -- The 'checkCommitmentsPayload' function will never pass without a valid
    -- multirichmen hashmap, meaning we can't use entirely arbitrary data.
    -- As such, one from a 'GoodCommsPayload' is fetched instead since the
    -- 'Arbitrary' instance ensures validity.
emptyPayloadComms GoodPayload {..} =
    let e :: EpochIndex
        validMrs :: MultiRichmenStakes
        (e, validMrs) = (gpEpoch, gpMultiRichmenStakes)
    in emptyPayload (checkCommitmentsPayload e) validMrs

data GoodPayload p = GoodPayload
    { gpEpoch              :: !EpochIndex
    , gpGlobalState        :: !SscGlobalState
    , gpPayload            :: p
    , gpMultiRichmenStakes :: !MultiRichmenStakes
    } deriving (Show, Eq)

type GoodCommsPayload = GoodPayload CommitmentsMap

instance HasConfiguration => Arbitrary GoodCommsPayload where
    arbitrary = do
        -- These fields won't be needed for anything, so they can be entirely arbitrary.
        _sgsOpenings <- arbitrary
        _sgsShares <- arbitrary

        -- The epoch used in the tests is generated separately to make sure it exists.
        (gpEpoch, m) <- arbitrary :: Gen (EpochIndex, MultiRichmenStakes)
        richmen <- getValid <$> (arbitrary :: Gen (ValidRichmenStakes GenesisMpcThd))
        let richmenIds = HM.keys richmen
            gpMultiRichmenStakes = HM.insert gpEpoch richmen m

        -- This is the list of richmen which will be used to make keys for commitments
        -- in the argument 'CommitmentsMap'.
        richmenWithCerts <- sublistOf richmenIds
        richmenWithComms <- sublistOf richmenWithCerts

        -- TODO: this generates an invalid VssCertificatesMap because the
        -- stakeholders don't match the certificates
        stableCerts <- UnsafeVssCertificatesMap . HM.fromList <$>
            mapM (\r -> (,) <$> pure r <*> (arbitrary :: Gen VssCertificate))
                 richmenWithCerts

        -- Only the simplest case is considered w.r.t. the 'lastKnownEoS' field:
        -- 'checkCommitmentsPayload' is called with the same epoch as the 'lastKnownEoS'
        -- in 'gsVssCertificate'.
        -- This is because rolling back slots is and should be tested elsewhere.
        _sgsVssCertificates <- VssCertData
            <$> (pure . EpochOrSlot . Right . crucialSlot $ gpEpoch)
            <*> pure stableCerts
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

        gpPayload <- mkCommitmentsMapUnsafe . HM.fromList <$>
                mapM (\k -> (,) <$> pure k <*> arbitrary) richmenWithComms
        _sgsCommitments <- mkCommitmentsMapUnsafe <$> customHashMapGen
            (arbitrary `suchThat` (not . flip elem richmenWithComms))
            arbitrary
        let gpGlobalState = SscGlobalState {..}

        return GoodPayload {..}

-- TODO: Account for 'CommSharesOnWrongParticipants' failure
checksGoodCommsPayload :: HasConfiguration => GoodCommsPayload -> Bool
checksGoodCommsPayload (GoodPayload epoch sgs commsMap mrs) =
    case tossRunner mrs sgs $ checkCommitmentsPayload epoch commsMap of
        Left (CommSharesOnWrongParticipants _) -> True
        Right _                                -> True
        _                                      -> False

-- TODO: Account for 'CommSharesOnWrongParticipants' failure
checksBadCommsPayload
    :: HasConfiguration
    => GoodCommsPayload
    -> StakeholderId
    -> SignedCommitment
    -> Int
    -> Property
checksBadCommsPayload
    (GoodPayload epoch sgs@SscGlobalState {..} commsMap mrs)
    sid
    comm
    seed =
    let mrsWithMissingEpoch = HM.delete epoch mrs
        noRichmen =
            tossRunner mrsWithMissingEpoch sgs $ checkCommitmentsPayload epoch commsMap
        res1 = case noRichmen of
            Left (NoRichmen e) -> e == epoch
            _                  -> False

        wrapCMap f = mkCommitmentsMapUnsafe . f . getCommitmentsMap
        commMember k = HM.member k . getCommitmentsMap

        newCommsMap = wrapCMap (HM.insert sid comm) commsMap
        committingNoParticipants =
            tossRunner mrs sgs $ checkCommitmentsPayload epoch newCommsMap
        res2 = case committingNoParticipants of
            Left (CommittingNoParticipants (s :| [])) -> s == sid
            _                                         -> False

        -- Inserting a random stakeholder would perturb the valid richmen stake we already
        -- have, so an existing one must be picked to avoid failing to calculate the
        -- shares distribution.
        someRichman =
            case res of
                Just s -> s
                _ -> error "Test.Pos.Ssc.Toss.BaseSpec.checkBadCommsPayload:\
                           \ empty commitments payload"
          where holders = HM.keys . getCommitmentsMap $ commsMap
                randomIndex = fst $ randomR (0, length holders - 1) $ mkStdGen seed
                res = holders ^? ix randomIndex

        sgs' = sgs & sgsCommitments %~ wrapCMap (HM.insert someRichman comm)
        commitmentAlreadySent =
            tossRunner mrs sgs' $ checkCommitmentsPayload epoch commsMap
        res3 = case commitmentAlreadySent of
            Left (CommitmentAlreadySent (s :| [])) -> s == someRichman
            _                                      -> False

    in not (commMember sid _sgsCommitments) &&
       not (commMember sid commsMap) &&
       not (null _sgsCommitments) &&
       not (null commsMap) ==> res1  && res2 && res3

newtype GoodOpeningPayload = GoodOpens
    { getGoodOpens :: (SscGlobalState, OpeningsMap)
    } deriving (Show, Eq)

instance HasConfiguration => Arbitrary GoodOpeningPayload where
    arbitrary = GoodOpens <$> do

      -- These fields won't be used, so they can be entirely arbitrary
        _sgsShares <- arbitrary :: Gen SharesMap
        _sgsVssCertificates <- arbitrary :: Gen VssCertData

        -- Because 'Opening's and 'Commitment's in the openings payload map and
        -- '_sgsCommitments' resp. will need to be matched succesfully by
        -- 'checkOpeningsPayload', they must be generated in tandem, along with their
        -- respective keys in both maps and the dummy 'CommitmentSignature' to be used in
        -- the commitments map.
        commsAndOpens <- arbitrary
            :: Gen [(StakeholderId, (PublicKey, CommitmentOpening, CommitmentSignature))]
        let fun (s, (pk, p, cs)) = (s, ((pk, coCommitment p, cs), coOpening p))

            -- The stakeholders in this assoc list are paired with a tuple, the first
            -- component of which will be their value in '_sgsCommitments' and the second
            -- the value in '_sgsOpenings'.
            stakeHoldersAndCOs
                :: [(StakeholderId,
                    ((PublicKey, Commitment, CommitmentSignature), Opening))]
            stakeHoldersAndCOs = map fun commsAndOpens
            _sgsCommitments =
                mkCommitmentsMapUnsafe .
                -- over _2 (view _1) (a,(b,c)) = (a,b)
                HM.fromList $ fmap (over _2 $ view _1) stakeHoldersAndCOs
        openingPldList <- sublistOf stakeHoldersAndCOs

        -- For the test data to be correct, none of the stakeholders with an opening in
        -- openings payload can have an opening in the global state i.e. be a key in
        -- '_sgsOpenings'.
        _sgsOpenings <- customHashMapGen
            (arbitrary `suchThat` (not . flip elem (map fst openingPldList)))
            (arbitrary :: Gen Opening)
        -- over _2 (view _2) (a,(b,c)) = (a,c)
        let opensPayload = HM.fromList $ fmap (over _2 $ view _2) openingPldList

        return (SscGlobalState {..}, opensPayload)

checksGoodOpeningsPayload
    :: HasConfiguration
    => MultiRichmenStakes -> GoodOpeningPayload -> Property
checksGoodOpeningsPayload mrs (getGoodOpens -> (sgs, openPayload)) =
    qcIsRight . tossRunner mrs sgs $ checkOpeningsPayload openPayload

checksBadOpeningsPayload
    :: HasConfiguration
    => StakeholderId
    -> Opening
    -> SignedCommitment
    -> MultiRichmenStakes
    -> GoodOpeningPayload
    -> Property
checksBadOpeningsPayload
    sid
    op
    sig@(_, comm, _)
    mrs
    (getGoodOpens -> (sgs@SscGlobalState {..}, openPayload)) =
    let newOpenPayload = HM.insert sid op openPayload

        openingAlreadySent =
            tossRunner mrs (sgs & sgsOpenings %~ HM.insert sid op) $
            checkOpeningsPayload newOpenPayload
        res1 = case openingAlreadySent of
            Left (OpeningAlreadySent _) -> True
            _                           -> False

        openingWithoutComm =
            tossRunner mrs sgs $ checkOpeningsPayload newOpenPayload
        res2 = case openingWithoutComm of
            Left (OpeningWithoutCommitment _) -> True
            _                                 -> False

        alterCommitsMap = mkCommitmentsMapUnsafe . HM.insert sid sig . getCommitmentsMap
        payloadWithBadOpening = HM.insert sid op openPayload
        openingNotMatchComm =
            tossRunner mrs (sgs & sgsCommitments %~ alterCommitsMap) $
            checkOpeningsPayload payloadWithBadOpening
        res3 = case openingNotMatchComm of
            Left (OpeningNotMatchCommitment _) -> True
            _                                  -> False

    in (not (HM.member sid _sgsOpenings) &&
        not (verifyOpening comm op)) ==> res1 && res2 && res3

type GoodSharesPayload = GoodPayload SharesMap

instance HasConfiguration => Arbitrary GoodSharesPayload where
    arbitrary = do
        -- These openings won't be needed for anything, so they can be entirely arbitrary.
        _sgsOpenings <- arbitrary

        -- The richmen for the epoch used in the tests is generated separately to make
        -- sure it exists.
        (gpEpoch, richmen, m) <-
            arbitrary :: Gen (EpochIndex, RichmenStakes, MultiRichmenStakes)
        let richmenIds = HM.keys richmen
            gpMultiRichmenStakes = HM.insert gpEpoch richmen m
        -- This is the list of richmen which will be used to make keys for the stable
        -- 'VssCertificates' that will be in the 'certs' field of '_sgsVssCertificates'.
        richmenWithCerts <- sublistOf richmenIds
        -- These richmen will be used to make keys for the shares that will be in the
        -- 'SharesMap' which will be used as the second argument of 'checkSharesPayload'
        -- in tests.
        richmenWithShares <- sublistOf richmenWithCerts
        let n = length richmenWithShares

        stableCerts <- UnsafeVssCertificatesMap . HM.fromList <$>
            mapM (\r -> (,) <$> pure r <*> (arbitrary :: Gen VssCertificate))
                 richmenWithCerts

        -- Like with 'GoodCommsPayload', only the simplest case is considered w.r.t. the
        -- 'lastKnownEoS' field: 'checkSharesPayload' is called with the same epoch as the
        -- 'lastKnownEoS' in 'gsVssCertificate'.
        _sgsVssCertificates <- VssCertData
            <$> (pure . EpochOrSlot . Right . crucialSlot $ gpEpoch)
            <*> pure stableCerts
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

        innerMaps <- (vector n) :: Gen [InnerSharesMap]
        let gpPayload = HM.fromList $ zip richmenWithShares innerMaps
            -- Every key from every inner shares map must have a commitment in the global
            -- state i.e. be a key in '_sgsCommitments'.
            necessaryKeys = concatMap HM.keys innerMaps

        _sgsCommitments <- mkCommitmentsMapUnsafe <$> do
            necessaryMap <- HM.fromList <$>
                mapM (\k -> (,) <$> pure k <*> arbitrary) necessaryKeys
            fillerMap <- arbitrary :: Gen (HashMap StakeholderId SignedCommitment)
            return $ HM.union necessaryMap fillerMap
        -- The keys in the shares map with which 'checkSharesPayload' is ran must not
        -- exist in '_sgsShares' for the global data to be correct.
        _sgsShares <-
            customHashMapGen (arbitrary `suchThat` (not . flip elem richmenWithShares))
                             arbitrary
        let gpGlobalState = SscGlobalState {..}

        return GoodPayload {..}

-- NOTE: this test does not care for 'DecrSharesNotMatchCommitment' failure. This would
--make the already non-trivial arbitrary instance for 'GoodSharesPayload' unmanageable.
checksGoodSharesPayload :: HasConfiguration => GoodSharesPayload -> Bool
checksGoodSharesPayload (GoodPayload epoch sgs sharesMap mrs) =
    case tossRunner mrs sgs $ checkSharesPayload epoch sharesMap of
        Left (DecrSharesNotMatchCommitment _) -> True
        Right _                               -> True
        _                                     -> False

-- | Checks that when the data 'checkSharesPayload' is passed is incorrect w.r.t.
-- 'checkSharesPayload', said function fails.
-- NOTE: does not check for 'DecrSharesNotMatchCommitment' failure. This would make the
-- already non-trivial arbitrary instance for 'GoodSharesPayload' unmanageable.
checksBadSharesPayload
    :: HasConfiguration
    => GoodSharesPayload
    -> PublicKey
    -> NonEmpty (AsBinary DecShare)
    -> VssCertificate
    -> Property
checksBadSharesPayload (GoodPayload epoch g@SscGlobalState {..} sm mrs) pk ne cert =
    let sid = addressHash pk

        -- This property assumes the existence of a stakeholder not present in neither the
        -- commitments nor shares map. Instead of writing a new 'Arbitrary' type which
        -- will only be useful here, this condition is just enforced by deleting it from
        -- where it shouldn't be.
        sgs = g & sgsShares %~ HM.delete sid
                 & sgsCommitments %~ deleteSignedCommitment sid
        sharesMap = fmap (HM.delete sid) . HM.delete sid $ sm

        mrsWithMissingEpoch = HM.delete epoch mrs
        noRichmen =
            tossRunner mrsWithMissingEpoch sgs $ checkSharesPayload epoch sharesMap
        res1 = noRichmen === Left (NoRichmen epoch)

        newSharesMap = HM.insert sid mempty sharesMap
        sharesNotRichmen = tossRunner mrs sgs $ checkSharesPayload epoch newSharesMap
        res2 = case sharesNotRichmen of
            Left (SharesNotRichmen nes) -> sid `qcElem` nes
            _ -> qcFail $ "expected " <> show sharesNotRichmen <>
                          " to be a Left (SharesNotRichmen ...)"

        newerSharesMap = fmap (HM.insert sid ne) sharesMap
        internalShareWithoutComm =
            tossRunner mrs sgs $ checkSharesPayload epoch newerSharesMap
        res3 = case internalShareWithoutComm of
            Left (InternalShareWithoutCommitment nes) -> sid `qcElem` nes
            _ -> qcFail $ "expected " <> show internalShareWithoutComm <>
                          " to be a Left (InternalShareWithoutCommitment ...)"

        -- The expiry epoch has to be big, otherwise the cert won't get
        -- inserted. However, making it 'maxBound' doesn't work because then
        -- it gets incremented internally and overflows
        cert' = cert { vcSigningKey = pk, vcExpiryEpoch = pred maxBound }
        newestSharesMap = HM.insert sid mempty sharesMap
        sgs' = sgs & sgsShares %~ HM.insert sid mempty
                   & sgsVssCertificates %~ \vcd@VssCertData{..} ->
                         vcd { certs = fst $ insertVss cert' certs}
        mrs' = HM.update (Just . HM.insert sid (mkCoin 0)) epoch mrs
        sharesAlreadySent =
            tossRunner mrs' sgs' $ checkSharesPayload epoch newestSharesMap
        res4 = case sharesAlreadySent of
            Left (SharesAlreadySent nes) -> sid `qcElem` nes
            _ -> qcFail $ "expected " <> show sharesAlreadySent <>
                          " to be a Left (SharesAlreadySent ...)"

        allCerts = toList . getVssCertificatesMap $
                   certs (sgs ^. sgsVssCertificates)

    in not (null sharesMap) &&
       not (vcVssKey cert `elem` map vcVssKey allCerts) &&
       not (pk `elem` map vcSigningKey allCerts)
       ==> res1 .&&. res2 .&&. res3 .&&. res4

type GoodCertsPayload = GoodPayload VssCertificatesMap

instance HasConfiguration => Arbitrary GoodCertsPayload where
    arbitrary = do

        -- These fields of 'SscGlobalState' are irrelevant for the
        -- 'checkCertificatesPayload' function.
        _sgsShares <- arbitrary
        _sgsOpenings <- arbitrary
        _sgsCommitments <- arbitrary

        -- We'll need an 'EpochIndex' to run 'checkCertificatesPayload'. This epoch index
        -- will also need an accompanying 'RichmenStakes', but because we'll need the
        -- public keys to generate valid 'VssCertificates' w.r.t.
        -- 'checkCertificatesPayload', a list with public keys in tuples is generated
        -- as an intermediate step.
        (gpEpoch, NonEmpty richKeys, m) <-
            arbitrary
                :: Gen (EpochIndex, NonEmptyList (PublicKey, Coin), MultiRichmenStakes)
        let richmenPks :: [PublicKey]
            richmenPks = map fst richKeys
            richmen :: RichmenStakes
            richmen = HM.fromList $ map (over _1 addressHash) richKeys
            -- The 'epoch' epoch is guaranteed to exist in 'richmen', so we
            -- can use '(!)' to search for it in later tests.
            gpMultiRichmenStakes :: MultiRichmenStakes
            gpMultiRichmenStakes = HM.insert gpEpoch richmen m

        -- This is the list of participants in SSC that will have a
        -- certificate in the 'VssCertificatesMap' which'll be returned.
        participants <- sublistOf $ HM.keys richmen

        -- This 'VssCertificatesMap' satisfies:
        --   * Every 'vcSigningKey' in all its certificates has a corresponding
        --    'StakeholderId' in 'mrs HM.! epoch'
        --   * The set of its 'StakeholderId' keys is a subset of all the
        --    'StakeholderId's in 'mrs'
        gpPayload <- UnsafeVssCertificatesMap <$> do
            let vssGen :: Gen VssCertificate
                vssGen = do
                    -- This list is guaranteed to be non-empty
                    richman <- elements richmenPks
                    vssCert <- arbitrary
                    return $ vssCert {vcSigningKey = richman}
                gen :: StakeholderId -> Gen (StakeholderId, VssCertificate)
                gen sid = (,) <$> pure sid <*> vssGen
            participantsMap <- HM.fromList <$> mapM gen participants
            fillerMap <- customHashMapGen arbitrary vssGen
            return $
                -- remove certs with duplicate VSS keys
                HM.fromList . nubOrdOn (vcVssKey . snd) . HM.toList $
                HM.union participantsMap fillerMap

        -- The 'VssCertificatesMap' field of this 'VssCertData' value satisfies:
        --   * None of its 'StakeholderId' keys is a richman
        --    (i.e. is a member of 'richmen')
        --   * None of the VSS keys from 'gpPayload' are present here
        -- TODO: this generates an invalid VssCertificatesMap because the
        -- stakeholders don't match the certificates
        let payloadVssKeys = HS.fromList $ map vcVssKey $
                toList (getVssCertificatesMap gpPayload)
        _sgsVssCertificates <- do
            certs <- UnsafeVssCertificatesMap <$> customHashMapGen
                (arbitrary `suchThat` (not . flip HM.member richmen))
                (arbitrary `suchThat` (not . flip HS.member payloadVssKeys . vcVssKey))
            vssData <- arbitrary
            return $ vssData {certs = certs}
        let gpGlobalState = SscGlobalState {..}

        return GoodPayload {..}

checksGoodCertsPayload :: HasConfiguration => GoodCertsPayload -> Property
checksGoodCertsPayload (GoodPayload epoch sgs certsMap mrs) =
    qcIsRight . tossRunner mrs sgs $ checkCertificatesPayload epoch certsMap

checksBadCertsPayload :: HasConfiguration => GoodCertsPayload -> PublicKey -> VssCertificate -> Property
checksBadCertsPayload (GoodPayload epoch sgs certsMap mrs) pk cert =
    let sid = addressHash pk

        mrsWithMissingEpoch = HM.delete epoch mrs
        noRichmen =
            tossRunner mrsWithMissingEpoch sgs $ checkCertificatesPayload epoch certsMap
        res1 = noRichmen === Left (NoRichmen epoch)

        -- The expiry epoch has to be big, otherwise the cert won't get
        -- inserted. However, making it 'maxBound' doesn't work because then
        -- it gets incremented internally and overflows
        cert' = cert { vcSigningKey = pk, vcExpiryEpoch = pred maxBound }
        (certsMap2, _) = insertVss cert' certsMap
        newSgs = sgs & sgsVssCertificates %~
            \vcd -> vcd { certs = fst $ insertVss cert' (certs vcd) }
        certAlreadySent =
            tossRunner mrs newSgs $ checkCertificatesPayload epoch certsMap2
        res2 = case certAlreadySent of
            Left (CertificateAlreadySent nes) -> sid `qcElem` nes
            _ -> qcFail $ "expected " <> show certAlreadySent <>
                          " to be a Left (CertificateAlreadySent ...)"

        certSid = addressHash . vcSigningKey $ cert
        (certsMap3, _) = insertVss cert certsMap
        certNoRichmen =
            tossRunner mrs sgs $ checkCertificatesPayload epoch certsMap3
        res3 = case certNoRichmen of
            Left (CertificateNotRichmen nes) -> certSid `qcElem` nes
            _ -> qcFail $ "expected " <> show certNoRichmen <>
                          " to be a Left (CertificateNotRichmen ...)"

        -- We take the VSS key of some cert from 'sgs' and replace a key of
        -- some cert in 'certsMap' with it
        res4 = fromMaybe (property True) $ do
            c1 <- (fmap fst . uncons . toList) (certs (sgs ^. sgsVssCertificates))
            let c1id = addressHash . vcSigningKey $ c1
            c2id <- (fmap fst . uncons) (HM.keys (getVssCertificatesMap certsMap))
            let certsMap4 = certsMap
                    & _Wrapped . ix c2id . _vcVssKey .~ vcVssKey c1
                certDuplicateVss =
                    tossRunner mrs sgs $
                    checkCertificatesPayload epoch certsMap4
            pure $ c1id /= c2id ==> case certDuplicateVss of
                Left (CertificateDuplicateVssKey nes) -> c2id `qcElem` nes
                _ -> qcFail $ "expected " <> show certDuplicateVss <>
                              " to be a Left (CertificateDuplicateVssKey ...)"

        allVssKeys = map vcVssKey (toList (getVssCertificatesMap certsMap))

    in (not (HM.member certSid $ mrs HM.! epoch) &&
        not (vcVssKey cert `elem` allVssKeys))
       ==> res1 .&&. res2 .&&. res3 .&&. res4

----------------------------------------------------------------------------
-- Utility functions for this module
----------------------------------------------------------------------------

-- Going to use fake randomness here because threading MonadRandom through
-- everything is annoying
tossRunner :: HasConfiguration
           => MultiRichmenStakes
           -> SscGlobalState
           -> ExceptT e PureTossWithEnv a
           -> Either e a
tossRunner mrs sgs =
    view _1 .
    fst . Rand.withDRG (Rand.drgNewTest (123,456,789,12345,67890)) .
    runPureToss sgs .
    supplyPureTossEnv (mrs, genesisBlockVersionData) .
    runExceptT

customHashMapGen
    :: (Hashable k, Eq k)
    => Gen k -> Gen v -> Gen (HM.HashMap k v)
customHashMapGen keyGen valGen = HM.fromList <$> (listOf $ (,) <$> keyGen <*> valGen)
