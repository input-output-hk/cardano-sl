{-# LANGUAGE TypeFamilies #-}

-- | Basic functionality from Toss.

module Pos.Ssc.GodTossing.Toss.Base
       (
         -- * Trivial functions
         getCommitment
       , hasCommitment
       , hasOpening
       , hasShares
       , hasCertificate

       -- * Basic logic
       , getParticipants
       , computeParticipants
       , computeCommitmentDistr

       -- * Payload processing
       , checkCommitmentsPayload
       , checkOpeningsPayload
       , checkSharesPayload
       , checkCertificatesPayload
       , checkPayload

       -- * Helpers
       , verifyEntriesGuardM
       ) where

import           Control.Monad.State             (get, put)
import           Data.Containers                 (ContainerKey, SetContainer (notMember))
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS
import qualified Data.List.NonEmpty              as NE
import           Formatting                      (ords, sformat, (%))
import           System.Wlog                     (logWarning)

import           Universum

import           Control.Monad.Except            (MonadError (throwError))
import           Pos.Crypto                      (Share, VssPublicKey, verifyShare)
import           Pos.Lrc.Types                   (RichmenSet, RichmenStake)
import           Pos.Ssc.GodTossing.Core         (CommitmentsDistribution,
                                                  CommitmentsMap (getCommitmentsMap),
                                                  GtPayload (..), InnerSharesMap,
                                                  MultiCommitment (..), MultiOpening (..),
                                                  OpeningsMap, SharesMap,
                                                  VssCertificatesMap, VssCertificatesMap,
                                                  commShares, vcSigningKey, vcVssKey,
                                                  verifyOpening, verifyOpening,
                                                  _gpCertificates)
import           Pos.Ssc.GodTossing.Toss.Class   (MonadToss (..), MonadTossRead (..))
import           Pos.Ssc.GodTossing.Toss.Failure (TossVerFailure (..))
import           Pos.Types                       (EpochIndex, StakeholderId, addressHash,
                                                  unsafeGetCoin)
import           Pos.Util                        (AsBinary, fromBinaryM, getKeys)

----------------------------------------------------------------------------
-- Trivial getters (proper interface of MonadTossRead)
----------------------------------------------------------------------------

-- | Retrieve 'SignedCommitment' of given stakeholder if it's known.
getCommitment :: MonadTossRead m => StakeholderId -> m (Maybe MultiCommitment)
getCommitment id = HM.lookup id . getCommitmentsMap <$> getCommitments

-- | Check whether there is a 'SignedCommitment' from given stakeholder.
hasCommitment :: MonadTossRead m => StakeholderId -> m Bool
hasCommitment id = HM.member id . getCommitmentsMap <$> getCommitments

-- | Check whether there is an 'Opening' from given stakeholder.
hasOpening :: MonadTossRead m => StakeholderId -> m Bool
hasOpening id = HM.member id <$> getOpenings

-- | Check whether there is 'InnerSharesMap' from given stakeholder.
hasShares :: MonadTossRead m => StakeholderId -> m Bool
hasShares id = HM.member id <$> getShares

-- | Check whether there is 'VssCertificate' from given stakeholder.
hasCertificate :: MonadTossRead m => StakeholderId -> m Bool
hasCertificate id = HM.member id <$> getVssCertificates

----------------------------------------------------------------------------
-- Non-trivial getters
----------------------------------------------------------------------------

-- | Get 'VssCertificatesMap' containing 'StakeholderId's and
-- 'VssPublicKey's of participating nodes for given epoch.
getParticipants :: (MonadError TossVerFailure m, MonadToss m)
                => EpochIndex
                -> m VssCertificatesMap
getParticipants epoch = do
    stableCerts <- getStableCertificates epoch
    richmen <- note (NoRichmen epoch) =<< getRichmen epoch
    pure $ computeParticipants (getKeys richmen) stableCerts

----------------------------------------------------------------------------
-- Simple checks in 'MonadTossRead'
----------------------------------------------------------------------------

-- | Check that the secret revealed in the opening matches the secret proof
-- in the commitment.
matchMultiCommitment
    :: MonadTossRead m
    => (StakeholderId, MultiOpening) -> m Bool
matchMultiCommitment op = flip matchMultiCommitmentPure op <$> getCommitments

checkMultiShares
    :: MonadTossRead m
    => EpochIndex -> (StakeholderId, InnerSharesMap) -> m Bool
checkMultiShares epoch (id, sh) = do
    certs <- getStableCertificates epoch
    let warnFmt = ("checkShares: no richmen for "%ords%" epoch")
    getRichmen epoch >>= \case
        Nothing -> False <$ logWarning (sformat warnFmt epoch)
        Just richmen -> do
            let parts = computeParticipants (getKeys richmen) certs
            coms <- getCommitments
            ops <- getOpenings
            pure $ checkMultiSharesPure coms ops parts id sh

----------------------------------------------------------------------------
-- Pure functions
----------------------------------------------------------------------------

-- | Compute 'VssCertificate's of GodTossing participants using set of
-- richmen and stable certificates.
computeParticipants :: RichmenSet -> VssCertificatesMap -> VssCertificatesMap
computeParticipants (HS.toMap -> richmen) = flip HM.intersection richmen

computeCommitmentDistr
    :: MonadError TossVerFailure m
    => RichmenStake -> m CommitmentsDistribution
computeCommitmentDistr richmen = do
    let total :: Word64
        total = sum $ map unsafeGetCoin $ toList richmen
    -- Max error between real portion of node and portion of result
    let portionError = toRational (1::Int) / toRational (20::Int) -- 0.05
    -- Max multiplier (see below more)
    let maxMult = 100

    let keys = map fst $ HM.toList richmen
    let portions = map ((`divRat` total) . unsafeGetCoin . snd) $ HM.toList richmen
    (_, res) <-
            execStateT (compute maxMult portions portionError)
                       (maxMult * 10, multPortions portions maxMult)
    pure $ HM.fromList $ zip keys res
  where
    -- We multiply all portions by mult and divide on total sum - we get commitment distribution
    -- next divide them on their gcd,
    -- next check that their real portion and portion for current mult
    --   different less than 'portionError',
    -- select minimal sum of commitment distribution by 1 <= mult <= 100
    compute maxMult portions maxError = do
        forM_ [1..maxMult] $ \mult -> do
            let curDistr = multPortions portions mult
            let curTotal = sum curDistr
            let curPortions = map (`divRat` curTotal) curDistr
            when (curTotal /= 0 &&
                  all (compareDiff maxError) (zip portions curPortions) && -- different < 0.05
                  all (> 0) curDistr) $ do -- all portions are positive
                let g = listGCD curDistr
                let dividedDistr = map (`div` g) curDistr
                let curSum = sum dividedDistr
                ans <- fst <$> get
                when (curSum < ans) $ put (mult, dividedDistr)

    multPortions :: [Rational] -> Word16 -> [Word16]
    multPortions p (toRational -> mult) = map (truncate . (* mult)) p

    divRat :: (Real a, Real b) => a -> b -> Rational
    divRat x y = toRational x / toRational y

    listGCD (x:xs) = foldl' gcd x xs
    listGCD []     = 1

    compareDiff maxError (x, y) = abs (x - y) < maxError

-- CHECK: @matchCommitmentPure
-- | Check that the secret revealed in the opening matches the secret proof
-- in the commitment.
matchMultiCommitmentPure
    :: CommitmentsMap -> (StakeholderId, MultiOpening) -> Bool
matchMultiCommitmentPure (getCommitmentsMap -> globalCommitments) (id, MultiOpening{..}) =
    case HM.lookup id globalCommitments of
        Nothing                    -> False
        Just (MultiCommitment{..}) ->
            NE.length mcCommitments == NE.length moOpenings &&
            (all identity $ NE.zipWith (verifyOpening . fst) mcCommitments moOpenings)

-- CHECK: @checkShare
-- | Check that the decrypted share matches the encrypted share in the
-- commitment
--
-- #verifyShare
checkMultiSharePure :: (SetContainer set, ContainerKey set ~ StakeholderId)
           => CommitmentsMap
           -> set --set of opening's addresses
           -> VssCertificatesMap
           -> (StakeholderId, StakeholderId, NonEmpty (AsBinary Share))
           -> Bool
checkMultiSharePure globalCommitments globalOpeningsPK globalCertificates (addrTo, addrFrom, multiShare) =
    fromMaybe False checks
  where
    -- addrFrom sent its encrypted share to addrTo on commitment phase
    -- addrTo must decrypt share from addrFrom on shares phase,
    checks = do
        -- CHECK: Check that addrFrom really sent its commitment
        MultiCommitment{..} <- HM.lookup addrFrom $ getCommitmentsMap globalCommitments
        -- CHECK: Check that multicommitment and multishare have same length
        guard $ NE.length multiShare == NE.length mcCommitments
        -- CHECK: Check that addrFrom really didn't send its opening
        guard $ notMember addrFrom globalOpeningsPK
        -- Get pkTo's vss certificate
        vssKey <- vcVssKey <$> HM.lookup addrTo globalCertificates
        pure $ all (checkShare vssKey) $ NE.zip mcCommitments multiShare
    checkShare vssKey ((comm, _), share) = fromMaybe False $ do
        -- Get encrypted share, which was sent from pkFrom to pkTo on commitment phase
        encShare <- HM.lookup vssKey (commShares comm)
        verifyShare <$> fromBinaryM encShare
                    <*> fromBinaryM vssKey
                    <*> fromBinaryM share

-- CHECK: @checkSharesPure
-- Apply checkShare to all shares in map.
--
-- #checkSharePure
checkMultiSharesPure
    :: (SetContainer set, ContainerKey set ~ StakeholderId)
    => CommitmentsMap
    -> set --set of opening's PK. TODO Should we add phantom type for more typesafety?
    -> VssCertificatesMap
    -> StakeholderId
    -> InnerSharesMap
    -> Bool
checkMultiSharesPure globalCommitments globalOpeningsPK globalCertificates addrTo shares =
    let listShares :: [(StakeholderId, StakeholderId, NonEmpty (AsBinary Share))]
        listShares = map convert $ HM.toList shares
        convert (addrFrom, share) = (addrTo, addrFrom, share)
    in all
           (checkMultiSharePure globalCommitments globalOpeningsPK globalCertificates)
           listShares

-- | Check that commitment is generated for proper set of participants.
checkCommitmentShares :: [AsBinary VssPublicKey] -> MultiCommitment -> Bool
checkCommitmentShares vssPublicKeys MultiCommitment{..} = all checkComm mcCommitments
  where
    checkComm c = HS.fromList vssPublicKeys == (getKeys . commShares . fst $ c)

----------------------------------------------------------------------------
-- Payload processing
----------------------------------------------------------------------------

-- For commitments we check that
--   * committing node is participant, i. e. she is rich and
--     her VSS certificate is one of stable certificates
--   * the nodes haven't already sent their commitments before
--     in some different block
--   * commitment is generated exactly for all participants
checkCommitmentsPayload
    :: (MonadToss m, MonadError TossVerFailure m)
    => EpochIndex
    -> CommitmentsMap
    -> m ()
checkCommitmentsPayload epoch (getCommitmentsMap -> comms) = do
    richmen <- note (NoRichmen epoch) =<< getRichmen epoch
    participants <- getParticipants epoch
    let participantKeys = map vcVssKey $ toList participants
    distr <- computeCommitmentDistr richmen
    exceptGuard CommitingNoParticipants
        (`HM.member` participants) (HM.keys comms)
    exceptGuardM CommitmentAlreadySent
        (notM hasCommitment) (HM.keys comms)
    exceptGuardSnd CommSharesOnWrongParticipants
        (checkCommitmentShares participantKeys) (HM.toList comms)
    exceptGuardEntryM InvalidNumCommitments
        (pure . lessDistr distr) (HM.toList comms)
  where
    lessDistr distr (id, MultiCommitment{..}) =
        (fromIntegral (NE.length mcCommitments) <= HM.lookupDefault 0 id distr) &&
        (1 <= NE.length mcCommitments)
    -- [CSL-206]: check that share IDs are different.

-- For openings, we check that
--   * the opening isn't present in previous blocks
--   * corresponding commitment is present
--   * the opening matches the commitment (this check implies that previous
--     one passes)
checkOpeningsPayload
    :: (MonadToss m, MonadError TossVerFailure m)
    => OpeningsMap
    -> m ()
checkOpeningsPayload opens = do
    exceptGuardM OpeningAlreadySent
        (notM hasOpening) (HM.keys opens)
    exceptGuardM OpeningWithoutCommitment
        hasCommitment (HM.keys opens)
    exceptGuardEntryM OpeningNotMatchCommitment
        matchMultiCommitment (HM.toList opens)

-- For shares, we check that
--   * 'InnerSharesMap's are sent only by participants
--   * these 'InnerSharesMap's weren't sent before
--   * shares have corresponding commitments which don't have openings
--   * if encrypted shares (in commitments) are decrypted, they match
--     decrypted shares
checkSharesPayload
    :: (MonadToss m, MonadError TossVerFailure m)
    => EpochIndex
    -> SharesMap
    -> m ()
checkSharesPayload epoch shares = do
    -- We intentionally don't check, that nodes which decrypted shares
    -- sent its commitments.
    -- If node decrypted shares correctly, such node is useful for us, despite of
    -- it didn't send its commitment.
    part <- getParticipants epoch
    exceptGuard SharesNotRichmen
        (`HM.member` part) (HM.keys shares)
    exceptGuardM InternalShareWithoutCommitment
        hasCommitment (concatMap HM.keys $ toList shares)
    exceptGuardM SharesAlreadySent
        (notM hasShares) (HM.keys shares)
    exceptGuardEntryM DecrSharesNotMatchCommitment
        (checkMultiShares epoch) (HM.toList shares)

-- For certificates we check that
--   * certificate hasn't been sent already
--   * certificate is generated by richman
checkCertificatesPayload
    :: (MonadToss m, MonadError TossVerFailure m)
    => EpochIndex
    -> VssCertificatesMap
    -> m ()
checkCertificatesPayload epoch certs = do
    richmenSet <- getKeys <$> (note (NoRichmen epoch) =<< getRichmen epoch)
    exceptGuardM CertificateAlreadySent
        (notM hasCertificate) (HM.keys certs)
    exceptGuardSnd CertificateNotRichmen
        ((`HS.member` richmenSet) . addressHash . vcSigningKey)
        (HM.toList certs)

checkPayload
    :: (MonadToss m, MonadError TossVerFailure m)
    => EpochIndex
    -> GtPayload
    -> m ()
checkPayload epoch payload = do
    let payloadCerts = _gpCertificates payload
    case payload of
        CommitmentsPayload  comms  _ ->
            checkCommitmentsPayload epoch comms
        OpeningsPayload     opens  _ ->
            checkOpeningsPayload opens
        SharesPayload       shares _ ->
            checkSharesPayload epoch shares
        CertificatesPayload        _ ->
            pass
    checkCertificatesPayload epoch payloadCerts

----------------------------------------------------------------------------
-- Verification helpers
----------------------------------------------------------------------------
-- It takes list of entries ([(StakeholderId, v)] or [StakeholderId]),
-- function condition and error tag (fKey and fValue - see below)
-- If condition is true for every entry - function does nothing.
-- Otherwise it gets all entries which don't pass condition
-- and throwError with [StakeholderId] corresponding to these entries.
-- fKey is needed for getting StakeholderId from entry.
-- fValue is needed for getting value which must be tested by condition function.
verifyEntriesGuardM
    :: MonadError TossVerFailure m
    => (entry -> key)
    -> (entry -> verificationVal)
    -> (NonEmpty key -> TossVerFailure)
    -> (verificationVal -> m Bool)
    -> [entry]
    -> m ()
verifyEntriesGuardM fKey fVal exception cond lst =
    maybeThrowError exception =<<
    NE.nonEmpty <$>
    map fKey <$>
    filterM f lst
  where
    f x = not <$> cond (fVal x)

    maybeThrowError _ Nothing    = pass
    maybeThrowError er (Just ne) = throwError $ er ne

exceptGuard
    :: MonadError TossVerFailure m
    => (NonEmpty key -> TossVerFailure) -> (key -> Bool) -> [key] -> m ()
exceptGuard onFail f =
    verifyEntriesGuardM identity identity onFail (pure . f)

exceptGuardM
    :: MonadError TossVerFailure m
    => (NonEmpty key -> TossVerFailure) -> (key -> m Bool) -> [key] -> m ()
exceptGuardM =
    verifyEntriesGuardM identity identity

exceptGuardSnd
    :: MonadError TossVerFailure m
    => (NonEmpty key -> TossVerFailure) -> (val -> Bool) -> [(key, val)] -> m ()
exceptGuardSnd onFail f =
    verifyEntriesGuardM fst snd onFail (pure . f)

exceptGuardEntryM
    :: MonadError TossVerFailure m
    => (NonEmpty key -> TossVerFailure) -> ((key, val) -> m Bool) -> [(key, val)] -> m ()
exceptGuardEntryM = verifyEntriesGuardM fst identity

notM :: Monad m => (a -> m Bool) -> (a -> m Bool)
notM f = (pure . not) <=< f
