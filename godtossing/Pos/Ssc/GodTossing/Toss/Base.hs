{-# LANGUAGE TypeFamilies #-}

-- | Basic functionality from Toss.

module Pos.Ssc.GodTossing.Toss.Base
       (
         -- * Trivial functions
         getCommitment
       , hasCommitmentToss
       , hasOpeningToss
       , hasSharesToss
       , hasCertificateToss

       -- * Basic logic
       , getParticipants
       , computeParticipants
       , computeSharesDistr

       -- * Payload processing
       , checkCommitmentsPayload
       , checkOpeningsPayload
       , checkSharesPayload
       , checkCertificatesPayload
       , checkPayload

       -- * Helpers
       , verifyEntriesGuardM
       ) where

import           Control.Monad.Except            (MonadError (throwError))
import           Control.Monad.State             (get, put)
import           Data.Containers                 (ContainerKey, SetContainer (notMember))
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS
import qualified Data.List.NonEmpty              as NE
import           Formatting                      (ords, sformat, (%))
import           System.Wlog                     (logWarning)

import           Universum

import           Pos.Binary.Class                (AsBinary, fromBinaryM)
import           Pos.Core                        (EpochIndex, StakeholderId, addressHash,
                                                  unsafeGetCoin)
import           Pos.Core.Types                  (coinPortionDenominator, getCoinPortion)
import           Pos.Crypto                      (Share, verifyShare)
import           Pos.Lrc.Types                   (RichmenSet, RichmenStake)
import           Pos.Ssc.GodTossing.Core         (Commitment (..),
                                                  CommitmentsMap (getCommitmentsMap),
                                                  GtPayload (..), InnerSharesMap,
                                                  Opening (..), OpeningsMap,
                                                  SharesDistribution, SharesMap,
                                                  SignedCommitment, VssCertificatesMap,
                                                  VssCertificatesMap, commShares,
                                                  vcSigningKey, vcVssKey, verifyOpening,
                                                  verifyOpening, _gpCertificates)
import           Pos.Ssc.GodTossing.Toss.Class   (MonadToss (..), MonadTossRead (..))
import           Pos.Ssc.GodTossing.Toss.Failure (TossVerFailure (..))
import           Pos.Update.Constants            (genesisMpcThd)
import           Pos.Util.Util                   (getKeys)

----------------------------------------------------------------------------
-- Trivial getters (proper interface of MonadTossRead)
----------------------------------------------------------------------------

-- | Retrieve 'SignedCommitment' of given stakeholder if it's known.
getCommitment :: MonadTossRead m => StakeholderId -> m (Maybe SignedCommitment)
getCommitment id = HM.lookup id . getCommitmentsMap <$> getCommitments

-- | Check whether there is a 'SignedCommitment' from given stakeholder.
hasCommitmentToss :: MonadTossRead m => StakeholderId -> m Bool
hasCommitmentToss id = HM.member id . getCommitmentsMap <$> getCommitments

-- | Check whether there is an 'Opening' from given stakeholder.
hasOpeningToss :: MonadTossRead m => StakeholderId -> m Bool
hasOpeningToss id = HM.member id <$> getOpenings

-- | Check whether there is 'InnerSharesMap' from given stakeholder.
hasSharesToss :: MonadTossRead m => StakeholderId -> m Bool
hasSharesToss id = HM.member id <$> getShares

-- | Check whether there is 'VssCertificate' from given stakeholder.
hasCertificateToss :: MonadTossRead m => StakeholderId -> m Bool
hasCertificateToss id = HM.member id <$> getVssCertificates

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
matchCommitment
    :: MonadTossRead m
    => (StakeholderId, Opening) -> m Bool
matchCommitment op = flip matchCommitmentPure op <$> getCommitments

checkShares
    :: MonadTossRead m
    => EpochIndex -> (StakeholderId, InnerSharesMap) -> m Bool
checkShares epoch (id, sh) = do
    certs <- getStableCertificates epoch
    let warnFmt = ("checkShares: no richmen for "%ords%" epoch")
    getRichmen epoch >>= \case
        Nothing -> False <$ logWarning (sformat warnFmt epoch)
        Just richmen -> do
            let parts = computeParticipants (getKeys richmen) certs
            coms <- getCommitments
            ops <- getOpenings
            pure $ checkSharesPure coms ops parts id sh

----------------------------------------------------------------------------
-- Pure functions
----------------------------------------------------------------------------

-- | Compute 'VssCertificate's of GodTossing participants using set of
-- richmen and stable certificates.
computeParticipants :: RichmenSet -> VssCertificatesMap -> VssCertificatesMap
computeParticipants (HS.toMap -> richmen) = flip HM.intersection richmen

computeSharesDistr
    :: MonadError TossVerFailure m
    => RichmenStake -> m SharesDistribution
computeSharesDistr richmen = do
    let total :: Word64
        total = sum $ map unsafeGetCoin $ toList richmen
    when (total == 0) $
        throwError $ TossInternallError "Richmen total stake equals zero"
    let epsilon = 0.05::Rational
    -- We accept error in computation = 0.05,
    -- so stakeholders must have at least 55% of stake (for reveal secret) in the worst case
    let mpcThreshold = toRational (getCoinPortion genesisMpcThd) / toRational coinPortionDenominator
    let fromX = 1
    let toX = truncate $ toRational (3::Int) / mpcThreshold

    let keys = map fst $ HM.toList richmen
    let portions = map ((`divRat` total) . unsafeGetCoin . snd) $ HM.toList richmen
    unless (all (>= mpcThreshold) portions) $
        throwError $ TossInternallError "Richmen stakes less than threshsold"

    let init = normalize $ multPortions portions toX
    let initS = sum init
    let initDelta = calcSumError (map (`divRat` initS) init) portions
    (_, _, res) <-
            execStateT (compute fromX toX epsilon portions) (initDelta, initS, init)
    pure $ HM.fromList $ zip keys res
  where
    -- We multiply all portions by mult and divide them on their gcd
    --     we get commitment distribution
    -- compute sum difference between real portions and current portions
    -- select optimum using next strategy:
    --   * if sum error < epsilon - we try minimize sum of commitments
    --   * otherwise we try minimize sum error
    compute fromX toX epsilon portions = do
        for_ [fromX..toX] $ \x -> do
            let curDistrN = multPortions portions x
            when (all (> 0) curDistrN) $ do
                let curDistr = normalize curDistrN
                let s = sum curDistr
                let curPortions = map (`divRat` s) curDistr
                let delta = calcSumError curPortions portions
                (optDelta, optS, _) <- get
                -- if delta less than epsilon then we try minimize sum of commitments
                when (delta <= epsilon && s < optS ||
                     -- otherwise we try minimize sum error of rounding
                      delta <= optDelta && delta > epsilon) $
                      put (delta, s, curDistr)

    calcSumError:: [Rational] -> [Rational] -> Rational
    calcSumError pNew p = runIdentity $ do
        let sorted1 = sortOn (\(a, b) -> b / a) $ zip p pNew
        let sorted2 = sortOn (\(a, b) -> b - a) $ zip p pNew
        let res1 = max (computeError1 0 0 sorted1) (computeError2 0 0 $ reverse sorted1)
        let res2 = max (computeError1 0 0 sorted2) (computeError2 0 0 $ reverse sorted2)
        pure $ max res1 res2

    half = 0.5::Rational
    -- Error when real stake more than 0.5 but our new stake less
    computeError1 _ _ [] = 0
    computeError1 real new (x:xs)
        | new < half && real >= half =
            max (real - half) $ computeError1 (real + fst x) (new + snd x) xs
        | otherwise = computeError1 (real + fst x) (new + snd x) xs

    -- Error when real stake less than 0.5 but our new stake more
    computeError2 _ _ [] = 0
    computeError2 real new (x:xs)
        | new >= half && real < half =
            max (half - real) $ computeError2 (real + fst x) (new + snd x) xs
        | otherwise = computeError2 (real + fst x) (new + snd x) xs

    multPortions :: [Rational] -> Word16 -> [Word16]
    multPortions p (toRational -> mult) = map (truncate . (* mult)) p

    divRat :: (Real a, Real b) => a -> b -> Rational
    divRat x y = toRational x / toRational y

    normalize :: [Word16] -> [Word16]
    normalize x = let g = listGCD x in map (`div` g) x

    listGCD (x:xs) = foldl' gcd x xs
    listGCD []     = 1

-- CHECK: @matchCommitmentPure
-- | Check that the secret revealed in the opening matches the secret proof
-- in the commitment.
matchCommitmentPure
    :: CommitmentsMap -> (StakeholderId, Opening) -> Bool
matchCommitmentPure (getCommitmentsMap -> globalCommitments) (id, op) =
    case HM.lookup id globalCommitments of
        Nothing           -> False
        Just (_, comm, _) -> verifyOpening comm op

-- CHECK: @checkShare
-- | Check that the decrypted share matches the encrypted share in the
-- commitment
--
-- #verifyShare
checkSharePure :: (SetContainer set, ContainerKey set ~ StakeholderId)
           => CommitmentsMap
           -> set --set of opening's identifiers
           -> VssCertificatesMap
           -> (StakeholderId, StakeholderId, NonEmpty (AsBinary Share))
           -> Bool
checkSharePure globalCommitments globalOpeningsPK globalCertificates (idTo, idFrom, multiShare) =
    fromMaybe False checks
  where
    -- idFrom sent its encrypted share to idTo on commitment phase
    -- idTo must decrypt share from idFrom on shares phase,

    checks = do
        -- CHECK: Check that idFrom really sent its commitment
        (_, Commitment{..}, _) <- HM.lookup idFrom $ getCommitmentsMap globalCommitments
        -- Get idTo's vss certificate
        vssKey <- vcVssKey <$> HM.lookup idTo globalCertificates
        idToCommShares <- HM.lookup vssKey commShares
        -- CHECK: Check that commitment's shares and multishare have same length
        guard $ length multiShare == length idToCommShares
        -- CHECK: Check that idFrom really didn't send its opening
        guard $ notMember idFrom globalOpeningsPK
        -- Get encrypted share, which was sent from idFrom to idTo in
        -- commitment phase
        pure $ all (checkShare vssKey) $ NE.zip idToCommShares multiShare
    checkShare vssKey (encShare, share) = fromMaybe False $
        verifyShare <$> fromBinaryM encShare
                    <*> fromBinaryM vssKey
                    <*> fromBinaryM share

-- CHECK: @checkSharesPure
-- Apply checkShare to all shares in map.
--
-- #checkSharePure
checkSharesPure
    :: (SetContainer set, ContainerKey set ~ StakeholderId)
    => CommitmentsMap
    -> set --set of opening's PK. TODO Should we add phantom type for more typesafety?
    -> VssCertificatesMap
    -> StakeholderId
    -> InnerSharesMap
    -> Bool
checkSharesPure globalCommitments globalOpeningsPK globalCertificates addrTo shares =
    let listShares :: [(StakeholderId, StakeholderId, NonEmpty (AsBinary Share))]
        listShares = map convert $ HM.toList shares
        convert (addrFrom, share) = (addrTo, addrFrom, share)
    in all
           (checkSharePure globalCommitments globalOpeningsPK globalCertificates)
           listShares

-- | Check that commitment is generated for proper set of participants.
checkCommitmentShares :: SharesDistribution -> VssCertificatesMap -> SignedCommitment -> Bool
checkCommitmentShares distr participants  (_, Commitment{..}, _) =
    let vssPublicKeys = map vcVssKey $ toList participants
        idVss = map (second vcVssKey) $ HM.toList participants in
    (HS.fromList vssPublicKeys == getKeys commShares) && (all checkPK idVss)
  where
    checkPK (id, pk) = case HM.lookup pk commShares of
        Nothing -> False
        Just ne ->
            length ne == fromIntegral (HM.lookupDefault 0 id distr)

----------------------------------------------------------------------------
-- Payload processing
----------------------------------------------------------------------------

-- For commitments we check that
--   * committing node is participant, i. e. she is rich and
--     her VSS certificate is one of stable certificates
--   * the nodes haven't already sent their commitments before
--     in some different block
--   * commitment is generated exactly for all participants with correct
--     proportions (according to 'computeSharesDistr')
checkCommitmentsPayload
    :: (MonadToss m, MonadError TossVerFailure m)
    => EpochIndex
    -> CommitmentsMap
    -> m ()
checkCommitmentsPayload epoch (getCommitmentsMap -> comms) = do
    richmen <- note (NoRichmen epoch) =<< getRichmen epoch
    participants <- getParticipants epoch
    distr <- computeSharesDistr richmen
    exceptGuard CommitingNoParticipants
        (`HM.member` participants) (HM.keys comms)
    exceptGuardM CommitmentAlreadySent
        (notM hasCommitmentToss) (HM.keys comms)
    exceptGuardSnd CommSharesOnWrongParticipants
        (checkCommitmentShares distr participants) (HM.toList comms)

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
        (notM hasOpeningToss) (HM.keys opens)
    exceptGuardM OpeningWithoutCommitment
        hasCommitmentToss (HM.keys opens)
    exceptGuardEntryM OpeningNotMatchCommitment
        matchCommitment (HM.toList opens)

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
    -- We intentionally don't check that nodes which decrypted shares sent
    -- its commitments. If a node decrypted shares correctly, such node is
    -- useful for us, despite that it didn't send its commitment.
    part <- getParticipants epoch
    exceptGuard SharesNotRichmen
        (`HM.member` part) (HM.keys shares)
    exceptGuardM InternalShareWithoutCommitment
        hasCommitmentToss (concatMap HM.keys $ toList shares)
    exceptGuardM SharesAlreadySent
        (notM hasSharesToss) (HM.keys shares)
    exceptGuardEntryM DecrSharesNotMatchCommitment
        (checkShares epoch) (HM.toList shares)

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
        (notM hasCertificateToss) (HM.keys certs)
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
    (nonEmpty . map fKey) <$>
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
