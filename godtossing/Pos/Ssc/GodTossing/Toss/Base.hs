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
       , computeSharesDistrPure
       , computeSharesDistr
       , isDistrInaccuracyAcceptable
       , sharesDistrInaccuracy
       , sharesDistrMaxSumDistr

       -- * Payload processing
       , checkCommitmentsPayload
       , checkOpeningsPayload
       , checkSharesPayload
       , checkCertificatesPayload
       , checkPayload

       -- * Helpers
       , verifyEntriesGuardM
       ) where

import           Universum

import           Control.Monad.Except            (MonadError (throwError))
import           Control.Monad.ST                (ST, runST)
import           Data.Array.MArray               (newArray, readArray, writeArray)
import           Data.Array.ST                   (STUArray)
import           Data.Containers                 (ContainerKey, SetContainer (notMember))
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS
import qualified Data.List.NonEmpty              as NE
import           Data.STRef                      (newSTRef, readSTRef, writeSTRef)
import           Formatting                      (ords, sformat, (%))
import           System.Wlog                     (logWarning)

import           Pos.Binary.Class                (AsBinary, fromBinaryM)
import           Pos.Core                        (CoinPortion, EpochIndex, StakeholderId,
                                                  addressHash, bvdMpcThd,
                                                  coinPortionDenominator, getCoinPortion,
                                                  unsafeGetCoin)
import           Pos.Crypto                      (Share, verifyShare)
import           Pos.Lrc.Types                   (RichmenSet, RichmenStakes)
import           Pos.Ssc.GodTossing.Core         (Commitment (..),
                                                  CommitmentsMap (getCommitmentsMap),
                                                  GtPayload (..), InnerSharesMap,
                                                  Opening (..), OpeningsMap,
                                                  SharesDistribution, SharesMap,
                                                  SignedCommitment, VssCertificatesMap,
                                                  VssCertificatesMap, commShares,
                                                  vcSigningKey, vcVssKey, verifyOpening,
                                                  verifyOpening, _gpCertificates)
import           Pos.Ssc.GodTossing.Toss.Class   (MonadToss (..), MonadTossEnv (..),
                                                  MonadTossRead (..))
import           Pos.Ssc.GodTossing.Toss.Failure (TossVerFailure (..))
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
getParticipants :: (MonadError TossVerFailure m, MonadToss m, MonadTossEnv m)
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
    :: (MonadTossRead m, MonadTossEnv m)
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

-- | We accept inaccuracy in computation not greater than 0.05,
-- so stakeholders must have at least 55% of stake to reveal secret
-- in the worst case
sharesDistrInaccuracy :: Fractional a => a
sharesDistrInaccuracy = 0.05

-- | Max sum of distribution
sharesDistrMaxSumDistr :: RealFrac a => a -> Word16
sharesDistrMaxSumDistr thd = truncate $ 3 / thd

-- | Internal type for work with Coin.
type CoinUnsafe = Int64

-- | Types represents one dimensional array
-- used for knapsack algorithm in the @computeDistrInaccuracy@
type Knapsack s = STUArray s Word16 CoinUnsafe

-- ATTENTION: IMPERATIVE CODE! PROTECT YOUR EYES! --
-- | Compute inaccuracy between real distribution and generared.
-- It take O(totalDistr * length consNDistr) time.

-- Max inaccuracy is more or less heuristic value
-- which means difference between generated and
-- real distribution we can get in the worst case.
-- This inaccuracy can lead to two bad situation:
-- 1. when nodes mustn't reveal commitment, but they can
-- 2. when nodes must reveal commitment, but they can't
-- We can get these situations when sum of stakes of nodes
-- which sent shares is close to 0.5.
isDistrInaccuracyAcceptable :: [(CoinUnsafe, Word16)] -> Bool
isDistrInaccuracyAcceptable coinsNDistr = runST $ do
    let !totalDistr = sum $ map snd coinsNDistr
    let !totalCoins = sum $ map fst coinsNDistr
    let halfDistr = totalDistr `div` 2 + 1
    let invalid = totalCoins + 1

    -- A sum of generated portions can be computed as
    -- sum of corresponding shares distribution divided by @totalDistr@.
    -- A sum of real portions can be computed as
    -- sum of corresponding coins divided by @totalCoins@.

    -- For the bad case of type 2,
    -- for each sum of shares distribution which is less than @totalDistr@ / 2
    -- we would know the maximum sum of real distribution corresponding to
    -- nodes which form this sum of shares distribution
    -- to evaluate max inaccuracy of the bad case type 2.
    -- So for every sum of generated shares
    -- we store this sum of coins and try to maximize it.

    -- We don't compute bad case of type 1 explicitly,
    -- because if there is such subset which causes inaccuracy of type 1
    -- we can take complement of this subset and get the same inaccuracy of type 2.
    dpMax <- newArray (0, totalDistr) (-invalid) :: ST s (Knapsack s)
    writeArray dpMax 0 0

    -- Relaxation function.
    let relax dp coins w nw cmp = do
            dpW <- readArray dp w
            dpNw <- readArray dp nw
            when ((dpW + coins) `cmp` dpNw) $
                writeArray dp nw (dpW + coins)
            pure (dpW + coins)

    let weights = [halfDistr - 1, halfDistr - 2..0]
    let halfCoins = totalCoins `div` 2 + 1
    let totalDistrD, totalCoinsD :: Double
        totalDistrD = fromIntegral totalDistr
        totalCoinsD = fromIntegral totalCoins

    let computeLimit i =
            let p = fromIntegral i / totalDistrD in
            max halfCoins (ceiling (totalCoinsD * (sharesDistrInaccuracy + p)))

    let weightsNLimits = zip weights (map computeLimit weights)
    isAcceptable <- newSTRef True
    -- Iterate over distribution
    forM_ coinsNDistr $ \(coins, distr) -> whenM (readSTRef isAcceptable) $ do
        -- Try to relax coins for whole weights
        forM_ weightsNLimits $ \(w, limit) -> when (w >= distr) $ do
            sCoinsMx <- relax dpMax coins (w - distr) w (>)
            when (sCoinsMx >= limit) $ writeSTRef isAcceptable False
    readSTRef isAcceptable

computeSharesDistrPure
    :: MonadError TossVerFailure m
    => RichmenStakes
    -> CoinPortion             -- ^ MPC threshold, e.g. 'genesisMpcThd'
    -> m SharesDistribution
computeSharesDistrPure richmen threshold
    | null richmen = pure mempty
    | otherwise = do
        when (totalCoins == 0) $
            throwError $ TossInternallError "Richmen total stake equals zero"

        let mpcThreshold = toRational (getCoinPortion threshold) / toRational coinPortionDenominator
        unless (all ((>= mpcThreshold) . toRational) portions) $
            throwError $ TossInternallError "Richmen stakes less than threshsold"

        let fromX = ceiling $ 1 / minimum portions
        let toX = sharesDistrMaxSumDistr mpcThreshold

        -- If we didn't find an appropriate distribution
        -- we use distribution [1, 1, ... 1] as fallback.
        pure $ HM.fromList $ zip keys $ fromMaybe (repeat 1) (compute fromX toX 0)
  where
    keys :: [StakeholderId]
    keys = map fst $ HM.toList richmen

    coins :: [CoinUnsafe]
    coins = map (fromIntegral . unsafeGetCoin . snd) (HM.toList richmen)

    portions :: [Double]
    portions = map ((/ fromIntegral totalCoins) . fromIntegral) coins

    totalCoins :: CoinUnsafe
    totalCoins = sum coins

    -- We multiply all portions by mult and divide them on their gcd
    --     we get commitment distribution
    -- compute sum difference between real portions and current portions
    -- select optimum using next strategy:
    --   * if sum error < epsilon - we try minimize sum of commitments
    --   * otherwise we try minimize sum error
    compute :: Word16 -> Word16 -> Word16 -> Maybe [Word16]
    compute !x !toX !prevSum
        | x > toX = Nothing
        | otherwise = do
            let curDistrN = multPortions x
            let curDistr = normalize curDistrN
            let !s = sum curDistr
            if s == prevSum then compute (x + 1) toX prevSum
            else if isDistrInaccuracyAcceptable (zip coins curDistr) then Just curDistr
            else compute (x + 1) toX s

    multPortions :: Word16 -> [Word16]
    multPortions mult = map (truncate . (fromIntegral mult *)) portions

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
-- Impure versions
----------------------------------------------------------------------------

-- | Like 'computeSharesDistrPure', but uses MPC threshold from the database.
computeSharesDistr
    :: (MonadToss m, MonadTossEnv m, MonadError TossVerFailure m)
    => RichmenStakes -> m SharesDistribution
computeSharesDistr richmen =
    computeSharesDistrPure richmen =<< (bvdMpcThd <$> getAdoptedBVData)

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
    :: (MonadToss m, MonadTossEnv m, MonadError TossVerFailure m)
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
    :: (MonadToss m, MonadTossEnv m, MonadError TossVerFailure m)
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
    :: (MonadToss m, MonadTossEnv m, MonadError TossVerFailure m)
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
    :: (MonadToss m, MonadTossEnv m, MonadError TossVerFailure m)
    => EpochIndex
    -> GtPayload
    -> m ()
checkPayload epoch payload = do
    let payloadCerts = _gpCertificates payload
    -- We explicitly don't check commitments if they are empty.
    -- It's ok, because empty commitments are always valid.
    -- And it certainly makes sense, because commitments check requires us to
    -- compute 'SharesDistribution' which might expensive.
    case payload of
        CommitmentsPayload comms _
            | null comms -> pass
            | otherwise -> checkCommitmentsPayload epoch comms
        OpeningsPayload opens _ -> checkOpeningsPayload opens
        SharesPayload shares _ -> checkSharesPayload epoch shares
        CertificatesPayload _ -> pass
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
