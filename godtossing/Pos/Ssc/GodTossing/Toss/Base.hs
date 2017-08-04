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
       , computeDistrInaccuracy
       , sharesDistrInaccuracy

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
import           Data.Containers                 (ContainerKey, SetContainer (notMember))
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS
import qualified Data.List.NonEmpty              as NE
import           Formatting                      (ords, sformat, (%))
import           System.Wlog                     (logWarning)
import           Data.Array.MArray               (newArray, readArray, writeArray)
import           Data.Array.ST                   (STUArray)
import           Control.Monad.ST                (ST, runST)

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

sharesDistrInaccuracy :: Fractional a => a
sharesDistrInaccuracy = 0.05

-- | Internal type for work with Coin.
type CoinUnsafe = Int

-- | Types represents one dimensional array
-- used for knapsack algorithm in the @computeDistrInaccuracy@
type Knapsack s = STUArray s Word16 CoinUnsafe

-- ATTENTION: IMPERATIVE CODE! PROTECT YOUR EYES! --
-- | Compute inaccuracy betwee real distribution and generared.
-- It take O(totalDistr * length consNDistr) time.
computeDistrInaccuracy :: (CoinUnsafe, Word16) -> [(CoinUnsafe, Word16)] -> Double
computeDistrInaccuracy (totalCoins, totalDistr) coinsNDistr = runST $ do
    let halfDistr = totalDistr `div` 2 + 1
    let invalid = totalCoins + 1 :: Int
    dpMax <- newArray (0, totalDistr) (-invalid) :: ST s (Knapsack s)
    dpMin <- newArray (0, totalDistr) invalid    :: ST s (Knapsack s)
    writeArray dpMax 0 0
    writeArray dpMin 0 0

    let relax dp coins w nw cmp = do
            dpW <- readArray dp w
            dpNw <- readArray dp nw
            when ((dpW + coins) `cmp` dpNw) $
                writeArray dp nw (dpW + coins)

    let weights = [totalDistr-1, totalDistr-2..0]
    forM_ coinsNDistr $ \(coins, distr) -> do
        forM_ weights $ \w -> when (w + distr <= totalDistr) $ do
            relax dpMax coins w (w + distr) (>)
            relax dpMin coins w (w + distr) (<)

    let computeError :: Word16 -> Int -> Double
        computeError i sCoins = abs $
            fromIntegral i / fromIntegral totalDistr -
            fromIntegral sCoins / fromIntegral totalCoins
    let selectMax = flip execStateT 0.0 $ forM_ [0..totalDistr] $ \i -> do
            if | i < halfDistr -> do
                -- Bad case of the second type is
                -- nodes can't reveal commitment using shares of honest nodes
                -- but real coin distribution says that nodes can do it.
                    sCoins <- lift (readArray dpMax i)
                    when (2 * sCoins > totalCoins) $
                        modify $ max $ computeError i sCoins
                | otherwise                -> do
                -- Bad case of the first type is
                -- nodes can reveal commitment using shares
                -- but real coin distribution says that nodes can't do it.
                    sCoins <- lift (readArray dpMin i)
                    when (2 * sCoins <= totalCoins) $
                        modify $ max $ computeError i sCoins
    selectMax

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
        -- We accept error in computation = 0.05,
        -- so stakeholders must have at least 55% of stake (for reveal secret) in the worst case
        let mpcThreshold = toRational (getCoinPortion threshold) / toRational coinPortionDenominator
        let fromX = ceiling $ 1.0 / minimum portions
        let toX = truncate $ toRational (3::Int) / mpcThreshold

        let keys = map fst $ HM.toList richmen
        unless (all ((>= mpcThreshold) . toRational) portions) $
            throwError $ TossInternallError "Richmen stakes less than threshsold"

        case compute fromX toX 0 of
            Nothing -> throwError $ TossInternallError "There is no appropriate distribution"
            Just res -> pure $ HM.fromList $ zip keys res
  where
    sharesDistrInaccuracyR = sharesDistrInaccuracy :: Double

    coins :: [CoinUnsafe]
    coins = map (fromIntegral . unsafeGetCoin) (toList richmen)

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
            else do
                let curInaccuracy = computeDistrInaccuracy (totalCoins, s) (zip coins curDistr)
                if curInaccuracy <= sharesDistrInaccuracyR then Just curDistr
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
