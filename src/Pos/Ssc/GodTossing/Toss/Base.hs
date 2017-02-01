{-# LANGUAGE TypeFamilies #-}

-- | Basic functionality from Toss.

module Pos.Ssc.GodTossing.Toss.Base
       ( getCommitment
       , hasCommitment
       , hasOpening
       , hasShares
       , hasCertificate
       , getParticipants

       , checkCommitmentShares
       , matchCommitment
       , checkShares

       , computeParticipants
       , checkCommitmentSharesPure
       , matchCommitmentPure
       , checkSharesPure

       , verifyEntriesGuardM
       , checkCommitmentsPayload
       , checkOpeningsPayload
       , checkSharesPayload
       , checkCertificatesPayload
       , checkPayload
       ) where

import           Data.Containers                 (ContainerKey, SetContainer (notMember))
import qualified Data.HashMap.Strict             as HM
import qualified Data.HashSet                    as HS
import qualified Data.List.NonEmpty              as NE
import           Formatting                      (ords, sformat, (%))
import           System.Wlog                     (logWarning)

import           Universum

import           Control.Monad.Except            (MonadError (throwError))
import           Pos.Crypto                      (Share, VssPublicKey, verifyShare)
import           Pos.Lrc.Types                   (RichmenSet)
import           Pos.Ssc.GodTossing.Core         (CommitmentsMap (getCommitmentsMap),
                                                  GtPayload (..), InnerSharesMap, Opening,
                                                  OpeningsMap, SharesMap,
                                                  SignedCommitment, VssCertificatesMap,
                                                  VssCertificatesMap, commShares,
                                                  vcSigningKey, vcVssKey, verifyOpening,
                                                  verifyOpening, _gpCertificates)
import           Pos.Ssc.GodTossing.Toss.Class   (MonadToss (..), MonadTossRead (..))
import           Pos.Ssc.GodTossing.Toss.Failure (TossVerErrorTag (..),
                                                  TossVerFailure (..))
import           Pos.Types                       (EpochIndex, StakeholderId, addressHash)
import           Pos.Util                        (AsBinary, fromBinaryM, getKeys)

----------------------------------------------------------------------------
-- Trivial getters (proper interface of MonadTossRead)
----------------------------------------------------------------------------

-- | Retrieve 'SignedCommitment' of given stakeholder if it's known.
getCommitment :: MonadTossRead m => StakeholderId -> m (Maybe SignedCommitment)
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

getParticipants :: (MonadError TossVerFailure m, MonadToss m)
                => EpochIndex
                -> m VssCertificatesMap
getParticipants epoch = do
    stableCerts <- getStableCertificates epoch
    richmenSet <- maybe (throwError $ NoRichmen epoch) pure =<< getRichmen epoch
    pure $ computeParticipants richmenSet stableCerts
----------------------------------------------------------------------------
-- Simple checks is 'MonadTossPure'
----------------------------------------------------------------------------

-- | Check that commitment is generated for correct set of
-- participants for given epoch.
checkCommitmentShares
    :: MonadTossRead m
    => EpochIndex -> SignedCommitment -> m Bool
checkCommitmentShares epoch comm = do
    certs <- getStableCertificates epoch
    let warnFmt = ("checkCommitmentShares: no richmen for "%ords%" epoch")
    getRichmen epoch >>= \case
        Nothing -> False <$ logWarning (sformat warnFmt epoch)
        Just richmen -> do
            let parts =
                    map vcVssKey $ toList $ computeParticipants richmen certs
            pure $ checkCommitmentSharesPure parts comm

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
            let parts = computeParticipants richmen certs
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

-- CHECK: @matchCommitmentPure
-- | Check that the secret revealed in the opening matches the secret proof
-- in the commitment.
matchCommitmentPure
    :: CommitmentsMap -> (StakeholderId, Opening) -> Bool
matchCommitmentPure (getCommitmentsMap -> globalCommitments) (id, opening) =
    case HM.lookup id globalCommitments of
        Nothing           -> False
        Just (_, comm, _) -> verifyOpening comm opening

-- CHECK: @checkShare
-- | Check that the decrypted share matches the encrypted share in the
-- commitment
--
-- #verifyShare
checkSharePure :: (SetContainer set, ContainerKey set ~ StakeholderId)
           => CommitmentsMap
           -> set --set of opening's addresses
           -> VssCertificatesMap
           -> (StakeholderId, StakeholderId, AsBinary Share)
           -> Bool
checkSharePure globalCommitments globalOpeningsPK globalCertificates (addrTo, addrFrom, share) =
    fromMaybe False $ case tuple of
      Just (eS, pk, s) -> verifyShare
                            <$> fromBinaryM eS
                            <*> fromBinaryM pk
                            <*> fromBinaryM s
      _ -> return False
  where
    tuple = do
        -- addrFrom sent its decrypted share to addrTo on commitment phase.
        -- addrTo must decrypt share from addrFrom on shares phase,
        -- if addrFrom didn't send its opening

        -- CHECK: Check that addrFrom really didn't send its opening
        guard $ notMember addrFrom globalOpeningsPK
        -- CHECK: Check that addrFrom really sent its commitment
        (_, comm, _) <- HM.lookup addrFrom $ getCommitmentsMap globalCommitments
        -- Get pkTo's vss certificate
        vssKey <- vcVssKey <$> HM.lookup addrTo globalCertificates
        -- Get encrypted share, which was sent from pkFrom to pkTo on commitment phase
        encShare <- HM.lookup vssKey (commShares comm)
        return (encShare, vssKey, share)

-- CHECK: @checkShares
-- Apply checkShare to all shares in map.
--
-- #checkShare
checkSharesPure
    :: (SetContainer set, ContainerKey set ~ StakeholderId)
    => CommitmentsMap
    -> set --set of opening's PK. TODO Should we add phantom type for more typesafety?
    -> VssCertificatesMap
    -> StakeholderId
    -> InnerSharesMap
    -> Bool
checkSharesPure globalCommitments globalOpeningsPK globalCertificates addrTo shares =
    let listShares :: [(StakeholderId, StakeholderId, AsBinary Share)]
        listShares = map convert $ HM.toList shares
        convert (addrFrom, share) = (addrTo, addrFrom, share)
    in all
           (checkSharePure globalCommitments globalOpeningsPK globalCertificates)
           listShares

-- | Check that commitment is generated for proper set of participants.
checkCommitmentSharesPure :: [AsBinary VssPublicKey] -> SignedCommitment -> Bool
checkCommitmentSharesPure vssPublicKeys c =
    HS.fromList vssPublicKeys == (getKeys . commShares $ c ^. _2)

-- For commitments we
--   * check that committing node is participant, i. e. she is richman and
--     her VSS certificate is one of stable certificates
--   * check that the nodes haven't already sent their commitments before
--     in some different block
--   * every commitment owner has enough (mpc+delegated) stake
checkCommitmentsPayload
    :: (MonadToss m, MonadError TossVerFailure m)
    => EpochIndex
    -> CommitmentsMap
    -> m ()
checkCommitmentsPayload epoch (getCommitmentsMap -> comms) = do
    participants <- getParticipants epoch
    exceptGuard CommitingNoParticipants
        (`HM.member` participants) (HM.keys comms)
    exceptGuardM CommitmentAlreadySent
        (notM hasCommitment) (HM.keys comms)
    exceptGuardSndM CommSharesOnWrongParticipants
        (checkCommitmentShares epoch) (HM.toList comms)
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
        matchCommitment (HM.toList opens)

-- For shares, we check that
--   * shares have corresponding commitments
--   * these shares weren't sent before
--   * if encrypted shares (in commitments) are decrypted, they match
--     decrypted shares
-- We don't check whether shares match the openings.
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
        (checkShares epoch) (HM.toList shares)

checkCertificatesPayload
    :: (MonadToss m, MonadError TossVerFailure m)
    => EpochIndex
    -> VssCertificatesMap
    -> m ()
checkCertificatesPayload epoch certs = do
    richmenSet <- maybe (throwError $ NoRichmen epoch) pure
                        =<< getRichmen epoch
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
    :: MonadError TossVerFailure m => TossVerErrorTag -> (StakeholderId -> Bool) -> [StakeholderId] -> m ()
exceptGuard tag f =
    verifyEntriesGuardM identity identity (TossVerFailure tag) (pure . f)

exceptGuardM
    :: MonadError TossVerFailure m => TossVerErrorTag -> (StakeholderId -> m Bool) -> [StakeholderId] -> m ()
exceptGuardM =
    verifyEntriesGuardM identity identity . TossVerFailure

exceptGuardSndM
    :: MonadError TossVerFailure m => TossVerErrorTag -> (val -> m Bool) -> [(StakeholderId, val)] -> m ()
exceptGuardSndM = verifyEntriesGuardM fst snd . TossVerFailure

exceptGuardSnd
    :: MonadError TossVerFailure m => TossVerErrorTag -> (val -> Bool) -> [(StakeholderId, val)] -> m ()
exceptGuardSnd tag f =
    verifyEntriesGuardM fst snd (TossVerFailure tag) (pure . f)

exceptGuardEntryM
    :: MonadError TossVerFailure m => TossVerErrorTag
    -> ((StakeholderId, val) -> m Bool)
    -> [(StakeholderId, val)] -> m ()
exceptGuardEntryM = verifyEntriesGuardM fst identity . TossVerFailure

notM :: Monad m => (a -> m Bool) -> (a -> m Bool)
notM f = (pure . not) <=< f
