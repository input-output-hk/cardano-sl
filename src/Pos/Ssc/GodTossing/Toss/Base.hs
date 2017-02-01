{-# LANGUAGE TypeFamilies #-}

-- | Basic functionality from Toss.

module Pos.Ssc.GodTossing.Toss.Base
       ( getCommitment
       , hasCommitment
       , hasOpening
       , hasShares
       , hasCertificate

       , checkCommitmentShares
       , matchCommitment
       , checkShares

       , computeParticipants
       , checkCommitmentSharesPure
       , matchCommitmentPure
       , checkSharesPure
       ) where

import           Data.Containers               (ContainerKey, SetContainer (notMember))
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import           Formatting                    (ords, sformat, (%))
import           System.Wlog                   (logWarning)

import           Universum

import           Pos.Crypto                    (Share, VssPublicKey, verifyShare)
import           Pos.Lrc.Types                 (RichmenSet)
import           Pos.Ssc.GodTossing.Core       (CommitmentsMap (getCommitmentsMap),
                                                InnerSharesMap, Opening, SignedCommitment,
                                                VssCertificatesMap, commShares, vcVssKey,
                                                verifyOpening)
import           Pos.Ssc.GodTossing.Toss.Class (MonadTossRead (..))
import           Pos.Types                     (EpochIndex, StakeholderId)
import           Pos.Util                      (AsBinary, fromBinaryM, getKeys)

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
