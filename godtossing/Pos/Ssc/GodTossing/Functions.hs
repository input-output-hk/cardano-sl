{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.GodTossing.Functions
       ( hasCommitment
       , hasOpening
       , hasShares
       , hasVssCertificate

       -- * GtPayload
       , verifyGTPayloadEpoch

       -- * VSS
       , vssThreshold
       , getStableCertsPure
       ) where

import           Universum

import           Control.Monad.Except            (MonadError)
import qualified Data.HashMap.Strict             as HM
import           Serokell.Util.Verify            (isVerSuccess)

import           Pos.Binary.Crypto               ()
import           Pos.Binary.GodTossing.Core      ()
import           Pos.Core                        (BlockCount, EpochIndex (..),
                                                  StakeholderId)
import           Pos.Core.Slotting               (crucialSlot)
import           Pos.Crypto                      (Threshold)
import           Pos.Ssc.GodTossing.Core         (CommitmentsMap (getCommitmentsMap),
                                                  GtPayload (..), VssCertificatesMap,
                                                  checkCertTTL, verifySignedCommitment)
import           Pos.Ssc.GodTossing.Genesis      (genesisCertificates)
import           Pos.Ssc.GodTossing.Toss.Base    (verifyEntriesGuardM)
import           Pos.Ssc.GodTossing.Toss.Failure (TossVerFailure (..))
import           Pos.Ssc.GodTossing.Types.Types  (GtGlobalState (..))
import qualified Pos.Ssc.GodTossing.VssCertData  as VCD

----------------------------------------------------------------------------
-- Simple predicates for GodTossing.Types.Base
----------------------------------------------------------------------------

hasCommitment :: StakeholderId -> GtGlobalState -> Bool
hasCommitment id = HM.member id . getCommitmentsMap . _gsCommitments

hasOpening :: StakeholderId -> GtGlobalState -> Bool
hasOpening id = HM.member id . _gsOpenings

hasShares :: StakeholderId -> GtGlobalState -> Bool
hasShares id = HM.member id . _gsShares

hasVssCertificate :: StakeholderId -> GtGlobalState -> Bool
hasVssCertificate id = VCD.member id . _gsVssCertificates

----------------------------------------------------------------------------
-- GtPayload Part
----------------------------------------------------------------------------

-- | Verify GodTossing payload for the given epoch.
verifyGTPayloadEpoch
    :: MonadError TossVerFailure m
    => EpochIndex -> GtPayload -> m ()
verifyGTPayloadEpoch epoch payload = case payload of
    CommitmentsPayload comms certs -> do
        -- whenHeader eoh isComm
        commChecks comms
        certsChecks certs
    OpeningsPayload        _ certs -> do
        -- whenHeader eoh isOpen
        certsChecks certs
    SharesPayload          _ certs -> do
        -- whenHeader eoh isShare
        certsChecks certs
    CertificatesPayload      certs -> do
        -- whenHeader eoh isOther
        certsChecks certs
  where
    -- FIXME [CSL-1436] Restore this checks (they should be in Toss now)
    -- whenHeader (Left _) _       = pass
    -- whenHeader (Right header) f = f $ header ^. headerSlotL

    -- isComm  slotId = unless (isCommitmentId slotId) $ throwError $ NotCommitmentPhase slotId
    -- isOpen  slotId = unless (isOpeningId slotId) $ throwError $ NotOpeningPhase slotId
    -- isShare slotId = unless (isSharesId slotId) $ throwError $ NotSharesPhase slotId
    -- isOther slotId = unless (all not $
    --                   map ($ slotId) [isCommitmentId, isOpeningId, isSharesId]) $
    --                   throwError $ NotIntermediatePhase slotId

    -- CHECK: For commitments specifically, we also
    --
    --   * check there are only commitments in the block
    --   * use verifySignedCommitment, which checks commitments themselves,
    --     e.g. checks their signatures (which includes checking that the
    --     commitment has been generated for this particular epoch)
    --
    -- #verifySignedCommitment
    commChecks commitments = do
        let checkComm =
                 isVerSuccess .
                 (verifySignedCommitment epoch)
        verifyEntriesGuardM fst snd CommitmentInvalid
                            (pure . checkComm)
                            (HM.toList . getCommitmentsMap $ commitments)

    -- CHECK: Vss certificates checker
    --
    --   * VSS certificates have valid TTLs
    --
    -- #checkCert
    certsChecks certs =
        verifyEntriesGuardM identity identity CertificateInvalidTTL
                            (pure . checkCertTTL epoch)
                            (toList certs)

----------------------------------------------------------------------------
-- Other helpers
----------------------------------------------------------------------------

-- | Figure out the threshold (i.e. how many secret shares would be required
-- to recover each node's secret) using number of participants.
vssThreshold :: Integral a => a -> Threshold
vssThreshold len = fromIntegral $ len `div` 2 + len `mod` 2

getStableCertsPure :: BlockCount -> EpochIndex -> VCD.VssCertData -> VssCertificatesMap
getStableCertsPure blkSecurityParam epoch certs
    | epoch == 0 = genesisCertificates
    | otherwise =
        VCD.certs $
        VCD.setLastKnownSlot (crucialSlot blkSecurityParam epoch) certs
