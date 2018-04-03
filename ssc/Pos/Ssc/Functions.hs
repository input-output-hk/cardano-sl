{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.Functions
       ( hasCommitment
       , hasOpening
       , hasShares
       , hasVssCertificate

       -- * SscPayload
       , verifySscPayload

       -- * VSS
       , vssThreshold
       , getStableCertsPure
       ) where

import           Universum hiding (id)

import           Control.Lens (to)
import           Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Strict as HM
import           Serokell.Util.Verify (isVerSuccess)

import           Pos.Binary.Core ()
import           Pos.Binary.Crypto ()
import           Pos.Core (EpochIndex (..), HasGenesisData, HasProtocolConstants,
                           IsMainHeader, SlotId (..), StakeholderId, VssCertificatesMap,
                           genesisVssCerts, headerSlotL, HasProtocolMagic)
import           Pos.Core.Slotting (crucialSlot)
import           Pos.Core.Ssc (CommitmentsMap (getCommitmentsMap), SscPayload (..))
import           Pos.Ssc.Base (checkCertTTL, isCommitmentId, isOpeningId, isSharesId,
                               verifySignedCommitment, vssThreshold)
import           Pos.Ssc.Error (SscVerifyError (..))
import           Pos.Ssc.Toss.Base (verifyEntriesGuardM)
import           Pos.Ssc.Types (SscGlobalState (..))
import qualified Pos.Ssc.VssCertData as VCD
import           Pos.Util.Some (Some)

----------------------------------------------------------------------------
-- Simple predicates for SSC.Types
----------------------------------------------------------------------------

hasCommitment :: StakeholderId -> SscGlobalState -> Bool
hasCommitment id = HM.member id . getCommitmentsMap . _sgsCommitments

hasOpening :: StakeholderId -> SscGlobalState -> Bool
hasOpening id = HM.member id . _sgsOpenings

hasShares :: StakeholderId -> SscGlobalState -> Bool
hasShares id = HM.member id . _sgsShares

hasVssCertificate :: StakeholderId -> SscGlobalState -> Bool
hasVssCertificate id = VCD.member id . _sgsVssCertificates

----------------------------------------------------------------------------
-- SscPayload Part
----------------------------------------------------------------------------

-- CHECK: @verifySscPayload
-- Verify payload using header containing this payload.
--
-- For each DS datum we check:
--
--   1. Whether it's stored in the correct block (e.g. commitments have to be
--      in first 2 * blkSecurityParam blocks, etc.)
--
--   2. Whether the message itself is correct (e.g. commitment signature is
--      valid, etc.)
--
-- We also do some general sanity checks.
verifySscPayload
    :: (MonadError SscVerifyError m, HasProtocolConstants, HasProtocolMagic)
    => Either EpochIndex (Some IsMainHeader) -> SscPayload -> m ()
verifySscPayload eoh payload = case payload of
    CommitmentsPayload comms certs -> do
        whenHeader eoh isComm
        commChecks comms
        certsChecks certs
    OpeningsPayload        _ certs -> do
        whenHeader eoh isOpen
        certsChecks certs
    SharesPayload          _ certs -> do
        whenHeader eoh isShare
        certsChecks certs
    CertificatesPayload      certs -> do
        whenHeader eoh isOther
        certsChecks certs
  where
    whenHeader (Left _) _       = pass
    whenHeader (Right header) f = f $ header ^. headerSlotL

    epochId = either identity (view $ headerSlotL . to siEpoch) eoh
    isComm  slotId = unless (isCommitmentId slotId) $ throwError $ NotCommitmentPhase slotId
    isOpen  slotId = unless (isOpeningId slotId) $ throwError $ NotOpeningPhase slotId
    isShare slotId = unless (isSharesId slotId) $ throwError $ NotSharesPhase slotId
    isOther slotId = unless (all not $
                      map ($ slotId) [isCommitmentId, isOpeningId, isSharesId]) $
                      throwError $ NotIntermediatePhase slotId

    -- We *forbid* blocks from having commitments/openings/shares in blocks
    -- with wrong slotId (instead of merely discarding such commitments/etc)
    -- because it's the miner's responsibility not to include them into the
    -- block if they're late.
    --
    -- CHECK: For commitments specifically, we also
    --
    --   * check there are only commitments in the block
    --   * use verifySignedCommitment, which checks commitments themselves,
    --     e.g. checks their signatures (which includes checking that the
    --     commitment has been generated for this particular epoch)
    --
    -- #verifySignedCommitment
    commChecks commitments = do
        let checkComm = isVerSuccess . verifySignedCommitment epochId
        verifyEntriesGuardM fst snd CommitmentInvalid
                            (pure . checkComm)
                            (HM.toList . getCommitmentsMap $ commitments)

    -- CHECK: Vss certificates checker
    --
    --   * VSS certificates are signed properly
    --   * VSS certificates have valid TTLs
    --
    -- #checkCert
    certsChecks certs =
        verifyEntriesGuardM identity identity CertificateInvalidTTL
                            (pure . checkCertTTL epochId)
                            (toList certs)

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

getStableCertsPure :: (HasProtocolConstants, HasGenesisData) => EpochIndex -> VCD.VssCertData -> VssCertificatesMap
getStableCertsPure epoch certs
    | epoch == 0 = genesisVssCerts
    | otherwise =
          VCD.certs $ VCD.setLastKnownSlot (crucialSlot epoch) certs
