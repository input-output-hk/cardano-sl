{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.GodTossing.Functions
       (
         hasCommitment
       , hasOpening
       , hasShares
       , hasVssCertificate

       -- * GtPayload
       , verifyGtPayload

       -- * VSS
       , vssThreshold
       , getStableCertsPure
       ) where

import           Control.Lens                    (to)
import           Control.Monad.Except            (MonadError (throwError))
import qualified Data.HashMap.Strict             as HM
import           Serokell.Util.Verify            (isVerSuccess)
import           Universum

import           Pos.Binary.Crypto               ()
import           Pos.Binary.Ssc.GodTossing.Core  ()
import           Pos.Crypto                      (Threshold)
import           Pos.Ssc.GodTossing.Core         (CommitmentsMap (getCommitmentsMap),
                                                  GtPayload (..), VssCertificate (..),
                                                  VssCertificatesMap, checkCertTTL,
                                                  isCommitmentId, isOpeningId, isSharesId,
                                                  verifySignedCommitment)
import           Pos.Ssc.GodTossing.Genesis      (genesisCertificates)
import           Pos.Ssc.GodTossing.Toss.Base    (verifyEntriesGuardM)
import           Pos.Ssc.GodTossing.Toss.Failure (TossVerErrorTag (..),
                                                  TossVerFailure (..))
import           Pos.Ssc.GodTossing.Types.Types  (GtGlobalState (..))
import qualified Pos.Ssc.GodTossing.VssCertData  as VCD
import           Pos.Types.Address               (addressHash)
import           Pos.Types.Block                 (MainBlockHeader, headerSlot)
import           Pos.Types.Core                  (EpochIndex (..), SlotId (..),
                                                  StakeholderId)
import           Pos.Types.Slotting              (crucialSlot)

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

-- CHECK: @verifyGtPayLoad
-- Verify payload using header containing this payload.
--
-- For each DS datum we check:
--
--   1. Whether it's stored in the correct block (e.g. commitments have to be in
--      first 2 * blkSecurityParam blocks, etc.)
--
--   2. Whether the message itself is correct (e.g. commitment signature is
--      valid, etc.)
--
-- We also do some general sanity checks.
verifyGtPayload
    :: MonadError TossVerFailure m
    => Either EpochIndex (MainBlockHeader ssc) -> GtPayload -> m ()
verifyGtPayload eoh payload = case payload of
    CommitmentsPayload comms certs -> do
        whenMB eoh isComm
        commChecks comms
        certsChecks certs
    OpeningsPayload        _ certs -> do
        whenMB eoh isOpen
        certsChecks certs
    SharesPayload          _ certs -> do
        whenMB eoh isShare
        certsChecks certs
    CertificatesPayload      certs -> do
        whenMB eoh isOther
        certsChecks certs
  where
    whenMB (Left _) _   = pass
    whenMB (Right mb) f = f $ mb ^. headerSlot

    epochId  = either identity (view $ headerSlot . to siEpoch) eoh
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
        let checkSignedComm =
                 isVerSuccess .
                 (verifySignedCommitment epochId)
        verifyEntriesGuardM (addressHash . view _1)
                            identity
                            (TossVerFailure CommitmentInvalid)
                            (pure . checkSignedComm)
                            (toList commitments)
        -- [CSL-206]: check that share IDs are different.

    -- CHECK: Vss certificates checker
    --
    --   * VSS certificates are signed properly
    --   * VSS certificates have valid TTLs
    --
    -- #checkCert
    certsChecks certs =
        verifyEntriesGuardM (second vcExpiryEpoch . join (,)) identity CertificateInvalidTTL
                            (pure . checkCertTTL epochId)
                            (toList certs)

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

-- | Figure out the threshold (i.e. how many secret shares would be required
-- to recover each node's secret) using number of participants.
vssThreshold :: Integral a => a -> Threshold
vssThreshold len = fromIntegral $ len `div` 2 + len `mod` 2

getStableCertsPure :: EpochIndex -> VCD.VssCertData -> VssCertificatesMap
getStableCertsPure epoch certs
    | epoch == 0 = genesisCertificates
    | otherwise =
          VCD.certs $ VCD.setLastKnownSlot (crucialSlot epoch) certs
