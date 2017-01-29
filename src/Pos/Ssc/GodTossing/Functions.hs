{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.GodTossing.Functions
       (
         hasCommitment
       , hasOpening
       , hasShares
       , hasVssCertificate

       -- * GtPayload
       , verifyGtPayload

       -- * Verification helper
       , verifyEntriesGuard

       -- * VSS
       , vssThreshold
       , computeParticipants
       , getStableCertsPure
       ) where

import           Control.Lens                   (to)
import           Control.Monad.Except           (MonadError (throwError), runExcept)
import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS
import qualified Data.List.NonEmpty             as NE
import           Serokell.Util.Verify           (isVerSuccess)
import           Universum

import           Pos.Binary.Class               (Bi)
import           Pos.Binary.Crypto              ()
import           Pos.Crypto                     (Threshold)
import           Pos.Lrc.Types                  (Richmen)
import           Pos.Ssc.Class.Types            (Ssc (..))
import           Pos.Ssc.GodTossing.Core        (Commitment (..),
                                                 CommitmentsMap (getCommitmentsMap),
                                                 VssCertificate (..), VssCertificatesMap,
                                                 checkCertTTL, isCommitmentId,
                                                 isOpeningId, isSharesId,
                                                 verifySignedCommitment)
import           Pos.Ssc.GodTossing.Genesis     (genesisCertificates)
import           Pos.Ssc.GodTossing.Types.Types (GtGlobalState (..), GtPayload (..),
                                                 TossVerErrorTag (..),
                                                 TossVerFailure (..))
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Types.Address              (addressHash)
import           Pos.Types.Core                 (EpochIndex (..), SlotId (..),
                                                 StakeholderId)
import           Pos.Types.Slotting             (crucialSlot)
import           Pos.Types.Types                (MainBlockHeader, headerSlot)

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
    :: (SscPayload ssc ~ GtPayload, Bi Commitment)
    => Either EpochIndex (MainBlockHeader ssc)
    -> SscPayload ssc
    -> Either TossVerFailure ()
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
    isComm  slotId = unless (isCommitmentId slotId) $ Left $ NotCommitmentPhase slotId
    isOpen  slotId = unless (isOpeningId slotId) $ Left $ NotOpeningPhase slotId
    isShare slotId = unless (isSharesId slotId) $ Left $ NotSharesPhase slotId
    isOther slotId = unless (all not $
                      map ($ slotId) [isCommitmentId, isOpeningId, isSharesId]) $
                      Left $ NotIntermediatePhase slotId

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
    commChecks commitments = runExcept $ do
        let checkSignedComm =
                 isVerSuccess .
                 (verifySignedCommitment epochId)
        verifyEntriesGuard (addressHash . view _1)
                           identity
                           (TossVerFailure CommitmentInvalid)
                           checkSignedComm
                           (toList commitments)
        -- [CSL-206]: check that share IDs are different.

    -- CHECK: Vss certificates checker
    --
    --   * VSS certificates are signed properly
    --   * VSS certificates have valid TTLs
    --
    -- #checkCert
    certsChecks certs = runExcept $ do
        verifyEntriesGuard (second vcExpiryEpoch . join (,)) identity CertificateInvalidTTL
                           (checkCertTTL epochId)
                           (toList certs)

----------------------------------------------------------------------------
-- Verification helper
----------------------------------------------------------------------------

-- It takes list of entries ([(StakeholderId, v)] or [StakeholderId]),
-- function condition and error tag (fKey and fValue - see below)
-- If condition is true for every entry - function does nothing.
-- Otherwise it gets all entries which don't pass condition
-- and throwError with [StakeholderId] corresponding to these entries.
-- fKey is needed for getting StakeholderId from entry.
-- fValue is needed for getting value which must be tested by condition function.
verifyEntriesGuard
    :: MonadError TossVerFailure m
    => (entry -> key)
    -> (entry -> verificationVal)
    -> (NonEmpty key -> TossVerFailure)
    -> (verificationVal -> Bool)
    -> [entry]
    -> m ()
verifyEntriesGuard fKey fVal exception cond =
    maybeThrowError exception .
    NE.nonEmpty .
    map fKey .
    filter (not . cond . fVal)
  where
    maybeThrowError _ Nothing    = pass
    maybeThrowError er (Just ne) = throwError $ er ne

----------------------------------------------------------------------------
-- Modern
----------------------------------------------------------------------------

-- | Figure out the threshold (i.e. how many secret shares would be required
-- to recover each node's secret) using number of participants.
vssThreshold :: Integral a => a -> Threshold
vssThreshold len = fromIntegral $ len `div` 2 + len `mod` 2

computeParticipants :: Richmen -> VssCertificatesMap -> VssCertificatesMap
computeParticipants (HS.toMap . HS.fromList . NE.toList -> richmen) =
    (`HM.intersection` richmen)

getStableCertsPure :: EpochIndex -> VCD.VssCertData -> VssCertificatesMap
getStableCertsPure epoch certs
    | epoch == 0 = genesisCertificates
    | otherwise =
          VCD.certs $ VCD.setLastKnownSlot (crucialSlot epoch) certs
