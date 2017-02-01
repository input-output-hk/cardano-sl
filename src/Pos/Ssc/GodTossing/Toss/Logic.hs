-- | Main Toss logic.

module Pos.Ssc.GodTossing.Toss.Logic
       ( verifyAndApplyGtPayload
       , applyGenesisBlock
       , rollbackGT
       , normalizeToss
       ) where

import           Control.Monad.Except            (MonadError)
import qualified Data.HashMap.Strict             as HM
import           System.Wlog                     (logError)
import           Universum

import           Pos.Ssc.GodTossing.Core         (GtPayload (..), getCommitmentsMap)
import           Pos.Ssc.GodTossing.Functions    (verifyGtPayload)
import           Pos.Ssc.GodTossing.Toss.Class   (MonadToss (..))
import           Pos.Ssc.GodTossing.Toss.Failure (TossVerFailure (..))
import           Pos.Ssc.GodTossing.Toss.Types   (TossModifier)
import           Pos.Ssc.GodTossing.Type         ()
import           Pos.Types                       (EpochIndex, EpochOrSlot,
                                                  MainBlockHeader, getEpochOrSlot)
import           Pos.Util                        (NewestFirst (..))

-- | Verify 'GtPayload' with respect to data provided by
-- MonadToss. If data is valid it is also applied.  Otherwise
-- TossVerFailure is thrown using 'MonadError' type class.
verifyAndApplyGtPayload
    :: (MonadToss m, MonadError TossVerFailure m)
    => Either EpochIndex (MainBlockHeader ssc) -> GtPayload -> m ()
verifyAndApplyGtPayload eoh payload = do
    verifyGtPayload eoh payload  -- not necessary for blocks, but just in case
    -- TODO: more
    notImplemented

-- | Apply genesis block for given epoch to 'Toss' state.
applyGenesisBlock :: MonadToss m => EpochIndex -> m ()
applyGenesisBlock epoch = do
    setEpochOrSlot $ getEpochOrSlot epoch
    resetCOS

-- | Rollback application of 'GtPayload's in 'Toss'. First argument is
-- 'EpochOrSlot' of oldest block which is subject to rollback.
rollbackGT :: MonadToss m => EpochOrSlot -> NewestFirst [] GtPayload -> m ()
rollbackGT oldestEOS (NewestFirst payloads)
    | oldestEOS == toEnum 0 = do
        logError "rollbackGT: most genesis block is passed to rollback"
        setEpochOrSlot oldestEOS
        resetCOS
    | otherwise = do
        setEpochOrSlot (pred oldestEOS)
        mapM_ rollbackGTDo payloads
  where
    rollbackGTDo (CommitmentsPayload comms _) =
        mapM_ delCommitment $ HM.keys $ getCommitmentsMap comms
    rollbackGTDo (OpeningsPayload opens _) = mapM_ delOpening $ HM.keys opens
    rollbackGTDo (SharesPayload shares _) = mapM_ delShares $ HM.keys shares
    rollbackGTDo (CertificatesPayload _) = pass

-- | Apply as much data from given 'TossModifier' as possible.
normalizeToss
    :: MonadToss m
    => EpochIndex -> TossModifier -> m ()
normalizeToss _ _ = const pass notImplemented

-- -- | Verify that if one adds given block to the current chain, it will
-- -- remain consistent with respect to SSC-related data.
-- gtVerifyBlock :: RichmenSet -> Block SscGodTossing -> GSVerify ()
-- -- Genesis blocks don't have any SSC data.
-- gtVerifyBlock _ (Left _) = pass
-- -- Main blocks have commitments, openings, shares and VSS
-- -- certificates.  We optionally (depending on verifyPure argument) use
-- -- verifyGtPayload to make the most general checks and also use global
-- -- data to make more checks using this data.
-- gtVerifyBlock richmenSet (Right b) = do
--     let slot@SlotId{siSlot = slotId} = b ^. blockSlot
--         payload      = b ^. blockMpc
--         curEpoch = siEpoch $ b ^. blockSlot
--         blockCerts = _gpCertificates payload

--     globalCommitments <- view gsCommitments
--     globalOpenings    <- view gsOpenings
--     globalShares      <- view gsShares
--     globalVCD         <- view gsVssCertificates
--     let globalComms   = getCommitmentsMap globalCommitments
--     let globalCerts   = VCD.certs globalVCD
--     let stableCerts   = getStableCertsPure curEpoch globalVCD
--     let participants  = computeParticipants richmenSet stableCerts
--     let participantsVssKeys = map vcVssKey $ toList participants

--     logDebug $ sformat ("Global certificates: "%listJson
--                         %", stable certificates: "%listJson
--                         %", richmen: "%listJson)
--                globalCerts stableCerts richmenSet

--     let isComm  = unless (isCommitmentIdx slotId) $ throwError $ NotCommitmentPhase slot
--         isOpen  = unless (isOpeningIdx slotId) $ throwError $ NotOpeningPhase slot
--         isShare = unless (isSharesIdx slotId) $ throwError $ NotSharesPhase slot

--     -- For commitments we
--     --   * check that committing node is participant, i. e. she is richman and
--     --     her VSS certificate is one of stable certificates
--     --   * check that the nodes haven't already sent their commitments before
--     --     in some different block
--     --   * every commitment owner has enough (mpc+delegated) stake
--     let commChecks (getCommitmentsMap -> comms) = do
--             isComm
--             exceptGuard CommitingNoParticipants
--                 (`HM.member` participants) (HM.keys comms)
--             exceptGuard CommitmentAlreadySent
--                 (not . (`HM.member` globalComms)) (HM.keys comms)
--             exceptGuardSnd CommSharesOnWrongParticipants
--                 (checkCommShares participantsVssKeys) (HM.toList comms)
--             -- [CSL-206]: check that share IDs are different.

--     -- For openings, we check that
--     --   * the opening isn't present in previous blocks
--     --   * corresponding commitment is present
--     --   * the opening matches the commitment (this check implies that previous
--     --     one passes)
--     let openChecks opens = do
--             isOpen
--             exceptGuard OpeningAlreadySent
--                 (not . (`HM.member` globalOpenings)) (HM.keys opens)
--             exceptGuard OpeningWithoutCommitment
--                 (`HM.member` globalComms) (HM.keys opens)
--             exceptGuardEntry OpeningNotMatchCommitment
--                 (checkOpeningMatchesCommitment globalCommitments) (HM.toList opens)

--     -- For shares, we check that
--     --   * shares have corresponding commitments
--     --   * these shares weren't sent before
--     --   * if encrypted shares (in commitments) are decrypted, they match
--     --     decrypted shares
--     -- We don't check whether shares match the openings.
--     let shareChecks shares = do
--             isShare
--             -- We intentionally don't check, that nodes which decrypted shares
--             -- sent its commitments.
--             -- If node decrypted shares correctly, such node is useful for us, despite of
--             -- it didn't send its commitment.
--             exceptGuard SharesNotRichmen
--                 (`HS.member` richmenSet) (HM.keys shares)
--             exceptGuard InternalShareWithoutCommitment
--                 (`HM.member` globalComms) (concatMap HM.keys $ toList shares)
--             exceptGuard SharesAlreadySent
--                 (not . (`HM.member` globalShares)) (HM.keys shares)
--             exceptGuardEntry DecrSharesNotMatchCommitment
--                 (uncurry (checkShares globalCommitments globalOpenings participants))
--                 (HM.toList shares)

--     let certChecks certs = do
--             exceptGuard CertificateAlreadySent
--                 (not . (`HM.member` globalCerts)) (HM.keys certs)
--             exceptGuardSnd CertificateNotRichmen
--                 ((`HS.member` richmenSet) . addressHash . vcSigningKey) (HM.toList certs)

--     certChecks blockCerts
--     case payload of
--         CommitmentsPayload  comms  _ -> commChecks comms
--         OpeningsPayload     opens  _ -> openChecks opens
--         SharesPayload       shares _ -> shareChecks shares
--         CertificatesPayload        _ -> pass
--   where
--     exceptGuard = verifyEntriesGuard identity identity . TossVerFailure

--     exceptGuardSnd = verifyEntriesGuard fst snd . TossVerFailure

--     exceptGuardEntry = verifyEntriesGuard fst identity . TossVerFailure

-- gtVerifyAndApplyBlocks
--     :: RichmenSet
--     -> OldestFirst NE (Block SscGodTossing)
--     -> SscVerifier SscGodTossing ()
-- gtVerifyAndApplyBlocks richmen blocks =
--     forM_ blocks $ \b -> do
--         readerTToState $ gtVerifyBlock richmen b
--         gtApplyBlock b
--   where
--     -- TODO: get rid of it
--     readerTToState
--         :: MonadState s m
--         => ReaderT s m a -> m a
--     readerTToState rdr = get >>= runReaderT rdr

-- gtApplyBlock
--     :: (SscPayload ssc ~ GtPayload)
--     => Block ssc -> GSUpdate ()
-- gtApplyBlock blk = do
--     let eos = blk ^. epochOrSlotG
--     gsVssCertificates %= VCD.setLastKnownEoS eos
--     case blk of
--         -- Genesis blocks don't contain anything interesting, but when they
--         -- “arrive”, we clear global commitments and other globals. Not
--         -- certificates, though, because we don't want to make nodes resend
--         -- them in each epoch.
--         Left _  -> resetGS
--         -- Main blocks contain commitments, openings, shares, VSS certificates
--         Right b -> identity %= unionPayload True (b ^. blockMpc)

-- ----------------------------------------------------------------------------
-- -- Utilities
-- ----------------------------------------------------------------------------

-- unionPayload :: Bool -> GtPayload -> GtGlobalState -> GtGlobalState
-- unionPayload considerCertificates payload gs =
--     flip execState gs $ do
--         case payload of
--             CommitmentsPayload comms _ -> gsCommitments <>= comms
--             OpeningsPayload opens _    -> gsOpenings <>= opens
--             SharesPayload shares _     -> gsShares <>= shares
--             CertificatesPayload _      -> pure ()
--         when considerCertificates $
--             gsVssCertificates %=
--                 flip (foldr' VCD.insert)
--                      (toList $ _gpCertificates payload)
