{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Instance of SscGStateClass.

module Pos.Ssc.GodTossing.GState
       ( -- * Instances
         -- ** instance SscGStateClass SscGodTossing
         getGlobalCerts
       , gtGetGlobalState
       , getStableCerts
       ) where

import           Control.Lens                   ((%=), (.=), (<>=), _Wrapped)
import           Control.Monad.Except           (MonadError (throwError))
import           Control.Monad.Reader           (ask)
import           Control.Monad.State            (get)
import           Data.Default                   (def)
import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS
import qualified Data.List.NonEmpty             as NE
import           Formatting                     (build, sformat, (%))
import           Serokell.Util.Text             (listJson)
import           System.Wlog                    (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Binary.Ssc                 ()
import           Pos.Constants                  (epochSlots, vssMaxTTL)
import           Pos.DB                         (DBError (DBMalformed), MonadDB,
                                                 getTipBlockHeader,
                                                 loadBlundsFromTipWhile)
import           Pos.Lrc.Types                  (RichmenSet)
import           Pos.Ssc.Class.Storage          (SscGStateClass (..), SscVerifier)
import           Pos.Ssc.Class.Types            (Ssc (..))
import           Pos.Ssc.Extra                  (MonadSscMem, sscRunGlobalQuery)
import           Pos.Ssc.GodTossing.Core        (GtPayload (..), VssCertificate (..),
                                                 VssCertificatesMap, checkCommShares,
                                                 checkShares, diffCommMap,
                                                 getCommitmentsMap, isCommitmentIdx,
                                                 isOpeningIdx, isSharesIdx, vcVssKey,
                                                 _gpCertificates)
import           Pos.Ssc.GodTossing.Functions   (computeParticipants, getStableCertsPure,
                                                 verifyEntriesGuard)
import           Pos.Ssc.GodTossing.Genesis     (genesisCertificates)
import           Pos.Ssc.GodTossing.Seed        (calculateSeed)
import           Pos.Ssc.GodTossing.Toss        (TossVerErrorTag (..),
                                                 TossVerFailure (..),
                                                 checkOpeningMatchesCommitment)
import           Pos.Ssc.GodTossing.Type        (SscGodTossing)
import           Pos.Ssc.GodTossing.Types       (GtGlobalState (..), gsCommitments,
                                                 gsOpenings, gsShares, gsVssCertificates)
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Types                      (Block, EpochIndex (..), EpochOrSlot (..),
                                                 SlotId (..), addressHash, blockMpc,
                                                 blockSlot, epochIndexL, epochOrSlot,
                                                 epochOrSlotG)
import           Pos.Util                       (NE, NewestFirst (..), OldestFirst (..),
                                                 maybeThrow, toOldestFirst)

type GSVerify a = forall m . ( MonadReader GtGlobalState m, WithLogger m
                             , MonadError TossVerFailure m) => m a
type GSUpdate a = forall m . (MonadState GtGlobalState m) => m a

instance SscGStateClass SscGodTossing where
    sscLoadGlobalState = mpcLoadGlobalState
    sscRollbackU = mpcRollback
    sscVerifyAndApplyBlocks = gtVerifyAndApplyBlocks
    sscCalculateSeedQ _ =
        calculateSeed <$> view gsCommitments <*> view gsOpenings <*>
        view gsShares

gtGetGlobalState
    :: (MonadSscMem SscGodTossing m, MonadIO m)
    => m GtGlobalState
gtGetGlobalState = sscRunGlobalQuery ask

getGlobalCerts
    :: (MonadSscMem SscGodTossing m, MonadIO m)
    => SlotId -> m VssCertificatesMap
getGlobalCerts sl =
    sscRunGlobalQuery $
        VCD.certs .
        VCD.setLastKnownSlot sl <$>
        view (gsVssCertificates)

-- | Get stable VSS certificates for given epoch.
getStableCerts
    :: (MonadSscMem SscGodTossing m, MonadIO m)
    => EpochIndex -> m VssCertificatesMap
getStableCerts epoch =
    getStableCertsPure epoch <$> sscRunGlobalQuery (view gsVssCertificates)

-- | Verify that if one adds given block to the current chain, it will
-- remain consistent with respect to SSC-related data.
gtVerifyBlock :: RichmenSet -> Block SscGodTossing -> GSVerify ()
-- Genesis blocks don't have any SSC data.
gtVerifyBlock _ (Left _) = pass
-- Main blocks have commitments, openings, shares and VSS
-- certificates.  We optionally (depending on verifyPure argument) use
-- verifyGtPayload to make the most general checks and also use global
-- data to make more checks using this data.
gtVerifyBlock richmenSet (Right b) = do
    let slot@SlotId{siSlot = slotId} = b ^. blockSlot
        payload      = b ^. blockMpc
        curEpoch = siEpoch $ b ^. blockSlot
        blockCerts = _gpCertificates payload

    globalCommitments <- view gsCommitments
    globalOpenings    <- view gsOpenings
    globalShares      <- view gsShares
    globalVCD         <- view gsVssCertificates
    let globalComms   = getCommitmentsMap globalCommitments
    let globalCerts   = VCD.certs globalVCD
    let stableCerts   = getStableCertsPure curEpoch globalVCD
    let participants  = computeParticipants richmenSet stableCerts
    let participantsVssKeys = map vcVssKey $ toList participants

    logDebug $ sformat ("Global certificates: "%listJson
                        %", stable certificates: "%listJson
                        %", richmen: "%listJson)
               globalCerts stableCerts richmenSet

    let isComm  = unless (isCommitmentIdx slotId) $ throwError $ NotCommitmentPhase slot
        isOpen  = unless (isOpeningIdx slotId) $ throwError $ NotOpeningPhase slot
        isShare = unless (isSharesIdx slotId) $ throwError $ NotSharesPhase slot

    -- For commitments we
    --   * check that committing node is participant, i. e. she is richman and
    --     her VSS certificate is one of stable certificates
    --   * check that the nodes haven't already sent their commitments before
    --     in some different block
    --   * every commitment owner has enough (mpc+delegated) stake
    let commChecks (getCommitmentsMap -> comms) = do
            isComm
            exceptGuard CommitingNoParticipants
                (`HM.member` participants) (HM.keys comms)
            exceptGuard CommitmentAlreadySent
                (not . (`HM.member` globalComms)) (HM.keys comms)
            exceptGuardSnd CommSharesOnWrongParticipants
                (checkCommShares participantsVssKeys) (HM.toList comms)
            -- [CSL-206]: check that share IDs are different.

    -- For openings, we check that
    --   * the opening isn't present in previous blocks
    --   * corresponding commitment is present
    --   * the opening matches the commitment (this check implies that previous
    --     one passes)
    let openChecks opens = do
            isOpen
            exceptGuard OpeningAlreadySent
                (not . (`HM.member` globalOpenings)) (HM.keys opens)
            exceptGuard OpeningWithoutCommitment
                (`HM.member` globalComms) (HM.keys opens)
            exceptGuardEntry OpeningNotMatchCommitment
                (checkOpeningMatchesCommitment globalCommitments) (HM.toList opens)

    -- For shares, we check that
    --   * shares have corresponding commitments
    --   * these shares weren't sent before
    --   * if encrypted shares (in commitments) are decrypted, they match
    --     decrypted shares
    -- We don't check whether shares match the openings.
    let shareChecks shares = do
            isShare
            -- We intentionally don't check, that nodes which decrypted shares
            -- sent its commitments.
            -- If node decrypted shares correctly, such node is useful for us, despite of
            -- it didn't send its commitment.
            exceptGuard SharesNotRichmen
                (`HS.member` richmenSet) (HM.keys shares)
            exceptGuard InternalShareWithoutCommitment
                (`HM.member` globalComms) (concatMap HM.keys $ toList shares)
            exceptGuard SharesAlreadySent
                (not . (`HM.member` globalShares)) (HM.keys shares)
            exceptGuardEntry DecrSharesNotMatchCommitment
                (uncurry (checkShares globalCommitments globalOpenings participants))
                (HM.toList shares)

    let certChecks certs = do
            exceptGuard CertificateAlreadySent
                (not . (`HM.member` globalCerts)) (HM.keys certs)
            exceptGuardSnd CertificateNotRichmen
                ((`HS.member` richmenSet) . addressHash . vcSigningKey) (HM.toList certs)

    certChecks blockCerts
    case payload of
        CommitmentsPayload  comms  _ -> commChecks comms
        OpeningsPayload     opens  _ -> openChecks opens
        SharesPayload       shares _ -> shareChecks shares
        CertificatesPayload        _ -> pass
  where
    exceptGuard = verifyEntriesGuard identity identity . TossVerFailure

    exceptGuardSnd = verifyEntriesGuard fst snd . TossVerFailure

    exceptGuardEntry = verifyEntriesGuard fst identity . TossVerFailure

gtVerifyAndApplyBlocks
    :: RichmenSet
    -> OldestFirst NE (Block SscGodTossing)
    -> SscVerifier SscGodTossing ()
gtVerifyAndApplyBlocks richmen blocks =
    forM_ blocks $ \b -> do
        readerTToState $ gtVerifyBlock richmen b
        gtApplyBlock b
  where
    -- TODO: get rid of it
    readerTToState
        :: MonadState s m
        => ReaderT s m a -> m a
    readerTToState rdr = get >>= runReaderT rdr

gtApplyBlock
    :: (SscPayload ssc ~ GtPayload)
    => Block ssc -> GSUpdate ()
gtApplyBlock blk = do
    let eos = blk ^. epochOrSlotG
    gsVssCertificates %= VCD.setLastKnownEoS eos
    case blk of
        -- Genesis blocks don't contain anything interesting, but when they
        -- “arrive”, we clear global commitments and other globals. Not
        -- certificates, though, because we don't want to make nodes resend
        -- them in each epoch.
        Left _  -> resetGS
        -- Main blocks contain commitments, openings, shares, VSS certificates
        Right b -> identity %= unionPayload True (b ^. blockMpc)

mpcRollback :: NewestFirst NE (Block SscGodTossing) -> GSUpdate ()
mpcRollback (NewestFirst blocks) = do
    -- Rollback certs
    let eos = (NE.last blocks) ^. epochOrSlotG
    rollbackCerts eos
    -- Rollback other payload
    wasGenesis <- foldM foldStep False blocks
    when wasGenesis resetGS
  where
    foldStep wasGen b
        | wasGen = pure wasGen
        | otherwise = differenceBlock b

    rollbackCerts :: EpochOrSlot -> GSUpdate ()
    rollbackCerts (EpochOrSlot (Left (EpochIndex 0))) =
        gsVssCertificates .= unionGenCerts
    rollbackCerts (EpochOrSlot (Left ei)) =
        gsVssCertificates %= VCD.setLastKnownSlot (SlotId (ei - 1) (epochSlots - 1))
    rollbackCerts (EpochOrSlot (Right (SlotId e 0))) =
        gsVssCertificates %= VCD.setLastKnownEoS (EpochOrSlot $ Left e)
    rollbackCerts (EpochOrSlot (Right (SlotId e s))) =
        gsVssCertificates %= VCD.setLastKnownSlot (SlotId e (s - 1))

    differenceBlock :: Block SscGodTossing -> GSUpdate Bool
    differenceBlock (Left _) = pure True
    differenceBlock (Right b) = do
        let payload = b ^. blockMpc
        case payload of
            CommitmentsPayload comms _ ->
                gsCommitments %= (`diffCommMap` comms)
            OpeningsPayload opens _ -> gsOpenings %= (`HM.difference` opens)
            SharesPayload shares _ -> gsShares %= (`HM.difference` shares)
            CertificatesPayload _ -> return ()
        pure False
    unionGenCerts = foldr' VCD.insert VCD.empty $ toList genesisCertificates

mpcLoadGlobalState
    :: (WithLogger m, MonadDB SscGodTossing m)
    => m GtGlobalState
mpcLoadGlobalState = do
    tipBlockHeader <- getTipBlockHeader
    let endEpoch  = epochOrSlot identity siEpoch $ tipBlockHeader ^. epochOrSlotG
    let startEpoch = safeSub endEpoch -- load blocks while >= endEpoch
        whileEpoch b = b ^. epochIndexL >= startEpoch
    logDebug $ sformat ("mpcLoadGlobalState: start epoch is "%build) startEpoch
    nfBlocks <- fmap fst <$> loadBlundsFromTipWhile whileEpoch
    blocks <- toOldestFirst <$>
                  maybeThrow (DBMalformed "No blocks during mpc load global state")
                             (_Wrapped NE.nonEmpty nfBlocks)
    let initGState
            | startEpoch == 0 =
                over gsVssCertificates unionGenCerts def
            | otherwise = def
    res <- execStateT (mapM_ gtApplyBlock blocks) initGState
    res <$ (logInfo $ sformat ("Loaded GodTossing state: "%build) res)
  where
    safeSub epoch = epoch - min epoch vssMaxTTL
    unionGenCerts gs = foldr' VCD.insert gs . toList $ genesisCertificates

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------
resetGS :: GSUpdate ()
resetGS = do
    gsCommitments .= mempty
    gsOpenings    .= mempty
    gsShares      .= mempty

unionPayload :: Bool -> GtPayload -> GtGlobalState -> GtGlobalState
unionPayload considerCertificates payload gs =
    flip execState gs $ do
        case payload of
            CommitmentsPayload comms _ -> gsCommitments <>= comms
            OpeningsPayload opens _    -> gsOpenings <>= opens
            SharesPayload shares _     -> gsShares <>= shares
            CertificatesPayload _      -> pure ()
        when considerCertificates $
            gsVssCertificates %=
                flip (foldr' VCD.insert)
                     (toList $ _gpCertificates payload)
