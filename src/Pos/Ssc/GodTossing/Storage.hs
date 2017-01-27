{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Instance of SscStorageClass.

module Pos.Ssc.GodTossing.Storage
       ( -- * Instances
         -- ** instance SscStorageClass SscGodTossing
         getGlobalCerts
       , gtGetGlobalState
       , getStableCerts
       ) where

import           Control.Lens                   ((%=), (.=), (<>=), _Wrapped)
import           Control.Monad.Except           (MonadError (throwError), runExceptT)
import           Control.Monad.Reader           (ask)
import           Data.Default                   (def)
import qualified Data.HashMap.Strict            as HM
import qualified Data.HashSet                   as HS
import qualified Data.List.NonEmpty             as NE
import           Formatting                     (build, sformat, (%))
import           Serokell.Util.Text             (listJson)
import           Serokell.Util.Verify           (VerificationRes (..))
import           System.Wlog                    (WithLogger, logDebug, logInfo)
import           Universum

import           Pos.Binary.Ssc                 ()
import           Pos.Constants                  (epochSlots, vssMaxTTL)
import           Pos.DB                         (DBError (DBMalformed), MonadDB,
                                                 getTipBlockHeader,
                                                 loadBlundsFromTipWhile)
import           Pos.Lrc.Types                  (Richmen)
import           Pos.Ssc.Class.Storage          (SscStorageClass (..))
import           Pos.Ssc.Class.Types            (Ssc (..))
import           Pos.Ssc.Extra.MonadGS          (MonadSscGS (..), sscRunGlobalQuery)
import           Pos.Ssc.GodTossing.Error       (SeedError)
import           Pos.Ssc.GodTossing.Functions   (checkCommShares,
                                                 checkOpeningMatchesCommitment,
                                                 checkShares, computeParticipants,
                                                 getStableCertsPure, isCommitmentIdx,
                                                 isOpeningIdx, isSharesIdx,
                                                 verifyGtPayload)
import           Pos.Ssc.GodTossing.Genesis     (genesisCertificates)
import           Pos.Ssc.GodTossing.Seed        (calculateSeed)
import           Pos.Ssc.GodTossing.Types       (GtGlobalState (..), GtPayload (..),
                                                 SscGodTossing, TossVerErrorTag (..),
                                                 TossVerFailure (..), VssCertificatesMap,
                                                 gsCommitments, gsOpenings, gsShares,
                                                 gsVssCertificates, vcVssKey,
                                                 _gpCertificates)
import           Pos.Ssc.GodTossing.Types.Base  (VssCertificate (..))
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Types                      (Block, EpochIndex (..), EpochOrSlot (..),
                                                 SharedSeed, SlotId (..), StakeholderId,
                                                 addressHash, blockMpc, blockSlot,
                                                 epochIndexL, epochOrSlot, epochOrSlotG,
                                                 gbHeader)
import           Pos.Util                       (NE, NewestFirst (..), OldestFirst (..),
                                                 maybeThrow, toOldestFirst)

type GSVerify a = forall m . ( MonadReader GtGlobalState m, WithLogger m
                             , MonadError TossVerFailure m) => m a
type GSQuery a  = forall m . (MonadReader GtGlobalState m, WithLogger m) => m a
type GSUpdate a = forall m . (MonadState GtGlobalState m) => m a

instance SscStorageClass SscGodTossing where
    sscLoadGlobalState = mpcLoadGlobalState
    sscApplyBlocksM = mpcApplyBlocks
    sscRollbackM = mpcRollback
    sscVerifyBlocksM pureVer rich = runExceptT . mpcVerifyBlocks pureVer rich
    sscCalculateSeedM = calculateSeedQ

gtGetGlobalState
    :: (MonadSscGS SscGodTossing m)
    => m GtGlobalState
gtGetGlobalState = sscRunGlobalQuery ask

getGlobalCerts
    :: (MonadSscGS SscGodTossing m)
    => SlotId -> m VssCertificatesMap
getGlobalCerts sl =
    sscRunGlobalQuery $
        VCD.certs .
        VCD.setLastKnownSlot sl <$>
        view (gsVssCertificates)

-- | Get stable VSS certificates for given epoch.
getStableCerts :: MonadSscGS SscGodTossing m => EpochIndex -> m VssCertificatesMap
getStableCerts epoch =
    getStableCertsPure epoch <$> sscRunGlobalQuery (view gsVssCertificates)

-- | Verify that if one adds given block to the current chain, it will
-- remain consistent with respect to SSC-related data.
mpcVerifyBlock :: Bool -> Richmen -> Block SscGodTossing -> GSVerify ()
-- Genesis blocks don't have any SSC data.
mpcVerifyBlock _ _ (Left _) = pass
-- Main blocks have commitments, openings, shares and VSS
-- certificates.  We optionally (depending on verifyPure argument) use
-- verifyGtPayload to make the most general checks and also use global
-- data to make more checks using this data.
mpcVerifyBlock verifyPure richmen (Right b) = do
    let slot@SlotId{siSlot = slotId} = b ^. blockSlot
        payload      = b ^. blockMpc
        curEpoch = siEpoch $ b ^. blockSlot
        blockCerts = _gpCertificates payload
        richmenSet = HS.fromList $ NE.toList richmen

    globalCommitments <- view gsCommitments
    globalOpenings    <- view gsOpenings
    globalShares      <- view gsShares
    globalVCD         <- view gsVssCertificates
    let globalCerts   = VCD.certs globalVCD
    let stableCerts   = getStableCertsPure curEpoch globalVCD
    let participants  = computeParticipants richmen stableCerts
    let participantsVssKeys = map vcVssKey $ toList participants

    logDebug $ sformat ("Global certificates: "%listJson
                        %", stable certificates: "%listJson
                        %", richmen: "%listJson)
               globalCerts stableCerts richmen

    let isComm  = unless (isCommitmentIdx slotId) $ throwError $ NotCommitmentPhase slot
        isOpen  = unless (isOpeningIdx slotId) $ throwError $ NotOpeningPhase slot
        isShare = unless (isSharesIdx slotId) $ throwError $ NotSharesPhase slot

    -- For commitments we
    --   * check that committing node is participant, i. e. she is richman and
    --     her VSS certificate is one of stable certificates
    --   * check that the nodes haven't already sent their commitments before
    --     in some different block
    --   * every commitment owner has enough (mpc+delegated) stake
    let commChecks comms = do
            isComm
            exceptGuard CommitingNoParticipants
                (`HM.member` participants) (HM.keys comms)
            exceptGuard CommitmentAlreadySent
                (not . (`HM.member` globalCommitments)) (HM.keys comms)
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
                (`HM.member` globalCommitments) (HM.keys opens)
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
                (`HM.member` globalCommitments) (concatMap HM.keys $ toList shares)
            exceptGuard SharesAlreadySent
                (not . (`HM.member` globalShares)) (HM.keys shares)
            exceptGuardEntry DecryptedSharesNotMatchCommitment
                (uncurry (checkShares globalCommitments globalOpenings participants))
                (HM.toList shares)

    let certChecks certs = do
            exceptGuard CertificateResubmitedEarly
                (not . (`HM.member` globalCerts)) (HM.keys certs)
            exceptGuardSnd CertificateNotRichmen
                ((`HS.member` richmenSet) . addressHash . vcSigningKey) (HM.toList certs)

    certChecks blockCerts
    case payload of
        CommitmentsPayload  comms  _ -> commChecks comms
        OpeningsPayload     opens  _ -> openChecks opens
        SharesPayload       shares _ -> shareChecks shares
        CertificatesPayload        _ -> pass

    when verifyPure $ case verifyGtPayload (b ^. gbHeader) payload of
        VerFailure errors -> throwError $ VerifyPureFailed errors
        _                 -> pass
  where
    -- This function is needed because gromak zadissil me and I was forced to write it.
    -- It takes list of entries ([(StakeholderId, v)] or [StakeholderId]),
    -- function condition and error tag (fKey and fValue - see below)
    -- If condition is true for every entry - function does nothing.
    -- Otherwise it gets all entries which don't pass condition
    -- and throwError with [StakeholderId] corresponding to these entries.
    -- fKey is needed for getting StakeholderId from entry.
    -- fValue is needed for getting value which must be tested by condition function.

    exceptGuardFull
        :: (entry -> StakeholderId)
        -> (entry -> val)
        -> TossVerErrorTag
        -> (val -> Bool)
        -> [entry]
        -> GSVerify ()
    exceptGuardFull fKey fVal tag cond =
        maybeThrowError (TossVerFailure tag) .
        NE.nonEmpty .
        map fKey .
        filter (not . cond . fVal)

    exceptGuard = exceptGuardFull identity identity

    exceptGuardSnd = exceptGuardFull fst snd

    exceptGuardEntry = exceptGuardFull fst identity

    maybeThrowError _ Nothing    = pass
    maybeThrowError er (Just ne) = throwError $ er ne

mpcVerifyBlocks
    :: Bool
    -> Richmen
    -> OldestFirst NE (Block SscGodTossing)
    -> GSVerify ()
mpcVerifyBlocks verifyPure richmen blocks = do
    curState <- ask
    flip evalStateT curState $ do
        forM_ blocks $ \b -> do
            mpcVerifyBlock verifyPure richmen b
            mpcProcessBlock b

-- | Apply sequence of blocks to state. Sequence must be based on last
-- applied block and must be valid.
mpcApplyBlocks :: OldestFirst NE (Block SscGodTossing) -> GSUpdate ()
mpcApplyBlocks = mapM_ mpcProcessBlock

mpcProcessBlock
    :: (SscPayload ssc ~ GtPayload)
    => Block ssc -> GSUpdate ()
mpcProcessBlock blk = do
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
        gsVssCertificates .= unionCerts genesisCertificates
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
                gsCommitments %= (`HM.difference` comms)
            OpeningsPayload opens _ -> gsOpenings %= (`HM.difference` opens)
            SharesPayload shares _ -> gsShares %= (`HM.difference` shares)
            CertificatesPayload _ -> return ()
        pure False
    unionCerts = (foldl' (flip $ uncurry VCD.insert)) VCD.empty . HM.toList

-- | Calculate leaders for the next epoch.
calculateSeedQ :: EpochIndex -> GSQuery (Either SeedError SharedSeed)
calculateSeedQ _ =
    calculateSeed <$> view gsCommitments <*> view gsOpenings <*>
        view gsShares

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
    res <- execStateT (mpcApplyBlocks blocks) initGState
    res <$ (logInfo $ sformat ("Loaded GodTossing state: "%build) res)
  where
    safeSub epoch = epoch - min epoch vssMaxTTL
    unionGenCerts gs = foldl' (flip $ uncurry VCD.insert) gs . HM.toList $ genesisCertificates

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
                flip
                    (foldl' (flip $ uncurry VCD.insert))
                    (HM.toList $ _gpCertificates payload)
