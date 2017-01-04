{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Instance of SscStorageClass.

module Pos.Ssc.GodTossing.Storage
       ( -- * Instances
         -- ** instance SscStorageClass SscGodTossing
         getGlobalCerts
       , getVerifiedCerts
       , gtGetGlobalState
       ) where

import           Control.Lens                   (over, use, view, views, (%=), (.=), (^.),
                                                 _1, _2)
import           Control.Monad.IfElse           (whileM)
import           Control.Monad.Reader           (ask)
import           Data.Default                   (def)
import qualified Data.HashMap.Strict            as HM
import           Data.List.NonEmpty             (nonEmpty)
import qualified Data.List.NonEmpty             as NE
import           Serokell.Util.Verify           (VerificationRes (..), isVerSuccess,
                                                 verifyGeneric)
import           Universum

import           Pos.Binary.Ssc                 ()
import           Pos.Constants                  (k)
import           Pos.Context.Class              (readRichmen)
import           Pos.DB                         (MonadDB, getBlock)
import           Pos.Ssc.Class.Storage          (SscImpureQuery, SscStorageClass (..))
import           Pos.Ssc.Class.Types            (Ssc (..))
import           Pos.Ssc.Extra.MonadGS          (MonadSscGS (..), sscRunGlobalQuery)
import           Pos.Ssc.GodTossing.Error       (SeedError)
import           Pos.Ssc.GodTossing.Functions   (checkOpeningMatchesCommitment,
                                                 checkShares, getThreshold,
                                                 isCommitmentIdx, isOpeningIdx,
                                                 isSharesIdx, verifyGtPayload)
import           Pos.Ssc.GodTossing.Genesis     (genesisCertificates)
import           Pos.Ssc.GodTossing.Seed        (calculateSeed)
import           Pos.Ssc.GodTossing.Types       (GtGlobalState (..), GtPayload (..),
                                                 SscGodTossing, VssCertificatesMap,
                                                 gsCommitments, gsOpenings, gsShares,
                                                 gsVssCertificates, vcVssKey,
                                                 _gpCertificates)
import           Pos.Ssc.GodTossing.Types.Base  (VssCertificate (..))
import qualified Pos.Ssc.GodTossing.VssCertData as VCD
import           Pos.Types                      (Block, EpochIndex, HeaderHash, NEBlocks,
                                                 SharedSeed, SlotId (..), blockMpc,
                                                 blockSlot, epochIndexL, epochOrSlot,
                                                 epochOrSlotG, gbHeader, prevBlockL,
                                                 subSlotSafe)
import           Pos.Util                       (readerToState)

type GSQuery a  = forall m . (MonadReader GtGlobalState m) => m a
type GSUpdate a = forall m . (MonadState GtGlobalState m) => m a
type GSImpureQuery a = SscImpureQuery SscGodTossing a

instance SscStorageClass SscGodTossing where
    sscLoadGlobalState = mpcLoadGlobalState
    sscApplyBlocksM = mpcApplyBlocks
    sscRollbackM = mpcRollback
    sscVerifyBlocksM = mpcVerifyBlocks
    sscCalculateSeedM = calculateSeedQ

gtGetGlobalState
    :: (MonadSscGS SscGodTossing m)
    => m GtGlobalState
gtGetGlobalState = sscRunGlobalQuery ask

getGlobalCerts
    :: (MonadSscGS SscGodTossing m)
    => m VssCertificatesMap
getGlobalCerts = sscRunGlobalQuery $ VCD.certs <$> view gsVssCertificates

-- | Verified certs for slotId
getVerifiedCerts :: (MonadSscGS SscGodTossing m) => SlotId -> m VssCertificatesMap
getVerifiedCerts (flip subSlotSafe k -> slotId) = sscRunGlobalQuery doIt
  where
    doIt = do
        VCD.certs . VCD.setLastKnownSlot slotId <$> view gsVssCertificates

-- | Verify that if one adds given block to the current chain, it will
-- remain consistent with respect to SSC-related data.
mpcVerifyBlock :: Bool -> Block SscGodTossing -> GSQuery VerificationRes
-- Genesis blocks don't have any SSC data.
mpcVerifyBlock _ (Left _) = return VerSuccess
-- Main blocks have commitments, openings, shares and VSS
-- certificates.  We optionally (depending on verifyPure argument) use
-- verifyGtPayload to make the most general checks and also use global
-- data to make more checks using this data.
mpcVerifyBlock verifyPure (Right b) = do
    let SlotId{siSlot = slotId} = b ^. blockSlot
        payload      = b ^. blockMpc
        curEpoch = siEpoch $ b ^. blockSlot
        blockCerts = _gpCertificates payload

    globalCommitments  <- view gsCommitments
    globalOpenings     <- view gsOpenings
    globalShares       <- view gsShares
    globalCertificates <- views gsVssCertificates VCD.certs

    let isComm       = (isCommitmentIdx slotId, "slotId doesn't belong commitment phase")
        isOpen       = (isOpeningIdx slotId, "slotId doesn't belong openings phase")
        isShare      = (isSharesIdx slotId, "slotId doesn't belong share phase")
    -- For commitments we
    --   * check that the nodes haven't already sent their commitments before
    --     in some different block
    --   * check that a VSS certificate is present for the committing node
    let commChecks comms certs =
            [ isComm
            , (all (`HM.member` (certs <> globalCertificates))
                   (HM.keys comms),
                   "some committing nodes haven't sent a VSS certificate")
            , (all (not . (`HM.member` globalCommitments))
                   (HM.keys comms),
                   "some nodes have already sent their commitments")
            -- [CSL-206]: check that share IDs are different.
            ]

    -- For openings, we check that
    --   * the opening isn't present in previous blocks
    --   * corresponding commitment is present
    --   * the opening matches the commitment
    let openChecks opens =
            [ isOpen
            , (all (not . (`HM.member` globalOpenings))
                   (HM.keys opens),
                   "some nodes have already sent their openings")
            , (all (`HM.member` globalCommitments)
                   (HM.keys opens),
                   "some openings don't have corresponding commitments")
            , (all (checkOpeningMatchesCommitment globalCommitments) (HM.toList opens),
                   "some openings don't match corresponding commitments")
            ]

    -- For shares, we check that
    --   * shares have corresponding commitments
    --   * these shares weren't sent before
    --   * if encrypted shares (in commitments) are decrypted, they match
    --     decrypted shares
    -- We don't check whether shares match the openings.
    let shareChecks shares =
            [ isShare
            -- We intentionally don't check, that nodes which decrypted shares
            -- sent its commitments.
            -- If node decrypted shares correctly, such node is useful for us, despite of
            -- it didn't send its commitment.
            , (all (`HM.member` globalCommitments)
                   (concatMap HM.keys (toList shares)),
                   "some shares don't have corresponding commitments")
            -- [CSL-203]: here we assume that all shares are always sent as a
            -- whole package.
            -- Use intersectionDoubleMap (doesn't exist yet) or something to
            -- allow spliting shares into multiple messages.
            , (null (shares `HM.intersection` globalShares),
                   "some shares have already been sent")
            , (all (uncurry (checkShares globalCommitments globalOpenings
                             globalCertificates)) $
                     HM.toList shares,
                   "some decrypted shares don't match encrypted shares \
                   \in the corresponding commitment")
            ]
    let certChecks certs =
            [
              (all (maybe True
                          ((==) curEpoch . vcExpiryEpoch)
                          . flip HM.lookup globalCertificates
                   ) (HM.keys certs),
               "some VSS certificates have been resubmitted earlier than expiry epoch")
            ]

    let ourRes = verifyGeneric $ certChecks blockCerts ++
            case payload of
                CommitmentsPayload comms certs -> commChecks comms certs
                OpeningsPayload        opens _ -> openChecks opens
                SharesPayload         shares _ -> shareChecks shares
                CertificatesPayload          _ -> []
    let pureRes = if verifyPure
                  then verifyGtPayload (b ^. gbHeader) payload
                  else mempty
    return (pureRes <> ourRes)

-- TODO:
--   ★ verification messages should include block hash/slotId
--   ★ we should stop at first failing block
mpcVerifyBlocks :: Bool -> NEBlocks SscGodTossing -> GSQuery VerificationRes
mpcVerifyBlocks verifyPure blocks = do
    curState <- ask
    return $ flip evalState curState $ do
        vs <- forM blocks $ \b -> do
            v <- readerToState $ mpcVerifyBlock verifyPure b
            when (isVerSuccess v) $
                mpcProcessBlock b
            return v
        return (fold vs)

-- | Apply sequence of blocks to state. Sequence must be based on last
-- applied block and must be valid.
mpcApplyBlocks :: NEBlocks SscGodTossing -> GSUpdate ()
mpcApplyBlocks = mapM_ mpcProcessBlock

mpcProcessBlock
    :: (SscPayload ssc ~ GtPayload)
    => Block ssc -> GSUpdate ()
mpcProcessBlock blk = do
    case blk of
        -- Genesis blocks don't contain anything interesting, but when they
        -- “arrive”, we clear global commitments and other globals. Not
        -- certificates, though, because we don't want to make nodes resend
        -- them in each epoch.
        Left gb -> do
            let slot = SlotId (gb ^. epochIndexL) 0
            resetGS
            gsVssCertificates %= (VCD.setLastKnownSlot slot)
        -- Main blocks contain commitments, openings, shares, VSS certificates
        Right b -> do
            gsVssCertificates %= VCD.setLastKnownSlot (b ^. blockSlot)
            modify (unionPayload (b ^. blockMpc))

-- | Head - youngest
mpcRollback :: NEBlocks SscGodTossing -> GSUpdate ()
mpcRollback blocks = do
     -- is there guarantee that genesis block won't be passed to mpcRollback?
    let slot = prevSlot $ blkSlot $ NE.last blocks
    -- Rollback certs
    gsVssCertificates %= (VCD.setLastKnownSlot slot)
    -- Rollback other payload
    wasGenesis <- foldM (\wasGen b -> if wasGen then pure wasGen else differenceBlock b)
                         False blocks
    when wasGenesis resetGS
  where
    prevSlot SlotId{..} =
        if (siSlot == 0) then
            if siEpoch == 0 then panic "Genesis block passed to mpc rollback"
            else SlotId (siEpoch - 1) (2 * k - 1)
        else SlotId siEpoch (siSlot - 1)
    differenceBlock :: Block SscGodTossing -> GSUpdate Bool
    differenceBlock (Left _)  = pure True
    differenceBlock (Right b) = do
        let payload = b ^. blockMpc
        case payload of
            CommitmentsPayload comms _ ->
                gsCommitments %= (`HM.difference` comms)
            OpeningsPayload    opens _ ->
                gsOpenings %= (`HM.difference` opens)
            SharesPayload     shares _ ->
                gsShares %= (`HM.difference` shares)
            CertificatesPayload      _ -> return ()
        pure False

-- [CSL-364] This function has bug, see issue.
mpcLoadGlobalState :: MonadDB SscGodTossing m => HeaderHash SscGodTossing -> m GtGlobalState
mpcLoadGlobalState tip = do
    global <- fst <$> execStateT unionBlocks (def, tip)
    pure $ over gsVssCertificates
                (flip (foldl' (flip $ uncurry VCD.insert)) (HM.toList genesisCertificates))
                global

-- | Calculate leaders for the next epoch.
calculateSeedQ :: EpochIndex -> GSImpureQuery (Either SeedError SharedSeed)
calculateSeedQ _ = do
    richmen <- readRichmen
    keymap <- view gsVssCertificates
    let participants =
            nonEmpty $ map vcVssKey $ mapMaybe (`VCD.lookup` keymap) $ toList richmen
    let threshold = maybe (panic "No participants") (getThreshold . length) participants
    calculateSeed threshold <$> view gsCommitments <*> view gsOpenings <*>
        view gsShares

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------
resetGS :: GSUpdate ()
resetGS = do
    gsCommitments .= mempty
    gsOpenings    .= mempty
    gsShares      .= mempty

unionPayload :: GtPayload -> GtGlobalState -> GtGlobalState
unionPayload payload =
    execState (do
    let blockCertificates = _gpCertificates payload
    case payload of
        CommitmentsPayload comms _ ->
            gsCommitments %= HM.union comms
        OpeningsPayload    opens _ ->
            gsOpenings %= HM.union opens
        SharesPayload     shares _ ->
            gsShares %= HM.unionWith HM.union shares
        CertificatesPayload      _ -> pure ()
    gsVssCertificates %= flip (foldl' (flip $ uncurry VCD.insert)) (HM.toList blockCertificates))

-- | Union payloads of blocks until meet genesis block
-- Invalid restore of VSS certificates
unionBlocks :: MonadDB SscGodTossing m => StateT (GtGlobalState, HeaderHash SscGodTossing) m ()
unionBlocks = whileM
    (do
        curTip <- use _2
        block <- lift $ getBlock curTip
        let b = fromMaybe (panic "No block with such tip") block
        case b of
            Left _   -> pure False
            Right mb -> do
                _1 %= unionPayload (mb ^. blockMpc)
                True <$ (_2 .= b ^. prevBlockL)
    ) (pure ())

blkSlot :: Block ssc -> SlotId
blkSlot = epochOrSlot (flip SlotId 0) identity . (^. epochOrSlotG)
