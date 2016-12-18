{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Instance of SscStorageClass.

module Pos.Modern.Ssc.GodTossing.Storage.Storage
       ( -- * Instances
         -- ** instance SscStorageClass SscGodTossing
       ) where

import           Control.Lens                             (Lens', ix, preview, to, use,
                                                           view, (%=), (.=), (^.))
import           Control.Monad.Reader                     (ask)
import           Data.Default                             (def)
import qualified Data.HashMap.Strict                      as HM
import           Data.List                                (nub)
import           Data.List.NonEmpty                       (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty                       as NE
import           Data.SafeCopy                            (SafeCopy)
import           Data.Serialize                           (Serialize (..))
import           Serokell.Util.Verify                     (VerificationRes (..),
                                                           isVerSuccess, verifyGeneric)
import           Universum

import           Pos.Crypto                               (EncShare, Threshold,
                                                           VssPublicKey)
import           Pos.FollowTheSatoshi                     (followTheSatoshi)
import           Pos.Modern.Ssc.GodTossing.Storage.Types  (GtGlobalState (..),
                                                           gsBlocksMpc, gsCommitments,
                                                           gsOpenings, gsShares,
                                                           gsVssCertificates)
import           Pos.Modern.Ssc.GodTossing.Types.Instance ()
import           Pos.Ssc.Class.Storage                    (HasSscStorage (..),
                                                           MonadSscGS (..), SscQuery,
                                                           SscStorageClassM (..),
                                                           SscUpdate, sscRunGlobalModify,
                                                           sscRunGlobalQuery)
import           Pos.Ssc.Class.Types                      (Ssc (..))
import           Pos.Ssc.GodTossing.Error                 (SeedError)
import           Pos.Ssc.GodTossing.Functions             (checkOpeningMatchesCommitment,
                                                           checkShares, isCommitmentIdx,
                                                           isOpeningIdx, isSharesIdx,
                                                           verifyGtPayload)
import           Pos.Ssc.GodTossing.Seed                  (calculateSeed)
import           Pos.Ssc.GodTossing.Types.Base            (Commitment (..),
                                                           VssCertificate (..))
import           Pos.Ssc.GodTossing.Types.Type            (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types           (GtPayload (..), SscBi,
                                                           emptyPayload, _gpCertificates)
import           Pos.State.Storage.Types                  (AltChain)
import           Pos.Types                                (Address (..), Block,
                                                           EpochIndex, SlotId (..),
                                                           SlotLeaders, Utxo, blockMpc,
                                                           blockSlot, blockSlot, gbHeader,
                                                           txOutAddress)
import           Pos.Util                                 (AsBinary, magnify', neFromList,
                                                           readerToState, zoom', _neHead)
type GSQuery a  = forall m . Monad m => ReaderT GtGlobalState m a
type GSUpdate a = forall m . Monad m => StateT GtGlobalState m a

instance (SscBi, Monad m, MonadSscGS SscGodTossing m
         , SscPayload SscGodTossing ~ GtPayload
         , SscGlobalState SscGodTossing ~ GtGlobalState)
         => SscStorageClassM SscGodTossing m where
    sscApplyBlocksM = sscRunGlobalModify . mpcApplyBlocks
    sscRollbackM = sscRunGlobalModify . mpcRollback
    --getGlobalStateM = sscRunGlobalQuery $ getGlobalMpcData
    --sscGetGlobalStateByDepth = getGlobalMpcDataByDepth
    sscVerifyBlocksM _ = sscRunGlobalQuery . mpcVerifyBlocks
    -- onNewSlot - we need it for mpcBlocks

    -- sscGetOurShares = getOurShares
    -- sscGetParticipants = getParticipants
    -- sscCalculateLeadersM = notImplemented

getGlobalMpcData :: GSQuery GtGlobalState
getGlobalMpcData =
    fromMaybe (panic "No global SSC payload for depth 0") <$>
    getGlobalMpcDataByDepth 0

-- TODO: check for off-by-one errors!!!!111
--
-- specifically, I'm not sure whether versioning here and versioning in .Tx
-- are the same versionings
getGlobalMpcDataByDepth :: Word -> GSQuery (Maybe GtGlobalState)
getGlobalMpcDataByDepth (fromIntegral -> depth) =
    Just . execState (mpcRollback depth) <$> ask

-- | Get keys of nodes participating in an epoch. A node participates if,
-- when there were 'k' slots left before the end of the previous epoch, both
-- of these were true:
--
--   1. It was a stakeholder.
--   2. It had already sent us its VSS key by that time.
getParticipants :: Word -> Utxo -> GSQuery (Maybe (NonEmpty (AsBinary VssPublicKey)))
getParticipants depth utxo = do
    mKeymap <- fmap _gsVssCertificates <$> getGlobalMpcDataByDepth depth  -- it is verifiedVssCertificates
    return $
        do keymap <- mKeymap
           let stakeholders =
                   nub $ map txOutAddress (toList utxo)
           NE.nonEmpty $
               map vcVssKey $ mapMaybe (`HM.lookup` keymap) stakeholders

-- | Calculate leaders for the next epoch.
calculateLeaders
    :: EpochIndex
    -> Utxo            -- ^ Utxo (k slots before the end of epoch)
    -> Threshold
    -> GSQuery (Either SeedError SlotLeaders)
calculateLeaders _ utxo threshold = do --GodTossing doesn't use epoch, but NistBeacon use it
    mbSeed <- calculateSeed threshold
                            <$> view gsCommitments
                            <*> view gsOpenings
                            <*> view gsShares
    return $ case mbSeed of
        Left e     -> Left e
        Right seed -> Right $ followTheSatoshi seed utxo

-- | Verify that if one adds given block to the current chain, it will
-- remain consistent with respect to SSC-related data.
mpcVerifyBlock
    :: forall ssc . (SscPayload ssc ~ GtPayload, SscBi)
    => Block ssc -> GSQuery VerificationRes
-- Genesis blocks don't have any SSC data.
mpcVerifyBlock (Left _) = return VerSuccess
-- Main blocks have commitments, openings, shares and VSS certificates.
-- We use verifyGtPayload to make the most general checks and also use
-- global data to make more checks using this data.
mpcVerifyBlock (Right b) = do
    let SlotId{siSlot = slotId} = b ^. blockSlot
    let payload      = b ^. blockMpc

    globalCommitments  <- view gsCommitments
    globalOpenings     <- view gsOpenings
    globalShares       <- view gsShares
    globalCertificates <- view gsVssCertificates

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
            --We intentionally don't check, that nodes which decrypted shares
            --sent its commitments.
            --If node decrypted shares correctly, such node is useful for us, despite of
            --it didn't send its commitment.
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

    let ourRes = verifyGeneric $
            case payload of
                CommitmentsPayload comms certs -> commChecks comms certs
                OpeningsPayload        opens _ -> openChecks opens
                SharesPayload         shares _ -> shareChecks shares
                CertificatesPayload          _ -> []
    return (verifyGtPayload @ssc (b ^. gbHeader) payload <> ourRes)

-- TODO:
--   ★ verification messages should include block hash/slotId
--   ★ we should stop at first failing block
mpcVerifyBlocks :: (SscBi, SscPayload SscGodTossing ~ GtPayload)
                => AltChain SscGodTossing -> GSQuery VerificationRes
mpcVerifyBlocks  blocks = do
    curState <- ask
    return $ flip evalState curState $ do
        vs <- forM blocks $ \b -> do
            v <- readerToState $ mpcVerifyBlock b
            when (isVerSuccess v) $
                mpcProcessBlock b
            return v
        return (fold vs)

-- | Apply sequence of blocks to state. Sequence must be based on last
-- applied block and must be valid.
mpcApplyBlocks
    :: forall ssc . SscPayload ssc ~ GtPayload
    => AltChain ssc -> GSUpdate ()
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
        Left _ -> do
            gsCommitments .= mempty
            gsOpenings    .= mempty
            gsShares      .= mempty
            gsBlocksMpc   %= prependNShift emptyPayload
        -- Main blocks contain commitments, openings, shares, VSS certificates
        Right b -> unionPayload (b ^. blockMpc)
  where
    unionPayload payload = do
        let blockCertificates = _gpCertificates payload
        gsBlocksMpc %= prependNShift payload
        case payload of
            CommitmentsPayload comms _ ->
                gsCommitments %= HM.union comms
            OpeningsPayload    opens _ ->
                gsOpenings %= HM.union opens
            SharesPayload     shares _ ->
                gsShares %= HM.unionWith HM.union shares
            CertificatesPayload      _ -> pure ()
        gsVssCertificates %= HM.union blockCertificates
    prependNShift payload = neFromList . NE.init . NE.cons payload

mpcRollback :: Word -> GSUpdate ()
mpcRollback (fromIntegral -> n) = replicateM_ n rollbackLast
  where
    rollbackLast = do
        payload <- use (gsBlocksMpc . _neHead)
        let payloadCertificates = _gpCertificates payload
        case payload of
            CommitmentsPayload comms _ ->
                gsCommitments %= (`HM.difference` comms)
            OpeningsPayload    opens _ ->
                gsOpenings %= (`HM.difference` opens)
            SharesPayload     shares _ ->
                gsShares %= (`HM.difference` shares)
            CertificatesPayload      _ -> return ()
        gsVssCertificates %= flip HM.difference payloadCertificates
        gsBlocksMpc %= NE.fromList . NE.tail


-- | Decrypt shares (in commitments) that we can decrypt.
-- getOurShares
--     :: AsBinary VssPublicKey                           -- ^ Our VSS key
--     -> Query (HashMap Address (AsBinary EncShare))
-- getOurShares ourPK = do
--     comms <- view (lastVer . dsGlobalCommitments)
--     opens <- view (lastVer . dsGlobalOpenings)
--     return .
--         HM.fromList . catMaybes $
--             flip fmap (HM.toList comms) $ \(theirAddr, (_, Commitment{..}, _)) ->
--                 if not $ HM.member theirAddr opens
--                    then (,) theirAddr <$> HM.lookup ourPK commShares
--                    else Nothing -- if we have opening for theirAddr, we shouldn't send shares for it
