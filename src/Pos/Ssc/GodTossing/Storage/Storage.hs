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
{-# LANGUAGE ViewPatterns          #-}

-- | Instance of SscStorageClass.

module Pos.Ssc.GodTossing.Storage.Storage
       ( -- * Instances
         -- ** instance SscStorageClass SscGodTossing
       ) where

import           Control.Lens                      (Lens', ix, preview, to, use, view,
                                                    (%=), (.=), (^.))
import           Crypto.Random                     (drgNewSeed, seedFromInteger, withDRG)
import           Data.Default                      (def)
import qualified Data.HashMap.Strict               as HM
import           Data.List                         (nub)
import           Data.List.NonEmpty                (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                as NE
import           Data.SafeCopy                     (SafeCopy)
import           Data.Serialize                    (Serialize (..))
import           Data.Tagged                       (Tagged (..))
import           Serokell.Util.Verify              (VerificationRes (..), isVerSuccess,
                                                    verifyGeneric)
import           Universum

import           Pos.Crypto                        (Share, Signed (signedValue),
                                                    Threshold, VssKeyPair, VssPublicKey,
                                                    decryptShare, toVssPublicKey)
import           Pos.Crypto                        (PublicKey)
import           Pos.FollowTheSatoshi              (followTheSatoshi)
import           Pos.Ssc.Class.Storage             (HasSscStorage (..), SscQuery,
                                                    SscStorageClass (..), SscUpdate)
import           Pos.Ssc.Class.Types               (Ssc (..))
import           Pos.Ssc.GodTossing.Error          (SeedError)
import           Pos.Ssc.GodTossing.Functions      (checkOpeningMatchesCommitment,
                                                    checkShares, isCommitmentIdx,
                                                    isOpeningIdx, isSharesIdx,
                                                    verifyGtPayload)
import           Pos.Ssc.GodTossing.Seed           (calculateSeed)
import           Pos.Ssc.GodTossing.Storage.Types  (GtStorage, GtStorageVersion (..),
                                                    dsGlobalCertificates,
                                                    dsGlobalCommitments, dsGlobalOpenings,
                                                    dsGlobalShares, dsVersionedL)
import           Pos.Ssc.GodTossing.Types.Base     (Commitment (..))
import           Pos.Ssc.GodTossing.Types.Instance ()
import           Pos.Ssc.GodTossing.Types.Type     (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Types    (GtGlobalState (..), GtPayload (..),
                                                    _gpCertificates)
import           Pos.State.Storage.Types           (AltChain)
import           Pos.Types                         (Address (getAddress), Block,
                                                    EpochIndex, SlotId (..), SlotLeaders,
                                                    Utxo, blockMpc, blockSlot, blockSlot,
                                                    gbHeader, txOutAddress)
import           Pos.Util                          (magnify', readerToState, zoom',
                                                    _neHead)

-- acid-state requires this instance because of a bug
instance SafeCopy SscGodTossing
instance Serialize SscGodTossing where
    put = panic "put@SscGodTossing: can't happen"
    get = panic "get@SscGodTossing: can't happen"

instance SscStorageClass SscGodTossing where
    sscApplyBlocks = mpcApplyBlocks
    sscRollback = mpcRollback
    sscGetGlobalPayload = getGlobalMpcData
    sscGetGlobalPayloadByDepth = getGlobalMpcDataByDepth
    sscVerifyBlocks = mpcVerifyBlocks

    sscGetOurShares = getOurShares

    sscGetParticipants = getParticipants
    sscCalculateLeaders = calculateLeaders

    sscVerifyPayload = Tagged verifyGtPayload

type Query a = SscQuery SscGodTossing a
type Update a = SscUpdate SscGodTossing a

instance (SscStorage ssc ~ GtStorage) => HasSscStorage ssc GtStorage where
    sscStorage = identity

dsVersioned
    :: HasSscStorage SscGodTossing a =>
       Lens' a (NonEmpty GtStorageVersion)
dsVersioned = sscStorage @SscGodTossing . dsVersionedL

-- | A lens to access the last version of GtStorage
lastVer :: HasSscStorage SscGodTossing a => Lens' a GtStorageVersion
lastVer = dsVersioned . _neHead

getGlobalMpcData :: Query GtGlobalState
getGlobalMpcData =
    fromMaybe (panic "No global SSC payload for depth 0") <$>
    getGlobalMpcDataByDepth 0

-- TODO: check for off-by-one errors!!!!111
--
-- specifically, I'm not sure whether versioning here and versioning in .Tx
-- are the same versionings
getGlobalMpcDataByDepth :: Word -> Query (Maybe GtGlobalState)
getGlobalMpcDataByDepth (fromIntegral -> depth) =
    preview $ dsVersioned . ix depth . to mkGlobalMpcData
  where
    mkGlobalMpcData GtStorageVersion {..} =
        GtGlobalState
        { _gsCommitments = _dsGlobalCommitments
        , _gsOpenings = _dsGlobalOpenings
        , _gsShares = _dsGlobalShares
        , _gsVssCertificates = _dsGlobalCertificates
        }

-- | Get keys of nodes participating in an epoch. A node participates if,
-- when there were 'k' slots left before the end of the previous epoch, both
-- of these were true:
--
--   1. It was a stakeholder.
--   2. It had already sent us its VSS key by that time.
getParticipants :: Word -> Utxo -> Query (Maybe (NonEmpty VssPublicKey))
getParticipants depth utxo = do
    mKeymap <- fmap _gsVssCertificates <$> getGlobalMpcDataByDepth depth
    return $
        do keymap <- mKeymap
           let stakeholders =
                   nub $ map (getAddress . txOutAddress) (toList utxo)
           NE.nonEmpty $
               map signedValue $ mapMaybe (`HM.lookup` keymap) stakeholders

-- | Calculate leaders for the next epoch.
calculateLeaders
    :: EpochIndex
    -> Utxo            -- ^ Utxo (k slots before the end of epoch)
    -> Threshold
    -> Query (Either SeedError SlotLeaders)
calculateLeaders _ utxo threshold = do --GodTossing doesn't use epoch, but NistBeacon use it
    mbSeed <- calculateSeed threshold
                            <$> view (lastVer . dsGlobalCommitments)
                            <*> view (lastVer . dsGlobalOpenings)
                            <*> view (lastVer . dsGlobalShares)
    return $ case mbSeed of
        Left e     -> Left e
        Right seed -> Right $ fmap getAddress $ followTheSatoshi seed utxo

-- -- Apply checkShares using last version.
-- checkSharesLastVer :: PublicKey -> HashMap PublicKey Share -> Query Bool
-- checkSharesLastVer pk shares =
--     magnify' lastVer $
--     (\comms openings certs -> checkShares comms openings certs pk shares) <$>
--     view dsGlobalCommitments <*>
--     view dsGlobalOpenings <*>
--     view dsGlobalCertificates

-- | Verify that if one adds given block to the current chain, it will
-- remain consistent with respect to SSC-related data.
mpcVerifyBlock
    :: forall ssc . (SscPayload ssc ~ GtPayload)
    => Block ssc -> Query VerificationRes
-- Genesis blocks don't have any SSC data.
mpcVerifyBlock (Left _) = return VerSuccess
-- Main blocks have commitments, openings, shares and VSS certificates.
-- We use verifyGtPayload to make the most general checks and also use
-- global data to make more checks using this data.
mpcVerifyBlock (Right b) = magnify' lastVer $ do
    let SlotId{siSlot = slotId} = b ^. blockSlot
    let payload      = b ^. blockMpc

    globalCommitments  <- view dsGlobalCommitments
    globalOpenings     <- view dsGlobalOpenings
    globalShares       <- view dsGlobalShares
    globalCertificates <- view dsGlobalCertificates

    let isComm       = (isCommitmentIdx slotId, "slotId doesn't belong commitment phase")
        isOpen       = (isOpeningIdx slotId, "slotId doesn't belong openings phase")
        isShare      = (isSharesIdx slotId, "slotId doesn't belong share phase")
    -- For commitments we
    --   * check that the nodes haven't already sent their commitments before
    --     in some different block
    --   * check that a VSS certificate is present for the committing node
    -- TODO: we might also check that all share IDs are different, because
    -- then we would be able to simplify 'calculateSeed' a bit – however,
    -- it's somewhat complicated because we have encrypted shares, shares in
    -- commitments, etc.
    let commChecks comms certs =
            [ isComm
            , (all (`HM.member` (certs <> globalCertificates))
                   (HM.keys comms),
                   "some committing nodes haven't sent a VSS certificate")
            , (all (not . (`HM.member` globalCommitments))
                   (HM.keys comms),
                   "some nodes have already sent their commitments")
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
            , (all (`HM.member` globalCommitments)
                   (HM.keys shares <> concatMap HM.keys (toList shares)),
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
mpcVerifyBlocks :: Word -> AltChain SscGodTossing -> Query VerificationRes
mpcVerifyBlocks toRollback blocks = do
    curState <- view (sscStorage @SscGodTossing)
    return $ flip evalState curState $ do
        mpcRollback toRollback
        vs <- forM blocks $ \b -> do
            v <- readerToState $ mpcVerifyBlock b
            when (isVerSuccess v) $
                mpcProcessBlock b
            return v
        return (fold vs)

-- | Apply sequence of blocks to state. Sequence must be based on last
-- applied block and must be valid.
mpcApplyBlocks
    :: (SscPayload ssc ~ GtPayload)
    => AltChain ssc -> Update ()
mpcApplyBlocks = mapM_ mpcProcessBlock

mpcRollback :: Word -> Update ()
mpcRollback (fromIntegral -> n) = do
    dsVersioned %= (fromMaybe (def :| []) . NE.nonEmpty . NE.drop n)

mpcProcessBlock
    :: (SscPayload ssc ~ GtPayload)
    => Block ssc -> Update ()
mpcProcessBlock blk = do
    lv <- use lastVer
    dsVersioned %= NE.cons lv
    case blk of
        -- Genesis blocks don't contain anything interesting, but when they
        -- “arrive”, we clear global commitments and other globals. Not
        -- certificates, though, because we don't want to make nodes resend
        -- them in each epoch.
        Left _ -> do
            zoom' lastVer $ do
                dsGlobalCommitments .= mempty
                dsGlobalOpenings    .= mempty
                dsGlobalShares      .= mempty
        -- Main blocks contain commitments, openings, shares, VSS certificates
        Right b -> do
            let payload = b ^. blockMpc
                blockCertificates = _gpCertificates payload
            zoom' lastVer $ do
                case payload of
                    CommitmentsPayload comms _ ->
                        -- commitments
                        dsGlobalCommitments %= HM.union comms
                    OpeningsPayload    opens _ ->
                        -- openings
                        dsGlobalOpenings %= HM.union opens
                    SharesPayload     shares _ ->
                        -- shares
                        dsGlobalShares %= HM.unionWith HM.union shares
                    CertificatesPayload      _ -> return ()
                -- VSS certificates
                dsGlobalCertificates %= HM.union blockCertificates

-- | Decrypt shares (in commitments) that we can decrypt.
getOurShares
    :: VssKeyPair                           -- ^ Our VSS key
    -> Integer                              -- ^ Random generator seed
                                            -- (needed for 'decryptShare')
    -> Query (HashMap PublicKey Share)
getOurShares ourKey seed = do
    let drg = drgNewSeed (seedFromInteger seed)
    comms <- view (lastVer . dsGlobalCommitments)
    opens <- view (lastVer . dsGlobalOpenings)
    let ourPK = toVssPublicKey ourKey
    return $ fst $ withDRG drg $
        fmap (HM.fromList . catMaybes) $
            forM (HM.toList comms) $ \(theirPK, (Commitment{..}, _)) -> do
                if not $ HM.member theirPK opens then do
                    let mbEncShare = HM.lookup ourPK commShares
                    case mbEncShare of
                        Nothing       -> return Nothing
                        Just encShare -> Just . (theirPK,) <$>
                                        decryptShare ourKey encShare
                 else return Nothing -- if we have opening for theirPK, we shouldn't send shares for it
