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

module Pos.Ssc.GodTossing.Instance.Storage
       ( -- * Instances
         -- ** instance SscStorageClass SscGodTossing
       ) where

import           Control.Lens                      (Lens', at, ix, preview, to, use, view,
                                                    (%=), (.=), (.~), (^.))
import           Crypto.Random                     (drgNewSeed, seedFromInteger, withDRG)
import           Data.Default                      (def)
import qualified Data.HashMap.Strict               as HM
import           Data.List                         (nub)
import           Data.List.NonEmpty                (NonEmpty ((:|)), fromList)
import qualified Data.List.NonEmpty                as NE
import           Data.SafeCopy                     (SafeCopy)
import           Data.Serialize                    (Serialize (..))
import           Data.Tagged                       (Tagged (..))
import           Serokell.Util.Verify              (VerificationRes (..), isVerSuccess,
                                                    verifyGeneric)
import           Universum

import           Pos.Crypto                        (Share, Signed (signedValue),
                                                    Threshold, VssKeyPair, VssPublicKey,
                                                    decryptShare, toVssPublicKey,
                                                    verifyShare)
import           Pos.Crypto                        (PublicKey)
import           Pos.FollowTheSatoshi              (followTheSatoshi)
import           Pos.Ssc.Class.Storage             (HasSscStorage (..), SscQuery,
                                                    SscStorageClass (..), SscUpdate)
import           Pos.Ssc.Class.Types               (Ssc (..))
import           Pos.Ssc.GodTossing.Base           (Commitment (..), CommitmentSignature,
                                                    CommitmentsMap, OpeningsMap,
                                                    VssCertificate, VssCertificatesMap,
                                                    isCommitmentIdx, isOpeningIdx,
                                                    isSharesIdx, verifyOpening,
                                                    verifySignedCommitment)
import           Pos.Ssc.GodTossing.Base           (Opening, SignedCommitment)
import           Pos.Ssc.GodTossing.Error          (SeedError)
import           Pos.Ssc.GodTossing.Instance.Type  (SscGodTossing)
import           Pos.Ssc.GodTossing.Instance.Types ()
import           Pos.Ssc.GodTossing.Seed           (calculateSeed)
import           Pos.Ssc.GodTossing.Storage        (GtStorage, GtStorageVersion (..),
                                                    dsCurrentSecretL,
                                                    dsGlobalCertificates,
                                                    dsGlobalCommitments, dsGlobalOpenings,
                                                    dsGlobalShares, dsLastProcessedSlotL,
                                                    dsLocalCertificates,
                                                    dsLocalCommitments, dsLocalOpenings,
                                                    dsLocalShares, dsVersionedL)
import           Pos.Ssc.GodTossing.Types          (GtMessage (..), GtPayload (..),
                                                    filterGtPayload, hasCommitment,
                                                    hasOpening, hasShares, mdCommitments,
                                                    mdOpenings, mdShares,
                                                    mdVssCertificates, verifyGtPayload)
import           Pos.State.Storage.Types           (AltChain)
import           Pos.Types                         (Address (getAddress), Block,
                                                    EpochIndex, SlotId (..), SlotLeaders,
                                                    Utxo, blockMpc, blockSlot, blockSlot,
                                                    gbHeader, txOutAddress)
import           Pos.Util                          (diffDoubleMap, magnify',
                                                    readerToState, zoom', _neHead)

-- acid-state requires this instance because of a bug
instance SafeCopy SscGodTossing
instance Serialize SscGodTossing where
    put = panic "put@SscGodTossing: can't happen"
    get = panic "get@SscGodTossing: can't happen"

helper :: (NonEmpty (a, b) -> GtMessage)
       -> (a -> b -> Update Bool)
       -> NonEmpty (a, b)
       -> Update (Maybe GtMessage)
helper c f ne = do
    res <- toList <$> mapM (uncurry f) ne
    let updated = map snd . filter fst . zip res . toList $ ne
    if null updated
      then return Nothing
      else return $ Just . c . fromList $ updated

instance SscStorageClass SscGodTossing where
    sscApplyBlocks = mpcApplyBlocks
    sscPrepareToNewSlot = mpcProcessNewSlot
    sscProcessMessage (DSCommitments ne)     =
        helper DSCommitments mpcProcessCommitment ne
    sscProcessMessage (DSOpenings ne)        =
        helper DSOpenings mpcProcessOpening ne
    sscProcessMessage (DSSharesMulti ne)     =
        helper DSSharesMulti mpcProcessShares ne
    sscProcessMessage (DSVssCertificates ne) =
        helper DSVssCertificates mpcProcessVssCertificate ne
    sscRollback = mpcRollback
    sscGetLocalPayload = getLocalPayload
    sscGetGlobalPayload = getGlobalMpcData
    sscGetGlobalPayloadByDepth = getGlobalMpcDataByDepth
    sscVerifyBlocks = mpcVerifyBlocks

    sscGetToken = getSecret
    sscSetToken = setSecret
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

dsCurrentSecret
    :: HasSscStorage SscGodTossing a =>
       Lens' a (Maybe (PublicKey, SignedCommitment, Opening))
dsCurrentSecret = sscStorage @SscGodTossing . dsCurrentSecretL

dsLastProcessedSlot
    :: HasSscStorage SscGodTossing a
    => Lens' a SlotId
dsLastProcessedSlot = sscStorage @SscGodTossing . dsLastProcessedSlotL

-- | A lens to access the last version of GtStorage
lastVer :: HasSscStorage SscGodTossing a => Lens' a GtStorageVersion
lastVer = dsVersioned . _neHead

--traceMpcLastVer :: Update ()
--traceMpcLastVer = do
--    hasSecret <- isJust <$> use (lastVer . dsCurrentSecret)
--    localCommKeys <- keys' <$> use (lastVer . dsLocalCommitments)
--    globalCommKeys <- keys' <$> use (lastVer . dsGlobalCommitments)
--    localOpenKeys <- keys' <$> use (lastVer . dsLocalOpenings)
--    globalOpenKeys <- keys' <$> use (lastVer . dsGlobalOpenings)
--    localShareKeys <- keys' <$> use (lastVer . dsLocalShares)
--    globalShareKeys <- keys' <$> use (lastVer . dsGlobalShares)
--    identity $! traceM $ "[~~~~~~] dsState: hasSecret=" <> show hasSecret
--                          <> " comms=" <> show (localCommKeys, globalCommKeys)
--                          <> " opens=" <> show (localOpenKeys, globalOpenKeys)
--                          <> " shares=" <> show (localShareKeys, globalShareKeys)
--  where keys' = fmap pretty . HM.keys

getLocalPayload :: SlotId -> Query GtPayload
getLocalPayload slotId =
    (filterGtPayload slotId <$> getStoredLocalPayload) >>= ensureOwnMpc slotId

getStoredLocalPayload :: Query GtPayload
getStoredLocalPayload =
    magnify' lastVer $
    GtPayload <$> view dsLocalCommitments <*> view dsLocalOpenings <*>
    view dsLocalShares <*> view dsLocalCertificates

ensureOwnMpc :: SlotId -> GtPayload -> Query GtPayload
ensureOwnMpc slotId payload = do
    globalMpc <- getGlobalMpcData
    ourSecret <- view dsCurrentSecret
    return $ maybe identity (ensureOwnMpcDo globalMpc slotId) ourSecret payload

ensureOwnMpcDo
    :: GtPayload
    -> SlotId
    -- -> (HashMap PublicKey Share)
    -> (PublicKey, SignedCommitment, Opening)
    -> GtPayload
    -> GtPayload
ensureOwnMpcDo globalMpcData (siSlot -> slotIdx) (pk, comm, opening) md
    | isCommitmentIdx slotIdx && (not $ hasCommitment pk globalMpcData) =
        md & mdCommitments . at pk .~ Just comm
    | isOpeningIdx slotIdx && (not $ hasOpening pk globalMpcData) =
        md & mdOpenings . at pk .~ Just opening
    | isSharesIdx slotIdx && (not $ hasShares pk globalMpcData) =
        md   -- TODO: set our shares, but it's not so easy :(
    | otherwise = md

getGlobalMpcData :: Query GtPayload
getGlobalMpcData =
    fromMaybe (panic "No global SSC payload for depth 0") <$>
    getGlobalMpcDataByDepth 0

-- TODO: check for off-by-one errors!!!!111
--
-- specifically, I'm not sure whether versioning here and versioning in .Tx
-- are the same versionings
getGlobalMpcDataByDepth :: Word -> Query (Maybe GtPayload)
getGlobalMpcDataByDepth (fromIntegral -> depth) =
    preview $ dsVersioned . ix depth . to mkGlobalMpcData
  where
    mkGlobalMpcData GtStorageVersion {..} =
        GtPayload
        { _mdCommitments = _dsGlobalCommitments
        , _mdOpenings = _dsGlobalOpenings
        , _mdShares = _dsGlobalShares
        , _mdVssCertificates = _dsGlobalCertificates
        }

-- | Get keys of nodes participating in an epoch. A node participates if,
-- when there were 'k' slots left before the end of the previous epoch, both
-- of these were true:
--
--   1. It was a stakeholder.
--   2. It had already sent us its VSS key by that time.
getParticipants :: Word -> Utxo -> Query (Maybe (NonEmpty VssPublicKey))
getParticipants depth utxo = do
    mKeymap <- fmap _mdVssCertificates <$> getGlobalMpcDataByDepth depth
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

-- | Check that the secret revealed in the opening matches the secret proof
-- in the commitment
checkOpening
    :: CommitmentsMap -> (PublicKey, Opening) -> Bool
checkOpening globalCommitments (pk, opening) =
    case HM.lookup pk globalCommitments of
        Nothing        -> False
        Just (comm, _) -> verifyOpening comm opening

-- Apply checkOpening using last version.
checkOpeningLastVer :: PublicKey -> Opening -> Query Bool
checkOpeningLastVer pk opening =
    magnify' lastVer $
    flip checkOpening (pk, opening) <$> view dsGlobalCommitments

-- | Check that the decrypted share matches the encrypted share in the
-- commitment
-- TODO: check that there is no opening for share, but only after fixing
-- getOurShares!!1!1
checkShare
    :: CommitmentsMap
    -> OpeningsMap
    -> VssCertificatesMap
    -> (PublicKey, PublicKey, Share)
    -> Bool
checkShare globalCommitments _ globalCertificates (pkTo, pkFrom, share) =
    fromMaybe False $ do
        guard $ HM.member pkTo globalCommitments
        (comm, _) <- HM.lookup pkFrom globalCommitments
        vssKey <- signedValue <$> HM.lookup pkTo globalCertificates
        encShare <- HM.lookup vssKey (commShares comm)
        return $ verifyShare encShare vssKey share

-- Apply checkShare to all shares in map.
checkShares
    :: CommitmentsMap
    -> OpeningsMap
    -> VssCertificatesMap
    -> PublicKey
    -> HashMap PublicKey Share
    -> Bool
checkShares globalCommitments globalOpenings globalCertificates pkTo shares =
    let listShares :: [(PublicKey, PublicKey, Share)]
        listShares = map convert $ HM.toList shares
        convert (pkFrom, share) = (pkTo, pkFrom, share)
    in all
           (checkShare globalCommitments globalOpenings globalCertificates)
           listShares

-- Apply checkShares using last version.
checkSharesLastVer :: PublicKey -> HashMap PublicKey Share -> Query Bool
checkSharesLastVer pk shares =
    magnify' lastVer $
    (\comms openings certs -> checkShares comms openings certs pk shares) <$>
    view dsGlobalCommitments <*>
    view dsGlobalOpenings <*>
    view dsGlobalCertificates

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
    let commitments  = payload ^. mdCommitments
        openings     = payload ^. mdOpenings
        shares       = payload ^. mdShares
        certificates = payload ^. mdVssCertificates
    globalCommitments  <- view dsGlobalCommitments
    globalOpenings     <- view dsGlobalOpenings
    globalShares       <- view dsGlobalShares
    globalCertificates <- view dsGlobalCertificates
    let isComm  = isCommitmentIdx slotId
        isOpen  = isOpeningIdx slotId
        isShare = isSharesIdx slotId

    -- For commitments we
    --   * check that the nodes haven't already sent their commitments before
    --     in some different block
    --   * check that a VSS certificate is present for the committing node
    -- TODO: we might also check that all share IDs are different, because
    -- then we would be able to simplify 'calculateSeed' a bit – however,
    -- it's somewhat complicated because we have encrypted shares, shares in
    -- commitments, etc.
    let commChecks =
            [ (all (`HM.member` (certificates <> globalCertificates))
                   (HM.keys commitments),
                   "some committing nodes haven't sent a VSS certificate")
            , (all (not . (`HM.member` globalCommitments))
                   (HM.keys commitments),
                   "some nodes have already sent their commitments")
            ]

    -- For openings, we check that
    --   * the opening isn't present in previous blocks
    --   * corresponding commitment is present
    --   * the opening matches the commitment
    let openChecks =
            [ (all (not . (`HM.member` globalOpenings))
                   (HM.keys openings),
                   "some nodes have already sent their openings")
            , (all (`HM.member` globalCommitments)
                   (HM.keys openings),
                   "some openings don't have corresponding commitments")
            , (all (checkOpening globalCommitments) (HM.toList openings),
                   "some openings don't match corresponding commitments")
            ]

    -- For shares, we check that
    --   * shares have corresponding commitments
    --   * these shares weren't sent before
    --   * if encrypted shares (in commitments) are decrypted, they match
    --     decrypted shares
    -- We don't check whether shares match the openings.
    let shareChecks =
            [ (all (`HM.member` globalCommitments)
                   (HM.keys shares <> concatMap HM.keys (toList shares)),
                   "some shares don't have corresponding commitments")
            -- TODO: here we assume that all shares are always sent as a whole
            -- package.
            -- Use intersectionDoubleMap or something to allow spliting
            -- shares into multiple messages.
            , (null (shares `HM.intersection` globalShares),
                   "some shares have already been sent")
            , (all (uncurry (checkShares globalCommitments globalOpenings
                             globalCertificates)) $
                     HM.toList shares,
                   "some decrypted shares don't match encrypted shares \
                   \in the corresponding commitment")
            ]

    let ourRes = verifyGeneric $ concat $ concat
            [ [ commChecks       | isComm ]
            , [ openChecks       | isOpen ]
            , [ shareChecks      | isShare ]
            ]

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

mpcProcessCommitment
    :: PublicKey -> (Commitment, CommitmentSignature) -> Update Bool
mpcProcessCommitment pk c = do
    epochIdx <- siEpoch <$> use dsLastProcessedSlot
    ok <- readerToState $ and <$> magnify' lastVer (sequence $ checks epochIdx)
    ok <$ when ok (zoom' lastVer $ dsLocalCommitments %= HM.insert pk c)
  where
    checks epochIndex =
        [ pure . isVerSuccess $ verifySignedCommitment pk epochIndex c
        , not . HM.member pk <$> view dsGlobalCommitments
        , not . HM.member pk <$> view dsLocalCommitments
        ]

mpcProcessOpening :: PublicKey -> Opening -> Update Bool
mpcProcessOpening pk o = do
    ok <- readerToState $ and <$> sequence checks
    ok <$ when ok (zoom' lastVer $ dsLocalOpenings %= HM.insert pk o)
  where
    checks = [checkOpeningAbsence pk, checkOpeningLastVer pk o]

-- Check that there is no opening from given public key in blocks. It is useful
-- in opening processing.
checkOpeningAbsence :: PublicKey -> Query Bool
checkOpeningAbsence pk =
    magnify' lastVer $
    (&&) <$> (notMember <$> view dsGlobalOpenings) <*>
    (notMember <$> view dsLocalOpenings)
  where
    notMember = not . HM.member pk

mpcProcessShares :: PublicKey -> HashMap PublicKey Share -> Update Bool
mpcProcessShares pk s
    | null s = pure False
    | otherwise = do
        -- TODO: we accept shares that we already have (but don't add them to
        -- local shares) because someone who sent us those shares might not be
        -- aware of the fact that they are already in the blockchain. On the
        -- other hand, now nodes can send us huge spammy messages and we can't
        -- ban them for that. On the third hand, is this a concern?
        preOk <- readerToState $ checkSharesLastVer pk s
        let mpcProcessSharesDo = do
                globalSharesForPK <-
                    HM.lookupDefault mempty pk <$> use dsGlobalShares
                localSharesForPk <- HM.lookupDefault mempty pk <$> use dsLocalShares
                let s' = s `HM.difference` globalSharesForPK
                let newLocalShares = localSharesForPk `HM.union` s'
                -- Note: size is O(n), but union is also O(n + m), so
                -- it doesn't matter.
                let ok = preOk && (HM.size newLocalShares /= HM.size localSharesForPk)
                ok <$ (when ok $ dsLocalShares . at pk .= Just newLocalShares)
        zoom' lastVer $ mpcProcessSharesDo

mpcProcessVssCertificate :: PublicKey -> VssCertificate -> Update Bool
mpcProcessVssCertificate pk c = zoom' lastVer $ do
    ok <- not . HM.member pk <$> use dsGlobalCertificates
    ok <$ when ok (dsLocalCertificates %= HM.insert pk c)

-- Should be executed before doing any updates within given slot.
mpcProcessNewSlot :: SlotId -> Update ()
mpcProcessNewSlot si@SlotId {siEpoch = epochIdx, siSlot = slotIdx} = do
    zoom' lastVer $ do
        unless (isCommitmentIdx slotIdx) $ dsLocalCommitments .= mempty
        unless (isOpeningIdx slotIdx) $ dsLocalOpenings .= mempty
        unless (isSharesIdx slotIdx) $ dsLocalShares .= mempty
    whenM ((epochIdx >) . siEpoch <$> use dsLastProcessedSlot) $
        dsCurrentSecret .= Nothing
    dsLastProcessedSlot .= si

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
    --identity $! traceM . (<>) ("[~~~~~~] MPC Processing " <> (either (const "genesis") (const "main") blk) <> " block for epoch: ") . pretty $ blk ^. epochIndexL
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
            let blockCommitments  = b ^. blockMpc . mdCommitments
                blockOpenings     = b ^. blockMpc . mdOpenings
                blockShares       = b ^. blockMpc . mdShares
                blockCertificates = b ^. blockMpc . mdVssCertificates
            zoom' lastVer $ do
                -- commitments
                dsGlobalCommitments %= HM.union blockCommitments
                dsLocalCommitments  %= (`HM.difference` blockCommitments)
                -- openings
                dsGlobalOpenings %= HM.union blockOpenings
                dsLocalOpenings  %= (`HM.difference` blockOpenings)
                -- shares
                dsGlobalShares %= HM.unionWith HM.union blockShares
                dsLocalShares  %= (`diffDoubleMap` blockShares)
                -- VSS certificates
                dsGlobalCertificates %= HM.union blockCertificates
                dsLocalCertificates  %= (`HM.difference` blockCertificates)

-- | Set FTS seed (and shares) to be used in this epoch. If the seed
-- wasn't cleared before (it's cleared whenever new epoch is processed
-- by mpcProcessNewSlot), it will fail.
setSecret :: (PublicKey, SignedCommitment, Opening) -> Update ()
setSecret (ourPk, comm, op) = do
    s <- use dsCurrentSecret
    case s of
        Just _  -> panic "setSecret: a secret was already present"
        Nothing -> dsCurrentSecret .= Just (ourPk, comm, op)

getSecret :: Query (Maybe (PublicKey, SignedCommitment, Opening))
getSecret = view dsCurrentSecret

-- | Decrypt shares (in commitments) that we can decrypt.
-- TODO: do not decrypt shares for which we know openings!
getOurShares
    :: VssKeyPair                           -- ^ Our VSS key
    -> Integer                              -- ^ Random generator seed
                                            -- (needed for 'decryptShare')
    -> Query (HashMap PublicKey Share)
getOurShares ourKey seed = do
    let drg = drgNewSeed (seedFromInteger seed)
    comms <- view (lastVer . dsGlobalCommitments)
    return $ fst $ withDRG drg $
        fmap (HM.fromList . catMaybes) $
            forM (HM.toList comms) $ \(theirPK, (Commitment{..}, _)) -> do
                let mbEncShare = HM.lookup (toVssPublicKey ourKey) commShares
                case mbEncShare of
                    Nothing       -> return Nothing
                    Just encShare -> Just . (theirPK,) <$>
                                     decryptShare ourKey encShare
                -- TODO: do we need to verify shares with 'verifyEncShare'
                -- here? Or do we need to verify them earlier (i.e. at the
                -- stage of commitment verification)?
