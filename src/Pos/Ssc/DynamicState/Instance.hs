{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Instance of SscTypes.

module Pos.Ssc.DynamicState.Instance
       ( SscDynamicState
       ) where

import           Control.Lens                 (Lens', at, ix, preview, to, use, view,
                                               (%=), (.=), (.~), (^.))
import           Crypto.Random                (drgNewSeed, seedFromInteger, withDRG)
import           Data.Default                 (def)
import           Data.Hashable                (Hashable)
import qualified Data.HashMap.Strict          as HM
import           Data.Ix                      (inRange)
import           Data.List                    (nub)
import           Data.List.NonEmpty           (NonEmpty ((:|)))
import qualified Data.List.NonEmpty           as NE
import           Data.SafeCopy                (SafeCopy)
import           Data.Serialize               (Serialize (..))
import           Data.Tagged                  (Tagged (..))
import           Formatting                   (int, sformat, (%))
import           Serokell.Util.Verify         (VerificationRes (..), isVerSuccess,
                                               verifyGeneric)
import           Universum

import           Pos.Constants                (k)
import           Pos.Crypto                   (Share, Signed (signedSig, signedValue),
                                               Threshold, VssKeyPair, VssPublicKey,
                                               decryptShare, toVssPublicKey, verify,
                                               verifyShare)
import           Pos.Crypto                   (PublicKey)
import           Pos.FollowTheSatoshi         (followTheSatoshi)
import           Pos.Ssc.Class.Storage        (SscQuery, SscUpdate)
import           Pos.Ssc.Class.Storage        (HasSscStorage (..), SscStorageClass (..))
import           Pos.Ssc.Class.Types          (SscTypes (..))
import           Pos.Ssc.DynamicState.Base    (Commitment (..), CommitmentSignature,
                                               CommitmentsMap, OpeningsMap,
                                               VssCertificate, VssCertificatesMap,
                                               isCommitmentIdx, isOpeningIdx, isSharesIdx,
                                               verifyOpening, verifySignedCommitment)
import           Pos.Ssc.DynamicState.Base    (Opening, SignedCommitment)
import           Pos.Ssc.DynamicState.Error   (SeedError)
import           Pos.Ssc.DynamicState.Seed    (calculateSeed)
import           Pos.Ssc.DynamicState.Storage (DSStorage, DSStorageVersion (..),
                                               dsCurrentSecretL, dsGlobalCertificates,
                                               dsGlobalCommitments, dsGlobalOpenings,
                                               dsGlobalShares, dsLastProcessedSlotL,
                                               dsLocalCertificates, dsLocalCommitments,
                                               dsLocalOpenings, dsLocalShares,
                                               dsVersionedL)
import           Pos.Ssc.DynamicState.Types   (DSMessage (..), DSPayload (..), DSProof,
                                               filterDSPayload, hasCommitment, hasOpening,
                                               hasShares, mdCommitments, mdOpenings,
                                               mkDSProof, verifyDSPayload)
import           Pos.State.Storage.Types      (AltChain)
import           Pos.Types                    (Address (getAddress), Block, SlotId (..),
                                               SlotLeaders, Utxo, blockMpc, blockSlot,
                                               blockSlot, txOutAddress, utxoF)
import           Pos.Util                     (Color (Magenta), colorize, magnify',
                                               readerToState, zoom', _neHead)

data SscDynamicState
    deriving (Generic)

-- acid-state requires this instance because of a bug
instance SafeCopy SscDynamicState
instance Serialize SscDynamicState where
    put = panic "put@SscDynamicState: can't happen"
    get = panic "get@SscDynamicState: can't happen"

instance SscTypes SscDynamicState where
    type SscStorage   SscDynamicState = DSStorage
    type SscPayload   SscDynamicState = DSPayload
    type SscProof     SscDynamicState = DSProof
    type SscMessage   SscDynamicState = DSMessage
    type SscSeedError SscDynamicState = SeedError
    type SscToken     SscDynamicState = (PublicKey, SignedCommitment, Opening)

    mkSscProof = Tagged mkDSProof

instance SscStorageClass SscDynamicState where
    sscApplyBlocks = mpcApplyBlocks
    sscPrepareToNewSlot = mpcProcessNewSlot
    sscProcessMessage (DSCommitment pk comm) = mpcProcessCommitment pk comm
    sscProcessMessage (DSOpening pk op) = mpcProcessOpening pk op
    sscProcessMessage (DSShares pk ss) = mpcProcessShares pk ss
    sscProcessMessage (DSVssCertificate pk cert) =
        mpcProcessVssCertificate pk cert
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

    sscVerifyPayload = Tagged verifyDSPayload

type Query a = SscQuery SscDynamicState a
type Update a = SscUpdate SscDynamicState a

instance (SscStorage ssc ~ DSStorage) => HasSscStorage ssc DSStorage where
    sscStorage = identity

dsVersioned
    :: HasSscStorage SscDynamicState a =>
       Lens' a (NonEmpty DSStorageVersion)
dsVersioned = sscStorage @SscDynamicState. dsVersionedL

dsCurrentSecret
    :: HasSscStorage SscDynamicState a =>
       Lens' a (Maybe (PublicKey, SignedCommitment, Opening))
dsCurrentSecret = sscStorage @SscDynamicState . dsCurrentSecretL

dsLastProcessedSlot
    :: HasSscStorage SscDynamicState a
    => Lens' a SlotId
dsLastProcessedSlot = sscStorage @SscDynamicState . dsLastProcessedSlotL

-- | A lens to access the last version of DSStorage
lastVer :: HasSscStorage SscDynamicState a => Lens' a DSStorageVersion
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

getLocalPayload :: SlotId -> Query DSPayload
getLocalPayload slotId =
    (filterDSPayload slotId <$> getStoredLocalPayload) >>= ensureOwnMpc slotId

getStoredLocalPayload :: Query DSPayload
getStoredLocalPayload =
    magnify' lastVer $
    DSPayload <$> view dsLocalCommitments <*> view dsLocalOpenings <*>
    view dsLocalShares <*> view dsLocalCertificates

ensureOwnMpc :: SlotId -> DSPayload -> Query DSPayload
ensureOwnMpc slotId payload = do
    globalMpc <- getGlobalMpcData
    ourSecret <- view dsCurrentSecret
    return $ maybe identity (ensureOwnMpcDo globalMpc slotId) ourSecret payload

ensureOwnMpcDo
    :: DSPayload
    -> SlotId
    -- -> (HashMap PublicKey Share)
    -> (PublicKey, SignedCommitment, Opening)
    -> DSPayload
    -> DSPayload
ensureOwnMpcDo globalMpcData (siSlot -> slotIdx) (pk, comm, opening) md
    | isCommitmentIdx slotIdx && (not $ hasCommitment pk globalMpcData) =
        md & mdCommitments . at pk .~ Just comm
    | isOpeningIdx slotIdx && (not $ hasOpening pk globalMpcData) =
        md & mdOpenings . at pk .~ Just opening
    | isSharesIdx slotIdx && (not $ hasShares pk globalMpcData) =
        md   -- TODO: set our shares, but it's not so easy :(
    | otherwise = md

getGlobalMpcData :: Query DSPayload
getGlobalMpcData =
    fromMaybe (panic "No global SSC payload for depth 0") <$>
    getGlobalMpcDataByDepth 0

-- TODO: check for off-by-one errors!!!!111
--
-- specifically, I'm not sure whether versioning here and versioning in .Tx
-- are the same versionings
getGlobalMpcDataByDepth :: Word -> Query (Maybe DSPayload)
getGlobalMpcDataByDepth (fromIntegral -> depth) =
    preview $ dsVersioned . ix depth . to mkGlobalMpcData
  where
    mkGlobalMpcData DSStorageVersion {..} =
        DSPayload
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
    :: Utxo            -- ^ Utxo (k slots before the end of epoch)
    -> Threshold
    -> Query (Either SeedError SlotLeaders)
calculateLeaders utxo threshold = do
    !() <- traceM $ colorize Magenta $ (sformat ("utxo: "%utxoF%", threshold: "%int) utxo threshold)
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

-- | Check that the VSS certificate is signed properly
checkCert
    :: (PublicKey, VssCertificate)
    -> Bool
checkCert (pk, cert) = verify pk (signedValue cert) (signedSig cert)

{- |

FIXME: this function does more than described below!
Specifically, it uses information about global data assuming that block is
based on it.
Note about FIXME: it applies to documentation only!

Verify MPC-related predicates of a single block, also using data stored
in 'MpcStorage'.

For each MPC message we check:

  1. Whether it's stored in the correct block (e.g. commitments have to be in
     first k blocks, etc.)

  2. Whether the message itself is correct (e.g. commitment signature is
     valid, etc.)
-}
mpcVerifyBlock
    :: (SscPayload ssc ~ DSPayload)
    => Block ssc -> Query VerificationRes
-- Genesis blocks don't have any MPC messages
mpcVerifyBlock (Left _) = return VerSuccess
-- Main blocks have commitments, openings, shares and VSS certificates
mpcVerifyBlock (Right b) = do
    let SlotId{siSlot = slotId, siEpoch = epochId} = b ^. blockSlot
    let commitments  = b ^. blockMpc . to _mdCommitments
        openings     = b ^. blockMpc . to _mdOpenings
        shares       = b ^. blockMpc . to _mdShares
        certificates = b ^. blockMpc . to _mdVssCertificates
    globalCommitments  <- view (lastVer . dsGlobalCommitments)
    globalOpenings     <- view (lastVer . dsGlobalOpenings)
    globalShares       <- view (lastVer . dsGlobalShares)
    globalCertificates <- view (lastVer . dsGlobalCertificates)
    -- Commitment blocks are ones in range [0,k), etc. Mixed blocks aren't
    -- allowed.
    let isComm  = inRange (0, k - 1) slotId
        isOpen  = inRange (2 * k, 3 * k - 1) slotId
        isShare = inRange (4 * k, 5 * k - 1) slotId

    -- We *forbid* blocks from having commitments/openings/shares in blocks
    -- with wrong slotId (instead of merely discarding such commitments/etc)
    -- because it's the miner's responsibility not to include them into the
    -- block if they're late.
    --
    -- For commitments specifically, we also
    --   * use verifySignedCommitment, which checks commitments themselves, e. g.
    --     checks their signatures (which includes checking that the
    --     commitment has been generated for this particular epoch)
    --   * check that the nodes haven't already sent their commitments before
    --     in some different block
    --   * check that a VSS certificate is present for the committing node
    -- TODO: we might also check that all share IDs are different, because
    -- then we would be able to simplify 'calculateSeed' a bit – however,
    -- it's somewhat complicated because we have encrypted shares, shares in
    -- commitments, etc.
    let commChecks =
            [ (null openings,
                   "there are openings in a commitment block")
            , (null shares,
                   "there are shares in a commitment block")
            , (let checkSignedComm = isVerSuccess .
                     uncurry (flip verifySignedCommitment epochId)
               in all checkSignedComm (HM.toList commitments),
                   "verifySignedCommitment has failed for some commitments")
            , (all (`HM.member` (certificates <> globalCertificates))
                   (HM.keys commitments),
                   "some committing nodes haven't sent a VSS certificate")
            , (all (not . (`HM.member` globalCommitments))
                   (HM.keys commitments),
                   "some nodes have already sent their commitments")
            ]

    -- For openings, we check that
    --   * there are only openings in the block
    --   * the opening isn't present in previous blocks
    --   * corresponding commitment is present
    --   * the opening matches the commitment
    let openChecks =
            [ (null commitments,
                   "there are commitments in an openings block")
            , (null shares,
                   "there are shares in an openings block")
            , (all (not . (`HM.member` globalOpenings))
                   (HM.keys openings),
                   "some nodes have already sent their openings")
            , (all (`HM.member` globalCommitments)
                   (HM.keys openings),
                   "some openings don't have corresponding commitments")
            , (all (checkOpening globalCommitments) (HM.toList openings),
                   "some openings don't match corresponding commitments")
            ]

    -- For shares, we check that
    --   * there are only shares in the block
    --   * shares have corresponding commitments
    --   * these shares weren't sent before
    --   * if encrypted shares (in commitments) are decrypted, they match
    --     decrypted shares
    -- We don't check whether shares match the openings.
    let shareChecks =
            [ (null commitments,
                   "there are commitments in a shares block")
            , (null openings,
                   "there are openings in a shares block")
            , (all (`HM.member` globalCommitments)
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

    -- For all other blocks, we check that
    --   * there are no commitments, openings or shares
    let otherBlockChecks =
            [ (null commitments,
                   "there are commitments in an ordinary block")
            , (null openings,
                   "there are openings in an ordinary block")
            , (null shares,
                   "there are shares in an ordinary block")
            ]

    -- For all blocks (no matter the type), we check that
    --   * slot ID is in range
    --   * VSS certificates are signed properly
    -- TODO: check that nodes providing their VSS certificates have stake
    let otherChecks =
            [ (inRange (0, 6 * k - 1) slotId,
                   "slot id is outside of [0, 6k)")
            , (all checkCert (HM.toList certificates),
                   "some VSS certificates aren't signed properly")
            ]

    return $ verifyGeneric $ concat $ concat
        [ [ commChecks       | isComm ]
        , [ openChecks       | isOpen ]
        , [ shareChecks      | isShare ]
        , [ otherBlockChecks | all not [isComm, isOpen, isShare] ]
        , [ otherChecks ]
        ]

-- TODO:
--   ★ verification messages should include block hash/slotId
--   ★ we should stop at first failing block
mpcVerifyBlocks :: Word -> AltChain SscDynamicState -> Query VerificationRes
mpcVerifyBlocks toRollback blocks = do
    curState <- view (sscStorage @SscDynamicState)
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
        preOk <- (readerToState $ checkSharesLastVer pk s)
        let mpcProcessSharesDo = do
                globalSharesForPK <-
                    HM.lookupDefault mempty pk <$> use dsGlobalShares
                let s' = s `HM.difference` globalSharesForPK
                let ok = preOk && not (null s')
                ok <$ (when ok $ dsLocalShares %= HM.insertWith HM.union pk s')
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
    :: (SscPayload ssc ~ DSPayload)
    => AltChain ssc -> Update ()
mpcApplyBlocks = mapM_ mpcProcessBlock

mpcRollback :: Word -> Update ()
mpcRollback (fromIntegral -> n) = do
    dsVersioned %= (fromMaybe (def :| []) . NE.nonEmpty . NE.drop n)

mpcProcessBlock
    :: (SscPayload ssc ~ DSPayload)
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
            let blockCommitments  = b ^. blockMpc . to _mdCommitments
                blockOpenings     = b ^. blockMpc . to _mdOpenings
                blockShares       = b ^. blockMpc . to _mdShares
                blockCertificates = b ^. blockMpc . to _mdVssCertificates
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

-- | Remove elements in 'b' from 'a'
diffDoubleMap
    :: (Eq k1, Eq k2, Hashable k1, Hashable k2)
    => HashMap k1 (HashMap k2 v)
    -> HashMap k1 (HashMap k2 v)
    -> HashMap k1 (HashMap k2 v)
diffDoubleMap a b = HM.filter (not . null) $ HM.unionWith HM.difference a b
