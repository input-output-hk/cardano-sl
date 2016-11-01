{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Internal state of the MPC algorithm (“multi-party computation”) – the
-- algorithm which computes a shared seed with other nodes and decides which
-- nodes will be leaders of the next epoch.

module Pos.State.Storage.Mpc
       (
         MpcStorage
       , HasMpcStorage(mpcStorage)

       , calculateLeaders
       , getLocalMpcData
       , getGlobalMpcData
       , getGlobalMpcDataByDepth
       , getOurCommitment
       , getOurOpening
       , getOurShares
       , getSecret
       , setSecret
       , mpcApplyBlocks
       , mpcProcessCommitment
       , mpcProcessNewSlot
       , mpcProcessOpening
       , mpcProcessShares
       , mpcProcessVssCertificate
       , mpcRollback
       , mpcVerifyBlock
       , mpcVerifyBlocks
       --, traceMpcLastVer
       ) where

import           Control.Lens            (Lens', at, ix, makeClassy, preview, to, use,
                                          view, (%=), (.=), (.~), (^.), _2, _3)
import           Crypto.Random           (drgNewSeed, seedFromInteger, withDRG)
import           Data.Default            (Default, def)
import           Data.Hashable           (Hashable)
import qualified Data.HashMap.Strict     as HM
import           Data.Ix                 (inRange)
import           Data.List.NonEmpty      (NonEmpty ((:|)))
import qualified Data.List.NonEmpty      as NE
import           Data.SafeCopy           (base, deriveSafeCopySimple)
import           Serokell.Util.Verify    (VerificationRes (..), isVerSuccess,
                                          verifyGeneric)
import           Universum

import           Pos.Constants           (k)
import           Pos.Crypto              (PublicKey, Share,
                                          Signed (signedSig, signedValue), Threshold,
                                          VssKeyPair, decryptShare, toVssPublicKey,
                                          verify, verifyShare)
import           Pos.FollowTheSatoshi    (FtsError, calculateSeed, followTheSatoshi)
import           Pos.Genesis             (genesisCertificates)
import           Pos.State.Storage.Types (AltChain)
import           Pos.Types               (Address (getAddress), Block, Commitment (..),
                                          CommitmentSignature, CommitmentsMap,
                                          MpcData (..), Opening (..), OpeningsMap,
                                          SharesMap, SignedCommitment, SlotId (..),
                                          SlotLeaders, Utxo, VssCertificate,
                                          VssCertificatesMap, blockMpc, blockSlot,
                                          blockSlot, hasCommitment, hasOpening, hasShares,
                                          mdCommitments, mdOpenings, mdShares,
                                          mdVssCertificates, unflattenSlotId,
                                          verifyOpening, verifySignedCommitment)
import           Pos.Util                (magnify', readerToState, zoom', _neHead)

data MpcStorageVersion = MpcStorageVersion
    { -- | Local set of 'Commitment's. These are valid commitments which are
      -- known to the node and not stored in blockchain. It is useful only
      -- for the first 'k' slots, after that it should be discarded.
      _mpcLocalCommitments   :: !CommitmentsMap
    , -- | Set of 'Commitment's stored in blocks for current epoch. This can
      -- be calculated by 'mconcat'ing stored commitments, but it would be
      -- inefficient to do it every time we need to know if commitments is
      -- stored in blocks.
      _mpcGlobalCommitments  :: !CommitmentsMap
    , -- | Local set of decrypted shares (encrypted shares are stored in
      -- commitments).
      _mpcLocalShares        :: !SharesMap
    , -- | Decrypted shares stored in blocks. These shares are guaranteed to
      -- match encrypted shares stored in 'mpcGlobalCommitments'.
      _mpcGlobalShares       :: !SharesMap
    , -- | Local set of openings
      _mpcLocalOpenings      :: !OpeningsMap
    , -- | Openings stored in blocks
      _mpcGlobalOpenings     :: !OpeningsMap
    , -- | Local set of VSS certificates
      _mpcLocalCertificates  :: !VssCertificatesMap
    , -- | VSS certificates stored in blocks (for all time, not just for
      -- current epoch)
      _mpcGlobalCertificates :: !VssCertificatesMap }
      deriving Show

makeClassy ''MpcStorageVersion
deriveSafeCopySimple 0 'base ''MpcStorageVersion

instance Default MpcStorageVersion where
    def =
        MpcStorageVersion
        { _mpcLocalCommitments = mempty
        , _mpcGlobalCommitments = mempty
        , _mpcLocalShares = mempty
        , _mpcGlobalShares = mempty
        , _mpcLocalOpenings = mempty
        , _mpcGlobalOpenings = mempty
        , _mpcLocalCertificates = mempty
        , _mpcGlobalCertificates = genesisCertificates
        }

data MpcStorage = MpcStorage
    { -- | Last several versions of MPC storage, a version for each received
      -- block. To bring storage to the state as it was just before the last
      -- block arrived, just remove the head. All incoming commitments/etc
      -- which aren't parts of blocks are applied to the head, too.
      --
      -- TODO: this is a very naive solution. A better one would be storing
      -- deltas for maps in 'MpcStorageVersion'.
      _mpcVersioned         :: NonEmpty MpcStorageVersion
    , -- | Secret that we are using for the current epoch.
      _mpcCurrentSecret     :: !(Maybe (PublicKey, SignedCommitment, Opening))
    , -- | Last slot we are aware of.
      _mpcLastProcessedSlot :: !SlotId
    }

makeClassy ''MpcStorage
deriveSafeCopySimple 0 'base ''MpcStorage

-- | A lens to access the last version of MpcStorage
lastVer :: HasMpcStorage a => Lens' a MpcStorageVersion
lastVer = mpcVersioned . _neHead

instance Default MpcStorage where
    def =
        MpcStorage
        { _mpcVersioned = (def :| [])
        , _mpcCurrentSecret = Nothing
        , _mpcLastProcessedSlot = unflattenSlotId 0
        }

type Update a = forall m x. (HasMpcStorage x, MonadState x m) => m a
-- If this type ever changes to include side effects (error reporting, etc)
-- we might have to change 'mpcVerifyBlock' because currently it works by
-- simulating block application and we don't want block verification to have
-- any side effects. The compiler will warn us if it happens, though.
type Query a = forall m x. (HasMpcStorage x, MonadReader x m) => m a

--traceMpcLastVer :: Update ()
--traceMpcLastVer = do
--    hasSecret <- isJust <$> use (lastVer . mpcCurrentSecret)
--    localCommKeys <- keys' <$> use (lastVer . mpcLocalCommitments)
--    globalCommKeys <- keys' <$> use (lastVer . mpcGlobalCommitments)
--    localOpenKeys <- keys' <$> use (lastVer . mpcLocalOpenings)
--    globalOpenKeys <- keys' <$> use (lastVer . mpcGlobalOpenings)
--    localShareKeys <- keys' <$> use (lastVer . mpcLocalShares)
--    globalShareKeys <- keys' <$> use (lastVer . mpcGlobalShares)
--    identity $! traceM $ "[~~~~~~] mpcState: hasSecret=" <> show hasSecret
--                          <> " comms=" <> show (localCommKeys, globalCommKeys)
--                          <> " opens=" <> show (localOpenKeys, globalOpenKeys)
--                          <> " shares=" <> show (localShareKeys, globalShareKeys)
--  where keys' = fmap pretty . HM.keys

getGlobalMpcData :: Query MpcData
getGlobalMpcData =
    fromMaybe (panic "No global MPC data for depth 0") <$>
    getGlobalMpcDataByDepth 0

getLocalMpcData :: Query MpcData
getLocalMpcData =
    (magnify' lastVer $
     MpcData <$> (view mpcLocalCommitments) <*> (view mpcLocalOpenings) <*>
     view mpcLocalShares <*>
     view mpcLocalCertificates) >>=
    ensureOwnMpc

ensureOwnMpc :: MpcData -> Query MpcData
ensureOwnMpc md = do
    globalMpc <- getGlobalMpcData
    -- ourShares <- getOurShares
    ourComm <- view mpcCurrentSecret
    slotId <- view mpcLastProcessedSlot
    return $ maybe identity (ensureOwnMpcDo globalMpc slotId) ourComm md

ensureOwnMpcDo
    :: MpcData
    -> SlotId
    -- -> (HashMap PublicKey Share)
    -> (PublicKey, SignedCommitment, Opening)
    -> MpcData
    -> MpcData
ensureOwnMpcDo globalMpcData (siSlot -> slotId) (pk, comm, opening) md
    | slotId < k && (not $ hasCommitment pk globalMpcData) =
        md & mdCommitments . at pk .~ Just comm
    | inRange (2 * k, 3 * k - 1) slotId && (not $ hasOpening pk globalMpcData) =
        md & mdOpenings . at pk .~ Just opening
    | inRange (5 * k, 6 * k - 1) slotId && (not $ hasShares pk globalMpcData) =
        md   -- TODO: set our shares, but it's not so easy :(
    | otherwise = md

-- TODO: check for off-by-one errors!!!!111
--
-- specifically, I'm not sure whether versioning here and versioning in .Tx
-- are the same versionings
getGlobalMpcDataByDepth :: Word -> Query (Maybe MpcData)
getGlobalMpcDataByDepth (fromIntegral -> depth) =
    preview $ mpcVersioned . ix depth . to mkGlobalMpcData
  where
    mkGlobalMpcData MpcStorageVersion {..} =
        MpcData
        { _mdCommitments = _mpcGlobalCommitments
        , _mdOpenings = _mpcGlobalOpenings
        , _mdShares = _mpcGlobalShares
        , _mdVssCertificates = _mpcGlobalCertificates
        }

-- | Calculate leaders for the next epoch.
calculateLeaders
    :: Utxo            -- ^ Utxo (k slots before the end of epoch)
    -> Threshold
    -> Query (Either FtsError SlotLeaders)
calculateLeaders utxo threshold = do
    mbSeed <- calculateSeed threshold
                            <$> view (lastVer . mpcGlobalCommitments)
                            <*> view (lastVer . mpcGlobalOpenings)
                            <*> view (lastVer . mpcGlobalShares)
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
    flip checkOpening (pk, opening) <$> view mpcGlobalCommitments

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
    view mpcGlobalCommitments <*>
    view mpcGlobalOpenings <*>
    view mpcGlobalCertificates

-- | Check that the VSS certificate is signed properly
checkCert
    :: (PublicKey, VssCertificate)
    -> Bool
checkCert (pk, cert) = verify pk (signedValue cert) (signedSig cert)

{- |
Verify MPC-related predicates of a single block, also using data stored
in 'MpcStorage'.

For each MPC message we check:

  1. Whether it's stored in the correct block (e.g. commitments have to be in
     first k blocks, etc.)

  2. Whether the message itself is correct (e.g. commitment signature is
     valid, etc.)
-}
mpcVerifyBlock :: Block -> Query VerificationRes
-- Genesis blocks don't have any MPC messages
mpcVerifyBlock (Left _) = return VerSuccess
-- Main blocks have commitments, openings, shares and VSS certificates
mpcVerifyBlock (Right b) = do
    let SlotId{siSlot = slotId, siEpoch = epochId} = b ^. blockSlot
    let commitments  = b ^. blockMpc . mdCommitments
        openings     = b ^. blockMpc . mdOpenings
        shares       = b ^. blockMpc . mdShares
        certificates = b ^. blockMpc . mdVssCertificates
    globalCommitments  <- view (lastVer . mpcGlobalCommitments)
    globalOpenings     <- view (lastVer . mpcGlobalOpenings)
    globalShares       <- view (lastVer . mpcGlobalShares)
    globalCertificates <- view (lastVer . mpcGlobalCertificates)
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
            -- TODO: use intersectionDoubleMap or something to allow spliting
            -- shares into multiple messages
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

-- | Verify MPC-related predicates of blocks sequence which is about to be
-- applied. It should check that MPC messages will be consistent if this
-- blocks are applied (after possible rollback if 'toRollback' isn't zero).
--
-- TODO:
--   * verification messages should include block hash/slotId
--   * we should stop at first failing block
mpcVerifyBlocks :: Word -> AltChain -> Query VerificationRes
mpcVerifyBlocks toRollback blocks = do
    curState <- view mpcStorage
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
mpcProcessCommitment pk c =
    zoom' lastVer $
    do ok <-
           (&&) <$> (not . HM.member pk <$> use mpcGlobalCommitments) <*>
           (not . HM.member pk <$> use mpcLocalCommitments)
       ok <$ when ok (mpcLocalCommitments %= HM.insert pk c)

mpcProcessOpening :: PublicKey -> Opening -> Update Bool
mpcProcessOpening pk o = do
    ok <- readerToState $ and <$> sequence checks
    ok <$ when ok (zoom' lastVer $ mpcLocalOpenings %= HM.insert pk o)
  where
    checks = [checkOpeningAbsence pk, checkOpeningLastVer pk o]

-- Check that there is no opening from given public key in blocks. It is useful
-- in opening processing.
checkOpeningAbsence :: PublicKey -> Query Bool
checkOpeningAbsence pk =
    magnify' lastVer $ not . HM.member pk <$> view mpcGlobalOpenings

mpcProcessShares :: PublicKey -> HashMap PublicKey Share -> Update Bool
mpcProcessShares pk s
    | null s = pure False
    | otherwise = do
        -- TODO: we accept shares that we already have (but don't add them to
        -- local shares) because someone who sent us those shares might not be
        -- aware of the fact that they are already in the blockchain. On the
        -- other hand, now nodes can send us huge spammy messages and we can't
        -- ban them for that. On the third hand, is this a concern?
        ok <- (readerToState $ checkSharesLastVer pk s)
        let mpcProcessSharesDo = do
                globalSharesForPK <-
                    HM.lookupDefault mempty pk <$> use mpcGlobalShares
                let s' = s `HM.difference` globalSharesForPK
                unless (null s') $
                    mpcLocalShares %= HM.insertWith HM.union pk s'
        ok <$ (when ok $ zoom' lastVer $ mpcProcessSharesDo)

mpcProcessVssCertificate :: PublicKey -> VssCertificate -> Update ()
mpcProcessVssCertificate pk c = zoom' lastVer $ do
    unlessM (HM.member pk <$> use mpcGlobalCertificates) $ do
        mpcLocalCertificates %= HM.insert pk c

-- Should be executed before doing any updates within given slot.
-- TODO: clean-up commitments, openings, shares.
mpcProcessNewSlot :: SlotId -> Update ()
mpcProcessNewSlot si@SlotId {siEpoch = epochIdx} = do
    whenM ((epochIdx >) . siEpoch <$> use mpcLastProcessedSlot) $
        mpcCurrentSecret .= Nothing
    mpcLastProcessedSlot .= si

-- | Apply sequence of blocks to state. Sequence must be based on last
-- applied block and must be valid.
mpcApplyBlocks :: AltChain -> Update ()
mpcApplyBlocks = mapM_ mpcProcessBlock

-- | Rollback application of last 'n' blocks. If @n > 0@, also removes all
-- commitments/etc received during that period but not included into
-- blocks. If there are less blocks than 'n' is, just leaves an empty ('def')
-- version.
mpcRollback :: Word -> Update ()
mpcRollback (fromIntegral -> n) = do
    mpcVersioned %= (fromMaybe (def :| []) . NE.nonEmpty . NE.drop n)

mpcProcessBlock :: Block -> Update ()
mpcProcessBlock blk = do
    --identity $! traceM . (<>) ("[~~~~~~] MPC Processing " <> (either (const "genesis") (const "main") blk) <> " block for epoch: ") . pretty $ blk ^. epochIndexL
    lv <- use lastVer
    mpcVersioned %= NE.cons lv
    case blk of
        -- Genesis blocks don't contain anything interesting, but when they
        -- “arrive”, we clear global commitments and other globals. Not
        -- certificates, though, because we don't want to make nodes resend
        -- them in each epoch.
        Left _ -> do
            zoom' lastVer $ do
                mpcGlobalCommitments .= mempty
                mpcGlobalOpenings    .= mempty
                mpcGlobalShares      .= mempty
        -- Main blocks contain commitments, openings, shares, VSS certificates
        Right b -> do
            let blockCommitments  = b ^. blockMpc . mdCommitments
                blockOpenings     = b ^. blockMpc . mdOpenings
                blockShares       = b ^. blockMpc . mdShares
                blockCertificates = b ^. blockMpc . mdVssCertificates
            zoom' lastVer $ do
                -- commitments
                mpcGlobalCommitments %= HM.union blockCommitments
                mpcLocalCommitments  %= (`HM.difference` blockCommitments)
                -- openings
                mpcGlobalOpenings %= HM.union blockOpenings
                mpcLocalOpenings  %= (`HM.difference` blockOpenings)
                -- shares
                mpcGlobalShares %= HM.unionWith HM.union blockShares
                mpcLocalShares  %= (`diffDoubleMap` blockShares)
                -- VSS certificates
                mpcGlobalCertificates %= HM.union blockCertificates
                mpcLocalCertificates  %= (`HM.difference` blockCertificates)

-- | Set FTS seed (and shares) to be used in this epoch. If the seed
-- wasn't cleared before (it's cleared whenever new epoch is processed
-- by mpcProcessNewSlot), it will fail.
setSecret :: PublicKey -> (SignedCommitment, Opening) -> Update ()
setSecret ourPk (comm, op) = do
    s <- use mpcCurrentSecret
    case s of
        Just _  -> panic "setSecret: a secret was already present"
        Nothing -> mpcCurrentSecret .= Just (ourPk, comm, op)

getSecret :: Query (Maybe (PublicKey, SignedCommitment, Opening))
getSecret = view mpcCurrentSecret

getOurCommitment :: Query (Maybe SignedCommitment)
getOurCommitment = fmap (view _2) <$> view mpcCurrentSecret

getOurOpening :: Query (Maybe Opening)
getOurOpening = fmap (view _3) <$> view mpcCurrentSecret

-- | Decrypt shares (in commitments) that we can decrypt.
-- TODO: do not decrypt shares for which we know openings!
getOurShares
    :: VssKeyPair                           -- ^ Our VSS key
    -> Integer                              -- ^ Random generator seed
                                            -- (needed for 'decryptShare')
    -> Query (HashMap PublicKey Share)
getOurShares ourKey seed = do
    let drg = drgNewSeed (seedFromInteger seed)
    comms <- view (lastVer . mpcGlobalCommitments)
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
