{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Internal state of the MPC algorithm (“multi-party computation”) – the
-- algorithm which computes a shared seed with other nodes and decides which
-- nodes will be leaders of the next epoch.

module Pos.State.Storage.Mpc
       (
         MpcStorage
       , HasMpcStorage(mpcStorage)

       , calculateLeaders
       , mpcApplyBlocks
       , mpcProcessCommitment
       , mpcProcessOpening
       , mpcRollback
       , mpcVerifyBlock
       , mpcVerifyBlocks
       ) where

import           Control.Lens            (Lens', makeClassy, to, view, (%=), (^.))
import           Data.Default            (Default, def)
import           Data.Hashable           (Hashable)
import qualified Data.HashMap.Strict     as HM
import           Data.Ix                 (inRange)
import           Data.List.NonEmpty      (NonEmpty ((:|)))
import qualified Data.List.NonEmpty      as NE
import           Data.SafeCopy           (base, deriveSafeCopySimple)
import qualified Data.Vector             as V
import           Serokell.Util.Verify    (VerificationRes (..), isVerSuccess,
                                          verifyGeneric)
import           Universum

import           Pos.Constants           (k)
import           Pos.Crypto              (PublicKey, Share,
                                          Signed (signedSig, signedValue), verify,
                                          verifyShare)
import           Pos.FollowTheSatoshi    (FtsError, calculateSeed, followTheSatoshi)
import           Pos.State.Storage.Types (AltChain)
import           Pos.Types               (Address (getAddress), Block, Body (..),
                                          Commitment (..), CommitmentSignature,
                                          CommitmentsMap, Opening (..), OpeningsMap,
                                          SharesMap, SlotId (..), SlotLeaders, Utxo,
                                          VssCertificate, VssCertificatesMap, blockSlot,
                                          gbBody, mbCommitments, mbOpenings, mbShares,
                                          mbVssCertificates, verifyOpening)
import           Pos.Util                (readerToState, zoom', _neHead)

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
        , _mpcGlobalCertificates = mempty
        }

data MpcStorage = MpcStorage
    { -- | Last several versions of MPC storage, a version for each received
      -- block. To bring storage to the state as it was just before the last
      -- block arrived, just remove the head. All incoming commitments/etc
      -- which aren't parts of blocks are applied to the head, too.
      --
      -- TODO: this is a very naive solution. A better one would be storing
      -- deltas for maps in 'MpcStorageVersion'.
      _mpcVersioned :: NonEmpty MpcStorageVersion
    }

makeClassy ''MpcStorage
deriveSafeCopySimple 0 'base ''MpcStorage

-- | A lens to access the last version of MpcStorage
lastVer :: HasMpcStorage a => Lens' a MpcStorageVersion
lastVer = mpcVersioned . _neHead

instance Default MpcStorage where
    def = MpcStorage (def :| [])

type Update a = forall m x. (HasMpcStorage x, MonadState x m) => m a
-- If this type ever changes to include side effects (error reporting, etc)
-- we might have to change 'mpcVerifyBlock' because currently it works by
-- simulating block application and we don't want block verification to have
-- any side effects. The compiler will warn us if it happens, though.
type Query a = forall m x. (HasMpcStorage x, MonadReader x m) => m a

-- | Calculate leaders for the next epoch.
calculateLeaders
    :: Utxo            -- ^ Utxo at the beginning of the epoch
    -> Query (Either FtsError SlotLeaders)
calculateLeaders utxo = do
    mbSeed <- calculateSeed undefined
                            <$> view (lastVer . mpcGlobalCommitments)
                            <*> view (lastVer . mpcGlobalOpenings)
                            <*> view (lastVer . mpcGlobalShares)
    return $ case mbSeed of
        Left e     -> Left e
        Right seed -> Right . V.fromList . map getAddress $
                      followTheSatoshi seed utxo

-- | Check that the secret revealed in the opening matches the secret proof
-- in the commitment
checkOpening
    :: CommitmentsMap -> (PublicKey, Opening) -> Bool
checkOpening globalCommitments (pk, opening) =
    case HM.lookup pk globalCommitments of
        Nothing        -> False
        Just (comm, _) -> verifyOpening comm opening

-- | Check that the decrypted share matches the encrypted share in the
-- commitment
checkShare
    :: CommitmentsMap
    -> VssCertificatesMap
    -> (PublicKey, PublicKey, Share)
    -> Bool
checkShare globalCommitments globalCertificates (pkTo, pkFrom, share) =
    fromMaybe False $ do
        (comm, _) <- HM.lookup pkFrom globalCommitments
        vssKey <- signedValue <$> HM.lookup pkTo globalCertificates
        encShare <- HM.lookup vssKey (commShares comm)
        return $ verifyShare encShare vssKey share

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
    let commitments  = b ^. gbBody . to mbCommitments
        openings     = b ^. gbBody . to mbOpenings
        shares       = b ^. gbBody . to mbShares
        certificates = b ^. gbBody . to mbVssCertificates
    globalCommitments  <- view (lastVer . mpcGlobalCommitments)
    globalOpenings     <- view (lastVer . mpcGlobalOpenings)
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
    --   * check their signatures (which includes checking that the
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
            , (let checkSig (pk, (comm, sig)) = verify pk (epochId, comm) sig
               in all checkSig (HM.toList commitments),
                   "signature check for some commitments has failed")
            , (all (`HM.member` (certificates <> globalCertificates))
                   (HM.keys commitments),
                   "some committing nodes haven't sent a VSS certificate")
            , (all (not . (`HM.member` globalCommitments))
                   (HM.keys commitments),
                   "some nodes have already sent their commitments")
            ]

    -- For openings, we check that
    --   * there are only openings in the block
    --   * the opening isn't present in previous blocks (TODO: may this
    --     check be skipped?)
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
            , (let listShares :: [(PublicKey, PublicKey, Share)]
                   listShares = do
                       (pk1, ss) <- HM.toList shares
                       (pk2, sh) <- HM.toList ss
                       return (pk1, pk2, sh)
               in all (checkShare globalCommitments globalCertificates)
                      listShares,
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

-- TODO: checks can happen anywhere but we must have a *clear* policy on
-- where checks are happening, to prevent the situation when they are, well,
-- not happening anywhere at all.

mpcProcessOpening :: PublicKey -> Opening -> Update ()
mpcProcessOpening pk o = do
    -- TODO: should it be ignored if it's in mpcGlobalOpenings?
    lastVer . mpcLocalOpenings %= HM.insert pk o

mpcProcessCommitment
    :: PublicKey -> (Commitment, CommitmentSignature) -> Update ()
mpcProcessCommitment pk c = do
    -- TODO: should it be ignored if it's in mpcGlobalCommitments?
    lastVer . mpcLocalCommitments %= HM.insert pk c

-- TODO: add mpcProcessCertificate

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
-- We don't have to process genesis blocks as full nodes always generate them
-- by themselves
mpcProcessBlock (Left _) = return ()
-- Main blocks contain commitments, openings, shares, VSS certificates
mpcProcessBlock (Right b) = do
    let blockCommitments  = b ^. gbBody . to mbCommitments
        blockOpenings     = b ^. gbBody . to mbOpenings
        blockShares       = b ^. gbBody . to mbShares
        blockCertificates = b ^. gbBody . to mbVssCertificates
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

-- | Remove elements in 'b' from 'a'
diffDoubleMap
    :: (Eq k1, Eq k2, Hashable k1, Hashable k2)
    => HashMap k1 (HashMap k2 v)
    -> HashMap k1 (HashMap k2 v)
    -> HashMap k1 (HashMap k2 v)
diffDoubleMap a b = HM.filter (not . null) $ HM.unionWith HM.difference a b
