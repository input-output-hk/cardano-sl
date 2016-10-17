{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

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

import           Control.Lens         (makeClassy, to, view, (%=), (^.))
import           Data.Default         (Default, def)
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as HM (difference, filter, insert, union, unionWith)
import           Data.SafeCopy        (base, deriveSafeCopySimple)
import qualified Data.Vector          as V (fromList)
import           Serokell.Util.Verify (VerificationRes)
import           Universum

import           Pos.Crypto           (PublicKey)
import           Pos.FollowTheSatoshi (FtsError, calculateSeed, followTheSatoshi)
import           Pos.Types            (Address (getAddress), Block, Body (..), Commitment,
                                       CommitmentSignature, CommitmentsMap, Opening,
                                       OpeningsMap, SharesMap, SlotLeaders, Utxo, gbBody,
                                       mbCommitments, mbOpenings, mbShares)


data MpcStorage = MpcStorage
    { -- | Local set of 'Commitment's. These are valid commitments which are
      -- known to the node and not stored in blockchain. It is useful only
      -- for the first 'k' slots, after that it should be discarded.
      _mpcLocalCommitments  :: !CommitmentsMap
    , -- | Set of 'Commitment's stored in blocks for current epoch. This can
      -- be calculated by 'mconcat'ing stored commitments, but it would be
      -- inefficient to do it every time we need to know if commitments is
      -- stored in blocks.
      _mpcGlobalCommitments :: !CommitmentsMap
    , -- | Local set of decrypted shares (encrypted shares are stored in
      -- commitments).
      _mpcLocalShares       :: !SharesMap
    , -- | Decrypted shares stored in blocks
      _mpcGlobalShares      :: !SharesMap
    , -- | Local set of openings
      _mpcLocalOpenings     :: !OpeningsMap
    , -- | Openings stored in blocks
      _mpcGlobalOpenings    :: !OpeningsMap }

makeClassy ''MpcStorage
deriveSafeCopySimple 0 'base ''MpcStorage

instance Default MpcStorage where
    def =
        MpcStorage
        { _mpcLocalCommitments = mempty
        , _mpcGlobalCommitments = mempty
        , _mpcLocalShares = mempty
        , _mpcGlobalShares = mempty
        , _mpcLocalOpenings = mempty
        , _mpcGlobalOpenings = mempty
        }

type Update a = forall m x. (HasMpcStorage x, MonadState x m) => m a
type Query a = forall m x. (HasMpcStorage x, MonadReader x m) => m a

-- | Calculate leaders for the next epoch.
calculateLeaders
    :: Utxo            -- ^ Utxo at the beginning of the epoch
    -> Query (Either FtsError SlotLeaders)
calculateLeaders utxo = do
    mbSeed <- calculateSeed <$> view mpcGlobalCommitments
                            <*> view mpcGlobalOpenings
                            <*> view mpcGlobalShares
    return $ case mbSeed of
        Left e     -> Left e
        Right seed -> Right . V.fromList . map getAddress $
                      followTheSatoshi seed utxo

-- | Verify MPC-related predicates of a single block, also using data
-- stored in MpcStorage.
-- The following checks are performed:
-- 1. Every MPC message is stored in block, whose SlotId permits such message.
-- 2. Every MPC message in block is correct (this check depends on message and
-- may be empty).
mpcVerifyBlock :: Block -> Query VerificationRes
mpcVerifyBlock = notImplemented

-- | Verify MPC-related predicates of blocks sequence which is about
-- to be applied. It should check that MPC messages will be consistent
-- if this blocks are applied (after possible rollback).
mpcVerifyBlocks :: Int -> [Block] -> Query VerificationRes
mpcVerifyBlocks toRollback = notImplemented

-- TODO: checks can happen anywhere but we must have a *clear* policy on
-- where checks are happening, to prevent the situation when they are, well,
-- not happening anywhere at all.

mpcProcessOpening :: PublicKey -> Opening -> Update ()
mpcProcessOpening pk o = do
    -- TODO: should it be ignored if it's in mpcGlobalOpenings?
    mpcLocalOpenings %= HM.insert pk o

mpcProcessCommitment
    :: PublicKey -> (Commitment, CommitmentSignature) -> Update ()
mpcProcessCommitment pk c = do
    -- TODO: should it be ignored if it's in mpcGlobalCommitments?
    mpcLocalCommitments %= HM.insert pk c

-- | Apply sequence of blocks to state. Sequence must be based on last
-- applied block and must be valid.
mpcApplyBlocks :: [Block] -> Update ()
mpcApplyBlocks = notImplemented

-- | Rollback application of last `n` blocks.
mpcRollback :: Int -> Update ()
mpcRollback = notImplemented

mpcProcessBlock :: Block -> Update ()
-- We don't have to process genesis blocks as full nodes always generate them
-- by themselves
mpcProcessBlock (Left _) = return ()
-- Main blocks contain commitments, openings, and shares
mpcProcessBlock (Right b) = do
    let blockCommitments = b ^. gbBody . to mbCommitments
        blockOpenings    = b ^. gbBody . to mbOpenings
        blockShares      = b ^. gbBody . to mbShares
    -- commitments
    mpcGlobalCommitments %= HM.union blockCommitments
    mpcLocalCommitments  %= (`HM.difference` blockCommitments)
    -- openings
    mpcGlobalOpenings %= HM.union blockOpenings
    mpcLocalOpenings  %= (`HM.difference` blockOpenings)
    -- shares
    mpcGlobalShares %= HM.unionWith HM.union blockShares
    mpcLocalShares  %= (`diffDoubleMap` blockShares)

-- | Remove elements in 'b' from 'a'
diffDoubleMap
    :: (Eq k1, Eq k2, Hashable k1, Hashable k2)
    => HashMap k1 (HashMap k2 v)
    -> HashMap k1 (HashMap k2 v)
    -> HashMap k1 (HashMap k2 v)
diffDoubleMap a b = HM.filter (not . null) $ HM.unionWith HM.difference a b
