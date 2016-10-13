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
       , processNewBlock
       ) where

import           Control.Lens         (makeClassy, to, view, (%=), (^.))
import           Data.Default         (Default, def)
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as HM (difference, filter, union, unionWith)
import           Data.SafeCopy        (base, deriveSafeCopySimple)
import qualified Data.Vector          as V (fromList)
import           Universum

import           Pos.FollowTheSatoshi (FtsError, calculateSeed, followTheSatoshi)
import           Pos.Types            (Address (getAddress), Block, Body (..),
                                       CommitmentsMap, OpeningsMap, SharesMap,
                                       SlotLeaders, Utxo, gbBody, mbCommitments,
                                       mbOpenings, mbShares)


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

processNewBlock :: Block -> Update ()
-- We don't have to process genesis blocks as full nodes always generate them
-- by themselves
processNewBlock (Left _) = return ()
-- Main blocks contain commitments, openings, and shares
processNewBlock (Right b) = do
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
