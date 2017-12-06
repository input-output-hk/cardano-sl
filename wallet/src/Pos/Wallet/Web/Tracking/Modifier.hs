{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | A module which contains definition of wallet state modifier
-- and helper functions to use it.
module Pos.Wallet.Web.Tracking.Modifier
       ( CAccModifier (..)
       , CachedCAccModifier

       , VoidModifier
       , deleteAndInsertVM
       , deleteAndInsertMM

       , IndexedMapModifier (..)
       , sortedInsertions
       , indexedDeletions
       , insertIMM
       , deleteIMM
       , deleteAndInsertIMM
       ) where

import           Universum

import           Data.DList (DList)
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import           Serokell.Util (listJson, listJsonIndent)

import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Core (HeaderHash)
import           Pos.Core.Txp (TxId)
import           Pos.Txp.Toil (UtxoModifier)
import           Pos.Util.Modifier (MapModifier)
import qualified Pos.Util.Modifier as MM

import           Pos.Wallet.Web.ClientTypes (Addr, CId, CWAddressMeta)
import           Pos.Wallet.Web.Pending.Types (PtxBlockInfo)

-- | 'VoidModifier' represents a difference between two sets.
-- It's @(<set of added k>, <set of deleted k>)@ essentially.
type VoidModifier a = MapModifier a ()

-- | 'IndexedMapModifier' represents a difference between two sets,
-- where added elements are ordered in a specific way.
data IndexedMapModifier a = IndexedMapModifier
    { immModifier :: MM.MapModifier a Int
    , immCounter  :: Int
    }

-- | Get all insertions from 'IndexedMapModifier' in order of indices.
sortedInsertions :: IndexedMapModifier a -> [a]
sortedInsertions = map fst . sortWith snd . MM.insertions . immModifier

-- | Get all deletions from 'IndexedMapModifier'.
indexedDeletions :: IndexedMapModifier a -> [a]
indexedDeletions = MM.deletions . immModifier

-- This monoid instance supports this invariant:
-- prop> sortedInsertions a ++ sortedInsertions b == sortedInsertions (a <> b)
instance (Eq a, Hashable a) => Monoid (IndexedMapModifier a) where
    mempty = IndexedMapModifier mempty 0
    IndexedMapModifier m1 c1 `mappend` IndexedMapModifier m2 c2 =
        IndexedMapModifier (m1 <> fmap (+ c1) m2) (c1 + c2)

-- | 'CAccModifier' is a sum of all modifications which may be
-- applied to wallet DB after a set of transactions is applied.
-- Used for accumulating changes to wallet DB from new blocks and mempool.
-- See "Pos.Wallet.Web.Tracking.Sync" for examples of using.
data CAccModifier = CAccModifier
    { camAddresses            :: !(IndexedMapModifier CWAddressMeta)
      -- ^ Changes to set of all existing addresses in wallet.
    , camUsed                 :: !(VoidModifier (CId Addr, HeaderHash))
      -- ^ Changes to set of used addresses in wallet.
    , camChange               :: !(VoidModifier (CId Addr, HeaderHash))
      -- ^ Changes to set of change addresses in wallet.
    , camUtxo                 :: !UtxoModifier
      -- ^ Changes to common 'Utxo' cache.
    , camAddedHistory         :: !(DList TxHistoryEntry)
      -- ^ Transactions which should be added in block history
      -- cache, oldest first.
    , camDeletedHistory       :: !(DList TxHistoryEntry)
      -- ^ Transactions which should be removed from block history
      -- cache, newest first. Computed only during rollback processing.
      -- TODO: as soon as block history cache is stored as map now,
      -- we may combine 'camAddedHistory' and 'camDeletedHistory'
      -- using one 'MapModifier'.
    , camAddedPtxCandidates   :: !(DList (TxId, PtxBlockInfo))
      -- ^ List of transactions, resubmission status of which should
      -- be changed to 'PtxInNewestBlocks'. Computed only during
      -- block application.
      -- TODO: it's more semantically valid to store this data in
      -- 'Map'.
    , camDeletedPtxCandidates :: !(DList (TxId, TxHistoryEntry))
      -- ^ List of transactions which should be resubmitted again.
      -- Computed only during rollback, only transactions which were
      -- in rolled back blocks are up to resubmission.
      -- TODO: it's more semantically valid to store this data in
      -- 'Map'.
    }

-- Note that 'camAddedHistory' fields are combined in different order
-- than all other fields, because it's the only list which is stored
-- in newest first order.
instance Monoid CAccModifier where
    mempty = CAccModifier mempty mempty mempty mempty mempty mempty mempty mempty
    (CAccModifier a b c d ah dh aptx dptx) `mappend` (CAccModifier a1 b1 c1 d1 ah1 dh1 aptx1 dptx1) =
        CAccModifier (a <> a1) (b <> b1) (c <> c1) (d <> d1) (ah1 <> ah) (dh <> dh1) (aptx <> aptx1) (dptx <> dptx1)

instance Buildable CAccModifier where
    build CAccModifier{..} =
        bprint
            ( "\n    added addresses: "%listJsonIndent 8
            %",\n    deleted addresses: "%listJsonIndent 8
            %",\n    used addresses: "%listJson
            %",\n    change addresses: "%listJson
            %",\n    local utxo (difference): "%build
            %",\n    added history entries: "%listJsonIndent 8
            %",\n    deleted history entries: "%listJsonIndent 8
            %",\n    added pending candidates: "%listJson
            %",\n    deleted pending candidates: "%listJson)
        (sortedInsertions camAddresses)
        (indexedDeletions camAddresses)
        (map (fst . fst) $ MM.insertions camUsed)
        (map (fst . fst) $ MM.insertions camChange)
        camUtxo
        camAddedHistory
        camDeletedHistory
        (map fst camAddedPtxCandidates)
        (map fst camDeletedPtxCandidates)

-- | `txMempoolToModifier`, once evaluated, is passed around under this type in
-- scope of single request.
type CachedCAccModifier = CAccModifier

----------------------------------------------------------------------------
-- Utility functions
----------------------------------------------------------------------------

-- | This function is alternative for MapModifier's @delete@.
-- It doesn't add removable element to delete set
-- if it was inserted before (in contrast with @delete@)
deleteNotDeep :: (Eq k, Hashable k) => k -> MapModifier k v -> MapModifier k v
deleteNotDeep = MM.alter alterDelF
  where
    alterDelF :: MM.KeyState v -> MM.KeyState v
    alterDelF MM.KeyNotFound     = MM.KeyDeleted
    alterDelF MM.KeyDeleted      = MM.KeyDeleted
    alterDelF (MM.KeyInserted _) = MM.KeyNotFound

-- | Appends new insertion to 'IndexedMapModifier'.
insertIMM
    :: (Eq a, Hashable a)
    => a -> IndexedMapModifier a -> IndexedMapModifier a
insertIMM k IndexedMapModifier {..} =
    IndexedMapModifier
    { immModifier = MM.insert k immCounter immModifier
    , immCounter  = immCounter + 1
    }

-- | Add a deletion into 'IndexedMapModifier' using 'deleteNotDeep'.
deleteIMM
    :: (Eq a, Hashable a)
    => a -> IndexedMapModifier a -> IndexedMapModifier a
deleteIMM k IndexedMapModifier {..} =
    IndexedMapModifier
    { immModifier = deleteNotDeep k immModifier
    , ..
    }

-- | Batch deletion\/insertion from\/to 'IndexedMapModifier'.
deleteAndInsertIMM
    :: (Eq a, Hashable a)
    => [a] -> [a] -> IndexedMapModifier a -> IndexedMapModifier a
deleteAndInsertIMM dels ins mapModifier =
    -- Insert CWAddressMeta coressponding to outputs of tx.
    (\mm -> foldl' (flip insertIMM) mm ins) $
    -- Delete CWAddressMeta coressponding to inputs of tx.
    foldl' (flip deleteIMM) mapModifier dels

-- | Batch deletion\/insertion from\/to 'VoidModifier'.
deleteAndInsertVM :: (Eq a, Hashable a) => [a] -> [a] -> VoidModifier a -> VoidModifier a
deleteAndInsertVM dels ins mapModifier = deleteAndInsertMM dels (zip ins $ repeat ()) mapModifier

-- | Batch deletion\/insertion from\/to basic 'MapModifier'.
deleteAndInsertMM :: (Eq k, Hashable k) => [k] -> [(k, v)] -> MM.MapModifier k v -> MM.MapModifier k v
deleteAndInsertMM dels ins mapModifier =
    -- Insert CWAddressMeta coressponding to outputs of tx (2)
    (\mm -> foldl' insertAcc mm ins) $
    -- Delete CWAddressMeta coressponding to inputs of tx (1)
    foldl' deleteAcc mapModifier dels
  where
    insertAcc :: (Hashable k, Eq k) => MapModifier k v -> (k, v) -> MapModifier k v
    insertAcc modifier (k, v) = MM.insert k v modifier

    deleteAcc :: (Hashable k, Eq k) => MapModifier k v -> k -> MapModifier k v
    deleteAcc = flip deleteNotDeep

