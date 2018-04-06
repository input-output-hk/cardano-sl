{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Wallet info modifier

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
import           Formatting (bprint, build, (%))
import           Serokell.Util (listJson, listJsonIndent)

import           Pos.Client.Txp.History (TxHistoryEntry (..))
import           Pos.Core (Address, HeaderHash)
import           Pos.Core.Txp (TxId)
import           Pos.Txp.Toil (UtxoModifier)
import           Pos.Util.LogSafe (BuildableSafeGen (..), deriveSafeBuildable, secretOnlyF,
                                   secureListF)
import           Pos.Util.Modifier (MapModifier)
import qualified Pos.Util.Modifier as MM

import           Pos.Wallet.Web.Pending.Types (PtxBlockInfo)
import           Pos.Wallet.Web.State         (WAddressMeta)

-- VoidModifier describes a difference between two states.
-- It's (set of added k, set of deleted k) essentially.
type VoidModifier a = MapModifier a ()

data IndexedMapModifier a = IndexedMapModifier
    { immModifier :: MM.MapModifier a Int
    , immCounter  :: Int
    }

sortedInsertions :: IndexedMapModifier a -> [a]
sortedInsertions = map fst . sortWith snd . MM.insertions . immModifier

indexedDeletions :: IndexedMapModifier a -> [a]
indexedDeletions = MM.deletions . immModifier

instance (Eq a, Hashable a) => Semigroup (IndexedMapModifier a) where
    IndexedMapModifier m1 c1 <> IndexedMapModifier m2 c2 =
        IndexedMapModifier (m1 <> fmap (+ c1) m2) (c1 + c2)

instance (Eq a, Hashable a) => Monoid (IndexedMapModifier a) where
    mempty = IndexedMapModifier mempty 0
    mappend = (<>)

data CAccModifier = CAccModifier
    { camAddresses            :: !(IndexedMapModifier WAddressMeta)
    , camUsed                 :: !(VoidModifier (Address, HeaderHash))
    , camChange               :: !(VoidModifier (Address, HeaderHash))
    , camUtxo                 :: !UtxoModifier
    , camAddedHistory         :: !(DList TxHistoryEntry)
    , camDeletedHistory       :: !(DList TxHistoryEntry)
    , camAddedPtxCandidates   :: !(DList (TxId, PtxBlockInfo))
    , camDeletedPtxCandidates :: !(DList (TxId, TxHistoryEntry))
    }

instance Semigroup CAccModifier where
    (CAccModifier a b c d ah dh aptx dptx) <> (CAccModifier a1 b1 c1 d1 ah1 dh1 aptx1 dptx1) =
        CAccModifier (a <> a1) (b <> b1) (c <> c1) (d <> d1) (ah1 <> ah)
                     (dh <> dh1) (aptx <> aptx1) (dptx <> dptx1)

instance Monoid CAccModifier where
    mempty = CAccModifier mempty mempty mempty mempty mempty mempty mempty mempty
    mappend = (<>)

instance BuildableSafeGen CAccModifier where
    buildSafeGen sl CAccModifier{..} =
        bprint
            ( "\n    added addresses: "%secureListF sl (listJsonIndent 8)
            %",\n    deleted addresses: "%secureListF sl (listJsonIndent 8)
            %",\n    used addresses: "%secureListF sl listJson
            %",\n    change addresses: "%secureListF sl listJson
            %",\n    local utxo (difference): "%secretOnlyF sl build
            %",\n    added history entries: "%secureListF sl (listJsonIndent 8)
            %",\n    deleted history entries: "%secureListF sl (listJsonIndent 8)
            %",\n    added pending candidates: "%secureListF sl listJson
            %",\n    deleted pending candidates: "%secureListF sl listJson)
        (sortedInsertions camAddresses)
        (indexedDeletions camAddresses)
        (map (fst . fst) $ MM.insertions camUsed)
        (map (fst . fst) $ MM.insertions camChange)
        camUtxo
        camAddedHistory
        camDeletedHistory
        (map fst camAddedPtxCandidates)
        (map fst camDeletedPtxCandidates)

deriveSafeBuildable ''CAccModifier

-- | `txMempoolToModifier`, once evaluated, is passed around under this type in
-- scope of single request.
type CachedCAccModifier = CAccModifier

----------------------------------------------------------------------------
-- Funcs
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


insertIMM
    :: (Eq a, Hashable a)
    => a -> IndexedMapModifier a -> IndexedMapModifier a
insertIMM k IndexedMapModifier {..} =
    IndexedMapModifier
    { immModifier = MM.insert k immCounter immModifier
    , immCounter  = immCounter + 1
    }

deleteIMM
    :: (Eq a, Hashable a)
    => a -> IndexedMapModifier a -> IndexedMapModifier a
deleteIMM k IndexedMapModifier {..} =
    IndexedMapModifier
    { immModifier = deleteNotDeep k immModifier
    , ..
    }

deleteAndInsertIMM
    :: (Eq a, Hashable a)
    => [a] -> [a] -> IndexedMapModifier a -> IndexedMapModifier a
deleteAndInsertIMM dels ins mapModifier =
    -- Insert CWAddressMeta coressponding to outputs of tx.
    (\mm -> foldl' (flip insertIMM) mm ins) $
    -- Delete CWAddressMeta coressponding to inputs of tx.
    foldl' (flip deleteIMM) mapModifier dels

deleteAndInsertVM :: (Eq a, Hashable a) => [a] -> [a] -> VoidModifier a -> VoidModifier a
deleteAndInsertVM dels ins mapModifier = deleteAndInsertMM dels (zip ins $ repeat ()) mapModifier

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
