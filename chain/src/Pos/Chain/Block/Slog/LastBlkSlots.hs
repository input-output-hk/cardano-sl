-- | Slog-related types.

module Pos.Chain.Block.Slog.LastBlkSlots
       ( LastBlkSlots
       , LastSlotInfo (..)

       -- * Create LastBlkSlots
       , create
       , fromList

       -- * Access LastBlkSlots components
       , getList
       , lbsCount
       , lbsList
       , lbsMap

       -- * LastBlkSlots operations
       , getKeyCount
       , isFull
       , listLength
       , mapSize
       , totalKeyCount
       , update
       , updateMany
       , updateManyR
       ) where

import           Universum

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Formatting (bprint, build, int, (%))
import qualified Formatting.Buildable as Buildable

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Core (AddressHash, FlatSlotId)
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Crypto (PublicKey (..))


-- Make sure its the actual genesis keys that are being counted.

data LastSlotInfo = LastSlotInfo
    { lsiFlatSlotId       :: !FlatSlotId
    -- ^ The flattened SlotId of this block.
    , lsiLeaderPubkeyHash :: !(AddressHash PublicKey)
    -- ^ The hash of the public key of the slot leader for this slot.
    } deriving (Eq, Show, Generic)

instance Buildable LastSlotInfo where
    build (LastSlotInfo i ahpk) =
        bprint ( "LastSlotInfo "% int %" "% build) i ahpk

instance NFData LastSlotInfo

-- | This type contains 'FlatSlotId's of the blocks whose depth is
-- less than 'blkSecurityParam'. 'FlatSlotId' is chosen in favor of
-- 'SlotId', because the main use case is chain quality calculation,
-- for which flat slot is more convenient.
-- Version 1 of this data type was:
--      type LastBlkSlots = OldestFirst [] FlatSlotId
-- Version 2 of this data type was:
--      data LastBlkSlots = LastBlkSlots
--          { lbsList :: !(OldestFirst [] LastSlotInfo)
--          , lbsMap :: !(Map (AddressHash PublicKey) Int)
--          } deriving (Eq, Show, Generic)
data LastBlkSlots = LastBlkSlots
    { lbsCount :: !Int
    , lbsList  :: !(OldestFirst [] LastSlotInfo)
    , lbsMap   :: !(Map (AddressHash PublicKey) Int)
    } deriving (Eq, Show, Generic)

instance NFData LastBlkSlots

create :: Int -> LastBlkSlots
create k = LastBlkSlots k (OldestFirst []) mempty

getKeyCount :: LastBlkSlots -> AddressHash PublicKey -> Int
getKeyCount lbs key =
    fromMaybe 0 $ Map.lookup key (lbsMap lbs)

totalKeyCount :: LastBlkSlots -> Int
totalKeyCount =
    sum . map snd . Map.toList . lbsMap

isFull :: LastBlkSlots -> Bool
isFull lbs =
    length (getList lbs) == lbsCount lbs

-- | Update LastBlkSlots with a single LastSlotInfo
update :: LastBlkSlots -> LastSlotInfo -> LastBlkSlots
update (LastBlkSlots k (OldestFirst lst) mp) lsi =
    if length lst < k
        then LastBlkSlots k (OldestFirst $ lst ++ [lsi]) (increment mp $ lsiLeaderPubkeyHash lsi)
        else case lst of
                [] -> error "Pos.Chain.Block.Slog.LastBlkSlots: Impossible empty list"
                (x:xs) ->
                    LastBlkSlots k
                        (OldestFirst $ xs ++ [lsi])
                        (increment (decrement mp (lsiLeaderPubkeyHash x)) $ lsiLeaderPubkeyHash lsi)

-- | Update 'LastBlkSlots' with the elements from the list (head first).
updateMany :: LastBlkSlots -> OldestFirst [] LastSlotInfo -> LastBlkSlots
updateMany lbs = List.foldl' update lbs . getOldestFirst

-- | Like 'updateMany` but returns a tuple of the new 'LastBlkSlots' and a list
-- of the elements removed.
updateManyR :: LastBlkSlots -> OldestFirst [] LastSlotInfo -> (LastBlkSlots, OldestFirst [] LastSlotInfo)
updateManyR lbs (OldestFirst xs) =
    let removed = List.take (length xs + listLength lbs - lbsCount lbs) (getList lbs ++ xs)
    in (List.foldl' update lbs xs, OldestFirst removed)

getList :: LastBlkSlots -> [LastSlotInfo]
getList = getOldestFirst . lbsList

listLength :: LastBlkSlots -> Int
listLength = length . lbsList

mapSize :: LastBlkSlots -> Int
mapSize = Map.size . lbsMap

fromList :: Int -> OldestFirst [] LastSlotInfo -> LastBlkSlots
fromList k = List.foldl' update (create k) . getOldestFirst

-- -----------------------------------------------------------------------------
-- Private

increment :: Map (AddressHash PublicKey) Int -> AddressHash PublicKey -> Map (AddressHash PublicKey) Int
increment m k =
    Map.alter incr k m
  where
    incr Nothing  = Just 1
    incr (Just x) = Just $ x + 1

decrement :: Map (AddressHash PublicKey) Int -> AddressHash PublicKey -> Map (AddressHash PublicKey) Int
decrement m k =
    Map.alter decr k m
  where
    decr Nothing = Nothing
    decr (Just x)
        | x > 1 = Just $ x - 1
        | otherwise = Nothing

-- -----------------------------------------------------------------------------
-- TH derived instances at the end of the file.

deriveSimpleBi ''LastSlotInfo [
    Cons 'LastSlotInfo [
        Field [| lsiFlatSlotId :: FlatSlotId |],
        Field [| lsiLeaderPubkeyHash :: AddressHash PublicKey |]
        ]
    ]
