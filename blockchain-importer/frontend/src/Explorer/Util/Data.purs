module Explorer.Util.Data where

import Prelude
import Data.Array (reverse, sortBy, unionBy)
import Data.Lens ((^.))
import Data.Maybe (fromMaybe)
import Data.Time.NominalDiffTime (mkTime, unwrapSeconds)
import Explorer.State (maxSlotInEpoch)
import Explorer.Types.State (CBlockEntries, CTxEntries)
import Pos.Explorer.Web.ClientTypes (CBlockEntry(..), CTxEntry(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CBlockEntry, _CHash, _CTxEntry, _CTxId, cbeBlkHash, cbeEpoch, cbeSlot, cbeTimeIssued, cteId, cteTimeIssued)

-- | Sort a list of CBlockEntry by time in an ascending (up) order
sortBlocksByTime :: CBlockEntries -> CBlockEntries
sortBlocksByTime blocks =
    sortBy (comparing time) blocks
    where
        time :: CBlockEntry -> Number
        time (CBlockEntry entry) =
            unwrapSeconds <<< fromMaybe (mkTime 0.0) $ entry ^. cbeTimeIssued

-- | Sort a list of CBlockEntry by time in a descending (down) order
sortBlocksByTime' :: CBlockEntries -> CBlockEntries
sortBlocksByTime' =
    reverse <<< sortBlocksByTime


-- | Sort CBlockEntries by epochs and slots an ascending (up) order
sortBlocksByEpochSlot :: CBlockEntries -> CBlockEntries
sortBlocksByEpochSlot blocks =
    sortBy (comparing epochsAndSlots) blocks
    where
        epochsAndSlots :: CBlockEntry -> Int
        epochsAndSlots (CBlockEntry entry) =
            ((entry ^. cbeEpoch) * maxSlotInEpoch) + (entry ^. cbeSlot)

-- | Sort CBlockEntries by epochs and slots a descending (down) order
sortBlocksByEpochSlot' :: CBlockEntries -> CBlockEntries
sortBlocksByEpochSlot' =
    reverse <<< sortBlocksByEpochSlot


-- | Helper to remove duplicates of blocks by comparing its CHash
-- |
-- | _Note:_  To "union" current with new blocks we have to compare CBlockEntry
-- | Because we don't have an Eq instance of generated CBlockEntry's
-- | As a workaround we do have to compare CBlockEntry by its hash
unionBlocks :: CBlockEntries -> CBlockEntries -> CBlockEntries
unionBlocks blocksA blocksB =
    unionBy (\b1 b2 -> getHash b1 == getHash b2) blocksA blocksB
    where
        getHash :: CBlockEntry -> String
        getHash block = block ^. (_CBlockEntry <<< cbeBlkHash <<< _CHash)



-- | Sort a list of `CTxEntries` by time in an ascending (up) order.
-- | Txs that have no time (the time couldn't be calculated) will come first.
sortTxsByTime :: CTxEntries -> CTxEntries
sortTxsByTime txs =
    sortBy (comparing time) txs
    where
        time :: CTxEntry -> Number
        time (CTxEntry entry) =
            unwrapSeconds <<< fromMaybe (mkTime 0.0) $ entry ^. cteTimeIssued

-- | Sort a list of `CTxEntry` by time in a descending (down) order
sortTxsByTime' :: CTxEntries -> CTxEntries
sortTxsByTime' =
    reverse <<< sortTxsByTime

-- | Helper to remove duplicates of blocks by comparing its CHash
-- |
-- | Note:  To "union" current with new `txs` we have to compare CTxEntry
-- | Because we don't have an Eq instance of generated CTxEntry's
-- | As a workaround we do have to compare CTxEntry by its id
unionTxs :: CTxEntries -> CTxEntries -> CTxEntries
unionTxs =
    unionBy (\tx1 tx2 -> getId tx1 == getId tx2)
    where
      getId tx = tx ^. (_CTxEntry <<< cteId <<< _CTxId <<< _CHash)
