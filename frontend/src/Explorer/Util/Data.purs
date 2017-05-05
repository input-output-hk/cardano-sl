module Explorer.Util.Data where

import Prelude
import Data.Array (reverse, sortBy)
import Data.Lens ((^.))
import Data.Maybe (fromMaybe)
import Data.Time.NominalDiffTime (mkTime, unwrapSeconds)
import Explorer.State (maxSlotInEpoch)
import Explorer.Types.State (CBlockEntries)
import Pos.Explorer.Web.ClientTypes (CBlockEntry(..))
import Pos.Explorer.Web.Lenses.ClientTypes (cbeEpoch, cbeSlot, cbeTimeIssued)

-- | Sort a list of CBlockEntry by time in an ascending (up) order
sortBlocksByTime :: CBlockEntries -> CBlockEntries
sortBlocksByTime blocks =
    sortBy (comparing time) blocks
    where
        time :: CBlockEntry -> Number
        time (CBlockEntry entry) =
            unwrapSeconds <<< fromMaybe (mkTime 0.0) $ entry ^. cbeTimeIssued

-- | Sort a list of CBlockEntry by time in an descending (down) order
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
