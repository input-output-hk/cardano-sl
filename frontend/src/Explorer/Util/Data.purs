module Explorer.Util.Data where

import Prelude
import Data.Array (reverse, sortBy)
import Data.Lens ((^.))
import Data.Maybe (fromMaybe)
import Data.Time.NominalDiffTime (mkTime, unwrapSeconds)
import Explorer.Types.State (CBlockEntries)
import Pos.Explorer.Web.ClientTypes (CBlockEntry(..))
import Pos.Explorer.Web.Lenses.ClientTypes (cbeTimeIssued)

-- | Sort a list of CBlockEntry in an ascending (up) order
sortBlocksByTime :: CBlockEntries -> CBlockEntries
sortBlocksByTime blocks =
    sortBy (comparing time) blocks
    where
        time :: CBlockEntry -> Number
        time (CBlockEntry entry) =
            unwrapSeconds <<< fromMaybe (mkTime 0.0) $ entry ^. cbeTimeIssued

-- | Sort a list of CBlockEntry in an descending (down) order
sortBlocksByTime' :: CBlockEntries -> CBlockEntries
sortBlocksByTime' =
    reverse <<< sortBlocksByTime
