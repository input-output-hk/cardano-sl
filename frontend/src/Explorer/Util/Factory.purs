module Explorer.Util.Factory where

import Prelude
import Data.Lens (set)
import Data.Maybe (Maybe(..))
import Data.Time.NominalDiffTime (NominalDiffTime, mkTime)
import Pos.Core.Types (EpochIndex(..), LocalSlotIndex(..))
import Pos.Explorer.Web.ClientTypes (CAddress(..), CAddressSummary(..), CBlockEntry(..), CCoin(..), CHash(..), CTxEntry(..), CTxId(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CBlockEntry, cbeTimeIssued)


mkCHash :: String -> CHash
mkCHash = CHash

mkCTxId :: String -> CTxId
mkCTxId =
    CTxId <<< mkCHash

mkCoin :: Int -> CCoin
mkCoin coin =
  CCoin {getCoin: show coin}

mkCAddress :: String -> CAddress
mkCAddress = CAddress

mkEpochIndex :: Int -> EpochIndex
mkEpochIndex index = EpochIndex {getEpochIndex: index}

mkLocalSlotIndex :: Int -> LocalSlotIndex
mkLocalSlotIndex index = LocalSlotIndex {getSlotIndex: index}

-- All the following helper function `mkEmpty**` are for debugging only
-- We do need these to mock live data
-- It can be removed if all endpoints are ready


mkEmptyCTxEntry :: CTxEntry
mkEmptyCTxEntry = CTxEntry
    { cteId: mkCTxId "--"
    , cteTimeIssued: mkTime 0.0
    , cteAmount: mkCoin 0
    }

mkEmptyCAddressSummary :: CAddressSummary
mkEmptyCAddressSummary = CAddressSummary
    { caAddress: mkCAddress "--"
    , caTxNum: 0
    , caBalance: mkCoin 0
    , caTxList: []
    }

mkCBlockEntry :: CBlockEntry
mkCBlockEntry = CBlockEntry
    { cbeEpoch: 0
    , cbeSlot: 0
    , cbeBlkHash: mkCHash "0"
    , cbeTimeIssued: Nothing
    , cbeTxNum: 0
    , cbeTotalSent: mkCoin 0
    , cbeSize: 0
    , cbeRelayedBy: Nothing
    }

mkCBlockEntryByTime :: NominalDiffTime -> CBlockEntry
mkCBlockEntryByTime time =
    set (_CBlockEntry <<< cbeTimeIssued) (Just time) mkCBlockEntry
