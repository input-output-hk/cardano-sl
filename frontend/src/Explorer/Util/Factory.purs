module Explorer.Util.Factory where

import Prelude
import Data.Time.NominalDiffTime (mkTime)
import Pos.Core.Types (EpochIndex(..), LocalSlotIndex(..))
import Pos.Explorer.Web.ClientTypes (CAddress(..), CAddressSummary(..), CAddressType(..), CCoin(..), CHash(..), CTxEntry(..), CTxId(..))


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
    , caType: CUnknownAddress
    , caTxNum: 0
    , caBalance: mkCoin 0
    , caTxList: []
    }
