module Explorer.Util.Factory where

import Prelude
import Pos.Core.Slotting.Types (EpochIndex(..), LocalSlotIndex(..))
import Pos.Explorer.Web.ClientTypes (CAddress(..), CCoin(..), CHash(..), CTxId(..))


mkCHash :: String -> CHash
mkCHash = CHash

mkCTxId :: String -> CTxId
mkCTxId =
    CTxId <<< mkCHash

mkCoin :: String -> CCoin
mkCoin coin =
  CCoin {getCoin: coin}

mkCAddress :: String -> CAddress
mkCAddress = CAddress

mkEpochIndex :: Int -> EpochIndex
mkEpochIndex index = EpochIndex {getEpochIndex: index}

mkLocalSlotIndex :: Int -> LocalSlotIndex
mkLocalSlotIndex index = UncheckedLocalSlotIndex {getSlotIndex: index}
