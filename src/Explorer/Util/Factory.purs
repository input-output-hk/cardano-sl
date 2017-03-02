module Explorer.Util.Factory where

import Prelude
import Data.Time.NominalDiffTime (mkTime)
import Pos.Explorer.Web.ClientTypes (CHash(..), CTxEntry(..), CTxId(..))
import Pos.Types.Core (Coin(..))


mkCHash :: String -> CHash
mkCHash = CHash

mkCTxId :: String -> CTxId
mkCTxId =
    CTxId <<< mkCHash

mkCoin :: Int -> Coin
mkCoin coin =
  Coin {getCoin: coin}

-- for debugging only
mkCTxEntry :: CTxEntry
mkCTxEntry = CTxEntry
  { cteId: mkCTxId "--"
  , cteTimeIssued: mkTime 0.0
  , cteAmount: mkCoin 0
  }
