module Explorer.Util.Factory where

import Prelude
import Data.Lens ((^.))
import Data.Time.NominalDiffTime (mkTime)
import Pos.Core.Types (Coin(..))
import Pos.Explorer.Web.ClientTypes (CAddress(..), CAddressSummary(..), CHash(..), CTxBrief(..), CTxEntry(..), CTxId(..))
import Pos.Explorer.Web.Lenses.ClientTypes (ctbId, ctbTimeIssued)


mkCHash :: String -> CHash
mkCHash = CHash

mkCTxId :: String -> CTxId
mkCTxId =
    CTxId <<< mkCHash

mkCoin :: Int -> Coin
mkCoin coin =
  Coin {getCoin: coin}

mkCAddress :: String -> CAddress
mkCAddress = CAddress

class CTxEntryFactory a where
    mkCTxEntryFrom :: a -> CTxEntry

instance txBriefCTxEntryFactory :: CTxEntryFactory CTxBrief where
    mkCTxEntryFrom (CTxBrief txBrief) = CTxEntry
      { cteId: txBrief ^. ctbId
      , cteTimeIssued: txBrief ^. ctbTimeIssued
      , cteAmount: mkCoin 0 -- TODO(jk) We do need an amount here
      }

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
