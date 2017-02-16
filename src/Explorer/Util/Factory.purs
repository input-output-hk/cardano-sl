module Explorer.Util.Factory where

import Prelude
import Pos.Explorer.Web.ClientTypes (CTxId(..), CHash(..))


mkCHash :: String -> CHash
mkCHash = CHash

mkCTxId :: String -> CTxId
mkCTxId =
    CTxId <<< mkCHash
