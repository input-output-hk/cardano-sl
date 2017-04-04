module Explorer.Routes where

import Prelude
import Control.Alt ((<|>))
import Data.Generic (gShow)
import Data.Int (fromString)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Explorer.Util.Factory (mkCAddress, mkCHash, mkCTxId, mkEpochIndex, mkLocalSlotIndex)
import Network.HTTP.Affjax.Response (ResponseType(..))
import Pos.Core.Types (EpochIndex, LocalSlotIndex)
import Pos.Explorer.Web.ClientTypes (CAddress, CHash, CTxId)
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, _CHash, _CTxId)
import Pux.Router (end, router, lit, str, int, param)

data Route =
    Dashboard
    | Tx CTxId
    | Address CAddress
    | Epoch EpochIndex
    -- | EpochSlot EpochIndex LocalSlotIndex
    | Calculator
    | Block CHash
    | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
    Dashboard <$ end
    <|>
    Tx <<< mkCTxId <$> (lit transactionLit *> str) <* end
    <|>
    Address <<< mkCAddress <$> (lit addressLit *> str) <* end
    <|>
    Epoch <<< mkEpochIndex <$> (lit epochLit *> int) <* end
    <|>
    Calculator <$ lit calculatorLit <* end
    <|>
    Block <<< mkCHash <$> (lit blockLit *> str) <* end
  where
    stringToInt = fromString

toUrl :: Route -> String
toUrl Dashboard = dashboardUrl
toUrl (Tx id) = transactionUrl id
toUrl (Address address) = addressUrl address
toUrl (Epoch epoch) = epochUrl epoch
-- toUrl (EpochSlot epoch slot) = epochSlotUrl epoch slot
toUrl Calculator = calculatorUrl
toUrl (Block hash) = blockUrl hash
toUrl NotFound = dashboardUrl

litUrl :: String -> String
litUrl lit = "/" <> lit <> "/"

dashboardUrl :: String
dashboardUrl = "/"

transactionLit :: String
transactionLit = "tx"

transactionUrl :: CTxId -> String
transactionUrl id = litUrl transactionLit <> id ^. (_CTxId <<< _CHash)

addressLit :: String
addressLit = "address"

addressUrl :: CAddress -> String
addressUrl address = litUrl addressLit <> address ^. _CAddress

epochLit :: String
epochLit = "epoch"

epochUrl :: EpochIndex -> String
epochUrl epoch = litUrl epochLit <> gShow epoch

epochSlotUrl :: EpochIndex -> LocalSlotIndex -> String
epochSlotUrl epoch slot = litUrl epochLit <> gShow epoch <> "?slot=" <> gShow slot

calculatorLit :: String
calculatorLit = "calculator"

calculatorUrl :: String
calculatorUrl = litUrl calculatorLit

blockLit :: String
blockLit = "slot"

blockUrl :: CHash -> String
blockUrl hash = litUrl blockLit <> hash ^. _CHash
