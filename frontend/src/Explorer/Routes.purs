module Explorer.Routes where

import Prelude
import Control.Alt ((<|>))
import Data.Lens ((^.))
import Data.Maybe (fromMaybe)
import Explorer.Util.Factory (mkCAddress, mkCHash, mkCTxId, mkEpochIndex, mkLocalSlotIndex)
import Pos.Core.Lenses.Types (_EpochIndex, _LocalSlotIndex, getEpochIndex, getSlotIndex)
import Pos.Core.Types (EpochIndex, LocalSlotIndex)
import Pos.Explorer.Web.ClientTypes (CAddress, CHash, CTxId)
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, _CHash, _CTxId)
import Pux.Router (end, int, lit, router, str)

data Route
    = Dashboard
    | Tx CTxId
    | Address CAddress
    | EpochSlot EpochIndex LocalSlotIndex
    | Epoch EpochIndex
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
    EpochSlot <$> mkEpochIndex <$> (lit epochLit *> int)
              <*> (mkLocalSlotIndex <$> (lit slotLit *> int) <* end)
    <|>
    Epoch <<< mkEpochIndex <$> (lit epochLit *> int) <* end
    <|>
    Calculator <$ (lit calculatorLit) <* end
    <|>
    Block <<< mkCHash <$> (lit slotLit *> str) <* end

toUrl :: Route -> String
toUrl Dashboard = dashboardUrl
toUrl (Tx id) = transactionUrl id
toUrl (Address address) = addressUrl address
toUrl (EpochSlot epoch slot) = epochSlotUrl epoch slot
toUrl (Epoch epoch) = epochUrl epoch
toUrl Calculator = calculatorUrl
toUrl (Block hash) = blockUrl hash
toUrl NotFound = notFoundUrl

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

slotLit :: String
slotLit = "slot"

epochUrl :: EpochIndex -> String
epochUrl epoch = litUrl epochLit <> paramToString epoch

epochSlotUrl :: EpochIndex -> LocalSlotIndex -> String
epochSlotUrl epoch slot = litUrl epochLit
                          <> paramToString epoch
                          <> litUrl slotLit
                          <> paramToString slot

calculatorLit :: String
calculatorLit = "calculator"

calculatorUrl :: String
calculatorUrl = "/" <> calculatorLit

blockUrl :: CHash -> String
blockUrl hash = litUrl slotLit <> hash ^. _CHash

notFoundLit :: String
notFoundLit = "404"

notFoundUrl :: String
notFoundUrl = "/" <> notFoundLit

class RouteParams a where
    paramToString :: a -> String

instance epochIndexRouteParams :: RouteParams EpochIndex where
    paramToString epoch =
        show (epoch ^. (_EpochIndex <<< getEpochIndex))

instance localSlotIndexRouteParams :: RouteParams LocalSlotIndex where
    paramToString slot =
        show (slot ^. (_LocalSlotIndex <<< getSlotIndex))
