module Explorer.Routes where

import Prelude
import Control.Alt ((<|>))
import Data.Lens ((^.))
import Data.Maybe (fromMaybe)
import Explorer.Util.Factory (mkCAddress, mkCHash, mkCTxId)
import Pos.Explorer.Web.ClientTypes (CAddress, CHash, CTxId)
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, _CHash, _CTxId)
import Pux.Router (end, router, lit, str)

data Route =
    Dashboard
    | Tx CTxId
    | Address CAddress
    | Calculator
    | Block CHash
    | Playground
    | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
    Dashboard <$ end
    <|>
    Tx <<< mkCTxId <$> (lit transactionLit *> str) <* end
    <|>
    Address <<< mkCAddress <$> (lit addressLit  *> str) <* end
    <|>
    Calculator <$ lit calculatorLit <* end
    <|>
    Block <<< mkCHash <$> (lit blockLit *> str) <* end
    <|>
    -- TODO (jk) Disable Playground route in production mode
    -- It is just for debugging
    Playground <$ lit playgroundLit <* end

toUrl :: Route -> String
toUrl Dashboard = dashboardUrl
toUrl (Tx id) = transactionUrl id
toUrl (Address address) = addressUrl address
toUrl Calculator = calculatorUrl
toUrl (Block hash) = blockUrl hash
toUrl Playground = playgroundUrl
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

calculatorLit :: String
calculatorLit = "calculator"

calculatorUrl :: String
calculatorUrl = litUrl calculatorLit

blockLit :: String
blockLit = "slot"

blockUrl :: CHash -> String
blockUrl hash = litUrl blockLit <> hash ^. _CHash

playgroundLit :: String
playgroundLit = "playground"

playgroundUrl :: String
playgroundUrl = litUrl playgroundLit
