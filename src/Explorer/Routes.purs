module Explorer.Routes where

import Prelude
import Data.Maybe (fromMaybe)
import Pux.Router (end, router, lit, str)
import Control.Alt ((<|>))
import Pos.Explorer.Web.ClientTypes (CHash)
import Pos.Explorer.Web.Lenses.ClientTypes (_CHash)
import Data.Lens ((^.))
import Explorer.Util.Factory (mkCHash)

data Route =
    Dashboard
    | Transaction CHash
    | Address
    | Calculator
    | Block
    | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
    Dashboard <$ end
    <|>
    Transaction <<< mkCHash <$> (lit transactionLit *> str) <* end
    <|>
    Address <$ lit addressLit <* end
    <|>
    Calculator <$ lit calculatorLit <* end
    <|>
    Block <$ lit "block" <* end

toUrl :: Route -> String
toUrl Dashboard = dashboardUrl
toUrl (Transaction hash) = transactionUrl hash
toUrl Address = addressUrl
toUrl Calculator = calculatorUrl
toUrl Block = blockUrl
toUrl NotFound = dashboardUrl

litUrl :: String -> String
litUrl lit = "/" <> lit <> "/"

dashboardLit :: String
dashboardLit = ""

dashboardUrl :: String
dashboardUrl = litUrl dashboardLit

transactionLit :: String
transactionLit = "tx"

transactionUrl :: CHash -> String
transactionUrl hash = litUrl transactionLit <> hash ^. _CHash

addressLit :: String
addressLit = "address"

addressUrl :: String
addressUrl = litUrl addressLit

calculatorLit :: String
calculatorLit = "calculator"

calculatorUrl :: String
calculatorUrl = litUrl calculatorLit

blockLit :: String
blockLit = "block"

blockUrl :: String
blockUrl = litUrl blockLit
