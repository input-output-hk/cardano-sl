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
    | Address CHash
    | Calculator
    | Block CHash
    | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
    Dashboard <$ end
    <|>
    Transaction <<< mkCHash <$> (lit transactionLit *> str) <* end
    <|>
    Address <<< mkCHash <$> (lit addressLit  *> str) <* end
    <|>
    Calculator <$ lit calculatorLit <* end
    <|>
    Block <<< mkCHash <$> (lit blockLit *> str) <* end

toUrl :: Route -> String
toUrl Dashboard = dashboardUrl
toUrl (Transaction hash) = transactionUrl hash
toUrl (Address hash) = addressUrl hash
toUrl Calculator = calculatorUrl
toUrl (Block hash) = blockUrl hash
toUrl NotFound = dashboardUrl

litUrl :: String -> String
litUrl lit = "/" <> lit <> "/"

dashboardUrl :: String
dashboardUrl = "/"

transactionLit :: String
transactionLit = "tx"

transactionUrl :: CHash -> String
transactionUrl hash = litUrl transactionLit <> hash ^. _CHash

addressLit :: String
addressLit = "address"

addressUrl :: CHash -> String
addressUrl hash = litUrl addressLit <> hash ^. _CHash

calculatorLit :: String
calculatorLit = "calculator"

calculatorUrl :: String
calculatorUrl = litUrl calculatorLit

blockLit :: String
blockLit = "block"

blockUrl :: CHash -> String
blockUrl hash = litUrl blockLit <> hash ^. _CHash
