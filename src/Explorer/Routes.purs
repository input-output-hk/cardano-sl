module Explorer.Routes where

import Prelude
import Control.Alt ((<|>))
import Data.Lens ((^.))
import Data.Maybe (fromMaybe)
import Explorer.Util.Factory (mkCAddress, mkCHash)
import Pos.Explorer.Web.ClientTypes (CAddress, CHash, _CAddress)
import Pos.Explorer.Web.Lenses.ClientTypes (_CHash)
import Pux.Router (end, router, lit, str)

data Route =
    Dashboard
    | Transaction CHash
    | Address CAddress
    | Calculator
    | Block CHash
    | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
    Dashboard <$ end
    <|>
    Transaction <<< mkCHash <$> (lit transactionLit *> str) <* end
    <|>
    Address <<< mkCAddress <$> (lit addressLit  *> str) <* end
    <|>
    Calculator <$ lit calculatorLit <* end
    <|>
    Block <<< mkCHash <$> (lit blockLit *> str) <* end

toUrl :: Route -> String
toUrl Dashboard = dashboardUrl
toUrl (Transaction hash) = transactionUrl hash
toUrl (Address address) = addressUrl address
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
