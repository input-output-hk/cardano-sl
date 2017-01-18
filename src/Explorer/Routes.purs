module Explorer.Routes where

import Prelude (($), (<>))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Pux.Router (end, router, lit)
import Control.Alt ((<|>))
import Control.Apply ((<*))

data Route = Dashboard | Transaction | Address | Calculator | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
  Dashboard <$ end
  <|>
  Transaction <$ lit transactionLit <* end
  <|>
  Address <$ lit "address" <* end
  <|>
  Calculator <$ lit "calculator" <* end

toUrl :: Route -> String
toUrl Dashboard = dashboardUrl
toUrl Transaction = transactionUrl
toUrl Address = addressUrl
toUrl Calculator = calculatorUrl
toUrl NotFound = dashboardUrl

litUrl :: String -> String
litUrl = (<>) "/"

dashboardLit :: String
dashboardLit = ""

dashboardUrl :: String
dashboardUrl = litUrl dashboardLit

transactionLit :: String
transactionLit = "transaction"

transactionUrl :: String
transactionUrl = litUrl transactionLit

addressLit :: String
addressLit = "address"

addressUrl :: String
addressUrl = litUrl addressLit

calculatorLit :: String
calculatorLit = "calculator"

calculatorUrl :: String
calculatorUrl = litUrl calculatorLit
