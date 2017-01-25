module Explorer.Routes where

import Prelude (($), (<>))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Pux.Router (end, router, lit)
import Control.Alt ((<|>))
import Control.Apply ((<*))

data Route =
    Dashboard
    | Transaction
    | Address
    | Calculator
    | Block
    | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
  Dashboard <$ end
  <|>
  Transaction <$ lit transactionLit <* end
  <|>
  Address <$ lit addressLit <* end
  <|>
  Calculator <$ lit calculatorLit <* end
  <|>
  Block <$ lit "block" <* end

toUrl :: Route -> String
toUrl Dashboard = dashboardUrl
toUrl Transaction = transactionUrl
toUrl Address = addressUrl
toUrl Calculator = calculatorUrl
toUrl Block = blockUrl
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

blockLit :: String
blockLit = "block"

blockUrl :: String
blockUrl = litUrl blockLit
