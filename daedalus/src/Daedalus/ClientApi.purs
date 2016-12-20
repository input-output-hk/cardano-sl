module Daedalus.ClientApi where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Promise (Promise, fromAff)
import Daedalus.BackendApi (newAddress, getBalances, getAddresses)
import Data.Either (Either)
import Data.Tuple (Tuple)
import Network.HTTP.Affjax (AJAX)
import Pos.Types.Types (Coin)
import Pos.Wallet.Web.ClientTypes (CAddress)

hello :: String -> Eff (console :: CONSOLE) Unit
hello = log <<< (<>) "Hello "

helloCallback :: forall cb. (String -> cb) -> cb
helloCallback cb = cb "hello"

getAddressesP :: forall eff. Eff(ajax :: AJAX | eff) (Promise (Array CAddress))
getAddressesP = fromAff getAddresses

getBalancesP :: forall eff. Eff(ajax :: AJAX | eff) (Promise (Array (Tuple CAddress Coin)))
getBalancesP = fromAff getBalances

newAddressP :: forall eff. Eff(ajax :: AJAX | eff) (Promise CAddress)
newAddressP = fromAff newAddress
