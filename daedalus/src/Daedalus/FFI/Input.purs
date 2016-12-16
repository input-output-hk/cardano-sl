module Deadalus.FFI.Input where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Promise (Promise, fromAff)
import Daedalus.Api (getAddresses)
import Data.Either (Either)
import Network.HTTP.Affjax (AJAX)
import Pos.Wallet.Web.ClientTypes (CAddress)

hello :: String -> Eff (console :: CONSOLE) Unit
hello = log <<< (<>) "Hello "

helloCallback :: forall cb. (String -> cb) -> cb
helloCallback cb = cb "hello"

getAddressesP :: forall eff. Eff(ajax :: AJAX | eff) (Promise (Either String(Array CAddress)))
getAddressesP = fromAff getAddresses
