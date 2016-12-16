module Daedalus.BackendApi where

import Prelude
import Control.Monad.Aff (attempt, Aff)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Either (either, Either(Left))
import Data.Generic (class Generic)
import Data.HTTP.Method (Method(POST))
import Data.Tuple (Tuple)
import Network.HTTP.Affjax (affjax, defaultRequest, AJAX, get)
import Pos.Types.Types (Coin)
import Pos.Wallet.Web.ClientTypes (CAddress)

-- TODO: remove traces, they are adding to increase verbosity in development
makeRequest :: forall eff a. (Generic a) => String -> Aff (ajax :: AJAX | eff) (Either String a)
makeRequest url = do
  res <- attempt $ get url
  pure $ either (Left <<< show) decodeResult res

decodeResult :: forall a eff. (Generic a) => {response :: Json | eff} -> Either String a
decodeResult res = decodeJson res.response

getAddresses :: forall eff. Aff (ajax :: AJAX | eff) (Either String (Array CAddress))
getAddresses = makeRequest "/addresses"

getBalances :: forall eff. Aff (ajax :: AJAX | eff) (Either String (Array (Tuple CAddress Coin)))
getBalances = makeRequest "/balances"

newAddress :: forall eff. Aff (ajax::AJAX | eff) (Either String CAddress)
newAddress = do
  res <- attempt $ affjax $ defaultRequest { url = "/new_address", method = Left POST }
  pure $ either (Left <<< show) decodeResult res
