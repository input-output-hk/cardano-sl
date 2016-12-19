module Daedalus.BackendApi where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error, Error)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Bifunctor (bimap)
import Data.Either (either, Either(Left))
import Data.Generic (class Generic)
import Data.HTTP.Method (Method(POST))
import Data.Tuple (Tuple)
import Network.HTTP.Affjax (affjax, defaultRequest, AJAX, get)
import Pos.Types.Types (Coin)
import Pos.Wallet.Web.ClientTypes (CAddress)

-- TODO: remove traces, they are adding to increase verbosity in development
makeRequest :: forall eff a. (Generic a) => String -> Aff (ajax :: AJAX | eff) a
makeRequest url = do
  res <- get url
  either throwError pure $ decodeResult res

decodeResult :: forall a eff. (Generic a) => {response :: Json | eff} -> Either Error a
decodeResult res = bimap error id $ decodeJson res.response

getAddresses :: forall eff. Aff (ajax :: AJAX | eff) (Array CAddress)
getAddresses = makeRequest "/api/addresses"

getBalances :: forall eff. Aff (ajax :: AJAX | eff) (Array (Tuple CAddress Coin))
getBalances = makeRequest "/api/balances"

newAddress :: forall eff. Aff (ajax :: AJAX | eff) CAddress
newAddress = do
  res <- affjax $ defaultRequest { url = "/api/new_address", method = Left POST }
  either throwError pure $ decodeResult res
