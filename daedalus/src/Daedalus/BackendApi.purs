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
import Data.HTTP.Method (Method(POST, DELETE))
import Data.Tuple (Tuple)
import Network.HTTP.Affjax (affjax, defaultRequest, AJAX, get)
import Daedalus.Types (CAddress, Coin, _address, _coin, CWallet)
import Daedalus.Constants (backendPrefix)

-- TODO: remove traces, they are adding to increase verbosity in development
makeRequest :: forall eff a. (Generic a) => String -> Aff (ajax :: AJAX | eff) a
makeRequest url = do
  res <- get $ backendPrefix <> url
  either throwError pure $ decodeResult res

decodeResult :: forall a eff. (Generic a) => {response :: Json | eff} -> Either Error a
decodeResult res = bimap error id $ decodeJson res.response

getWallets :: forall eff. Aff (ajax :: AJAX | eff) (Array CWallet)
getWallets = makeRequest $ "/api/get_wallets"

getWallet :: forall eff. CAddress -> Aff (ajax :: AJAX | eff) CWallet
getWallet addr = makeRequest $ "/api/get_wallet/" <> _address addr

-- getHistory :: forall eff. CAddress -> Aff (ajax :: AJAX | eff) (Array (Tuple CAddress Coin))
-- getHistory = makeRequest "/api/history" <<< _address

send :: forall eff. CAddress -> CAddress -> Coin -> Aff (ajax :: AJAX | eff) Unit
send addrFrom addrTo amount = do
  res <- affjax $ defaultRequest
    -- TODO: use url constructor
    { url = backendPrefix <> "/api/send/" <> _address addrFrom <> "/" <> _address addrTo <> "/" <> show (_coin amount)
    , method = Left POST
    }
  either throwError pure $ decodeResult res

newAddress :: forall eff. Aff (ajax :: AJAX | eff) CAddress
newAddress = do
  res <- affjax $ defaultRequest { url = backendPrefix <> "/api/new_address", method = Left POST }
  either throwError pure $ decodeResult res

deleteAddress :: forall eff. CAddress -> Aff (ajax :: AJAX | eff) Unit
deleteAddress addr = do
  res <- affjax $ defaultRequest { url = backendPrefix <> "/api/delete_address/" <> _address addr, method = Left DELETE }
  either throwError pure $ decodeResult res
