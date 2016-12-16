module Daedalus.BackendApi where

import Prelude
import Control.Monad.Aff (attempt, Aff)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Either (either, Either(Left))
import Data.Generic (class Generic)
import Data.Tuple (Tuple)
import Network.HTTP.Affjax (AJAX, get)
import Pos.Types.Types (Coin)
import Pos.Wallet.Web.ClientTypes (CAddress)
import Debug.Trace (traceAnyM)

-- TODO: remove traces, they are adding to increase verbosity in development
makeRequest :: forall eff a. (Generic a) => String -> Aff (ajax :: AJAX | eff) (Either String a)
makeRequest url = do
    traceAnyM $ "makeRequest: Starting request " <> url
    res <- attempt $ get url
    traceAnyM "makeRequest: Got reponse"
    traceAnyM res
    traceAnyM "makeRequest: Result of decoding"
    traceAnyM $ either (Left <<< show) decode res
    where
      decode res' = decodeJson res'.response

getAddresses :: forall eff. Aff (ajax :: AJAX | eff) (Either String (Array CAddress))
getAddresses = makeRequest "/addresses"

getBalances :: forall eff. Aff (ajax :: AJAX | eff) (Either String (Array (Tuple CAddress Coin)))
getBalances = makeRequest "/balances"
