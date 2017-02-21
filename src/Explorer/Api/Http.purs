module Explorer.Api.Http where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..), either)
import Data.Generic (class Generic)
import Data.HTTP.Method (Method(..))
import Explorer.Api.Helper (decodeResult)
import Explorer.Api.Types (EndpointError(..), Endpoint)
import Explorer.Types.State (CBlockEntries, CTxEntries)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.StatusCode (StatusCode(..))

endpointPrefix :: String
-- endpointPrefix = "http://localhost:8100/api/"
endpointPrefix = "/api/"

-- result helper

decodeResponse :: forall a eff. Generic a => {response :: Json | eff} -> Either Error a
decodeResponse = decodeResult <<< _.response

request :: forall a r eff. (Generic a, Requestable r) => AffjaxRequest r ->
    Endpoint -> Aff (ajax :: AJAX | eff) a
request req endpoint = do
    result <- affjax $ req { url = endpointPrefix <> endpoint }
    when (isHttpError result.status) $
        throwError <<< error <<< show $ HTTPStatusError result
    either throwError pure $ decodeResponse result
    where
      isHttpError (StatusCode c) = c >= 400

get :: forall eff a. Generic a => Endpoint -> Aff (ajax :: AJAX | eff) a
get = request defaultRequest

post :: forall eff a. Generic a => Endpoint -> Aff (ajax :: AJAX | eff) a
post = request $ defaultRequest { method = Left POST }

-- api

fetchLatestBlocks :: forall eff. Aff (ajax::AJAX | eff) CBlockEntries
fetchLatestBlocks = get "last_blocks"

fetchLatestTransactions :: forall eff. Aff (ajax::AJAX | eff) CTxEntries
fetchLatestTransactions = get "last_txs"
