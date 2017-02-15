module Explorer.Api where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), either)
import Data.Generic (class Generic, gShow)
import Data.HTTP.Method (Method(..))
import Explorer.Types.State (CBlockEntries, CTxEntries)
import Explorer.Util.Api (decodeJson)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, affjax, defaultRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.StatusCode (StatusCode(..))
import Pos.Explorer.Web.Error (ExplorerError)

type Endpoint = String

backendPrefix :: String
-- backendPrefix = "http://localhost:8100/api/"
backendPrefix = "/api/"

-- errors

data ApiError
    = HTTPStatusError (AffjaxResponse Json)
    | JSONDecodingError String
    | ServerError ExplorerError

instance showApiError :: Show ApiError where
    show (HTTPStatusError res) =
        "HTTPStatusError: " <> show res.status <> " msg: " <> show res.response
    show (JSONDecodingError e) =
        "JSONDecodingError: " <> gShow e
    show (ServerError e) =
        "ServerError: " <> gShow e

-- result helper

decodeResult :: forall a eff. Generic a => {response :: Json | eff} -> Either Error a
decodeResult = either (Left <<< mkJSONError) (bimap mkServerError id) <<< decodeJson <<< _.response
  where
    mkJSONError = error <<< show <<< JSONDecodingError
    mkServerError = error <<< show <<< ServerError

request :: forall a r eff. (Generic a, Requestable r) => AffjaxRequest r ->
    Endpoint -> Aff (ajax :: AJAX | eff) a
request req endpoint = do
    result <- affjax $ req { url = backendPrefix <> endpoint }
    when (isHttpError result.status) $
        throwError <<< error <<< show $ HTTPStatusError result
    either throwError pure $ decodeResult result
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
