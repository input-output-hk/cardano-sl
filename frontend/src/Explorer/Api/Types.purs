module Explorer.Api.Types where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Generic (gShow)
import Network.HTTP.Affjax (AffjaxResponse)
import Pos.Explorer.Web.Error (ExplorerError)

type Endpoint = String

data EndpointError
    = HTTPStatusError (AffjaxResponse Json)
    | JSONDecodingError String
    | ServerError ExplorerError

instance showEndpointError :: Show EndpointError where
    show (HTTPStatusError res) =
        "HTTPStatusError: " <> show res.status <> " msg: " <> show res.response
    show (JSONDecodingError e) =
        "JSONDecodingError: " <> gShow e
    show (ServerError e) =
        "ServerError: " <> gShow e
