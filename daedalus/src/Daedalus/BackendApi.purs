module Daedalus.BackendApi where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error, Error)
import Control.Monad.Error.Class (throwError)
import Daedalus.Constants (backendPrefix)
import Daedalus.Types (CAddress, Coin, _address, _coin, CWallet, CTx, CWalletMeta, CTxId, CTxMeta, _ctxIdValue, CCurrency)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Array.Partial (last)
import Data.Bifunctor (bimap)
import Data.Either (either, Either(Left))
import Data.Generic (class Generic, gShow)
import Data.HTTP.Method (Method(POST))
import Data.Maybe (Maybe(Just))
import Data.MediaType.Common (applicationJSON)
import Data.String (split, joinWith)
import Network.HTTP.Affjax (AffjaxResponse, affjax, defaultRequest, AJAX, URL, AffjaxRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.RequestHeader (RequestHeader(ContentType))
import Network.HTTP.StatusCode (StatusCode(..))
import Partial.Unsafe (unsafePartial)

-- HELPERS

type URLPath = Array String

mkUrl :: URLPath -> URL
mkUrl = joinWith "/"

backendApi :: URLPath -> URL
backendApi path = mkUrl $ [backendPrefix, "api"] <> path

data ApiError
    = HTTPStatusError (AffjaxResponse Json)
    | JSONDecodingError String

instance showApiError :: Show ApiError where
    show (HTTPStatusError res) =
        "HTTPStatusError: " <> show res.status
    show (JSONDecodingError e) =
        "JSONDecodingError: " <> show e

-- REQUESTS HELPERS

decodeResult :: forall a eff. Generic a => {response :: Json | eff} -> Either Error a
decodeResult res = bimap (error <<< show <<< JSONDecodingError) id $ decodeJson res.response

makeRequest :: forall eff a r. (Generic a, Requestable r) => AffjaxRequest r -> URLPath -> Aff (ajax :: AJAX | eff) a
makeRequest request urlPath = do
    res <- affjax $ request { url = backendApi urlPath }
    when (res.status /= StatusCode 200) $
      throwError <<< error <<< show $ HTTPStatusError res
    either throwError pure $ decodeResult res

getR :: forall eff a. Generic a => URLPath -> Aff (ajax :: AJAX | eff) a
getR = makeRequest defaultRequest

postR :: forall eff a. Generic a => URLPath -> Aff (ajax :: AJAX | eff) a
postR = makeRequest $ defaultRequest { method = Left POST }

postRBody :: forall eff a b. (Generic a, Generic b) => URLPath -> b -> Aff (ajax :: AJAX | eff) a
postRBody urlPath content = flip makeRequest urlPath $
    defaultRequest { method = Left POST
                   , content = Just <<< show $ encodeJson content
                   , headers = [ContentType applicationJSON]
                   }

-- REQUESTS
getWallets :: forall eff. Aff (ajax :: AJAX | eff) (Array CWallet)
getWallets = getR ["get_wallets"]

getWallet :: forall eff. CAddress -> Aff (ajax :: AJAX | eff) CWallet
getWallet addr = getR ["get_wallet", _address addr]

getHistory :: forall eff. CAddress -> Aff (ajax :: AJAX | eff) (Array CTx)
getHistory addr = getR ["history", _address addr]

send :: forall eff. CAddress -> CAddress -> Coin -> Aff (ajax :: AJAX | eff) CTx
send addrFrom addrTo amount = postR ["send", _address addrFrom, _address addrTo, show $ _coin amount]

newWallet :: forall eff. CWalletMeta -> Aff (ajax :: AJAX | eff) CWallet
newWallet = postRBody ["new_wallet"]

updateTransaction :: forall eff. CAddress -> CTxId -> CTxMeta -> Aff (ajax :: AJAX | eff) Unit
updateTransaction addr ctxId = postRBody ["update_transaction", _address addr, _ctxIdValue ctxId]

updateWallet :: forall eff. CAddress -> CWalletMeta -> Aff (ajax :: AJAX | eff) CWallet
updateWallet addr = postRBody ["update_wallet", _address addr]

-- FIXME: use DELETE method
deleteWallet :: forall eff. CAddress -> Aff (ajax :: AJAX | eff) Unit
deleteWallet addr = postR ["delete_wallet", _address addr]

isValidAddress :: forall eff. CCurrency -> String -> Aff (ajax :: AJAX | eff) Boolean
isValidAddress cCurrency addr = getR ["valid_address", dropModuleName $ gShow cCurrency, addr]
  where
    -- TODO: this is again stupid. We should derive Show for this type instead of doing this
    dropModuleName = unsafePartial last <<< split "."
