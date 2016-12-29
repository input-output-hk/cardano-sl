module Daedalus.BackendApi where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff.Exception (error, Error)
import Data.Argonaut (Json)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Bifunctor (bimap)
import Data.Either (either, Either(Left))
import Data.Generic (class Generic)
import Data.HTTP.Method (Method(POST))
import Data.Maybe (Maybe (Just))
import Network.HTTP.Affjax (affjax, defaultRequest, AJAX, get)
import Network.HTTP.RequestHeader (RequestHeader (ContentType))
import Daedalus.Types (CAddress, Coin, _address, _coin, CWallet, CTx, CWalletMeta, CTxId, CTxMeta, _ctxIdValue)
import Daedalus.Constants (backendPrefix)
import Data.MediaType.Common (applicationJSON)

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

getHistory :: forall eff. CAddress -> Aff (ajax :: AJAX | eff) (Array CTx)
getHistory addr = makeRequest $ "/api/history/" <> _address addr

send :: forall eff. CAddress -> CAddress -> Coin -> Aff (ajax :: AJAX | eff) CTx
send addrFrom addrTo amount = do
  res <- affjax $ defaultRequest
    -- TODO: use url constructor
    { url = backendPrefix <> "/api/send/" <> _address addrFrom <> "/" <> _address addrTo <> "/" <> show (_coin amount)
    , method = Left POST
    }
  either throwError pure $ decodeResult res

newWallet :: forall eff. CWalletMeta -> Aff (ajax :: AJAX | eff) CWallet
newWallet wMeta = do
  res <- affjax $ defaultRequest
    { url = backendPrefix <> "/api/new_wallet"
    , method = Left POST
    , content = Just <<< show $ encodeJson wMeta
    , headers = [ContentType applicationJSON]
    }
  either throwError pure $ decodeResult res

updateTransaction :: forall eff. CAddress -> CTxId -> CTxMeta -> Aff (ajax :: AJAX | eff) Unit
updateTransaction addr ctxId ctxMeta = do
  res <- affjax $ defaultRequest
    { url = backendPrefix <> "/api/update_transaction/" <> _address addr <> "/" <> _ctxIdValue ctxId
    , method = Left POST
    , content = Just $ encodeJson ctxMeta
    , headers = [ContentType applicationJSON]
    }
  either throwError pure $ decodeResult res

updateWallet :: forall eff. CAddress -> CWalletMeta -> Aff (ajax :: AJAX | eff) CWallet
updateWallet addr wMeta = do
  res <- affjax $ defaultRequest
    { url = backendPrefix <> "/api/update_wallet/" <> _address addr
    , method = Left POST
    , content = Just <<< show $ encodeJson wMeta
    , headers = [ContentType applicationJSON]
    }
  either throwError pure $ decodeResult res

deleteWallet :: forall eff. CAddress -> Aff (ajax :: AJAX | eff) Unit
deleteWallet addr = do
  -- FIXME: use DELETE method
  res <- affjax $ defaultRequest { url = backendPrefix <> "/api/delete_wallet/" <> _address addr, method = Left POST }
  either throwError pure $ decodeResult res
