module Daedalus.Api where

import Prelude
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Promise (Promise, fromAff)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Either (Either(Left), either)
import Network.HTTP.Affjax (AJAX, get)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.ResponseHeader (ResponseHeader)
import Network.HTTP.StatusCode (StatusCode)
import Pos.Wallet.Web.ClientTypes (CAddress)

getAddresses :: forall eff. Aff (ajax::AJAX | eff) (Either String (Array CAddress))
getAddresses = do
  res <- attempt $ get "/addresses"
  let decode r = decodeJson r.response
  pure $ either (Left <<< show) decode res

getAddressesPromise :: forall a eff. (Respondable a) => Eff (ajax::AJAX | eff)
  (Promise
    { status :: StatusCode
    , headers :: Array ResponseHeader
    , response :: a}
  )
-- TODO jk:
-- makeAff after decoding to create Promise from it
getAddressesPromise = fromAff $ get "/addresses"
