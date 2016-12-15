module Daedalus.Api where

import Data.Argonaut.Generic.Aeson (decodeJson)
import Prelude
import Data.Either (Either(Left), either)
import Network.HTTP.Affjax (AJAX, get)
import Control.Monad.Aff (Aff, attempt)
import Pos.Wallet.Web.ClientTypes (CAddress)

getAddresses :: forall eff. Aff (ajax::AJAX | eff) (Either String (Array CAddress))
getAddresses = do
  res <- attempt $ get "/addresses"
  let decode r = decodeJson r.response
  pure $ either (Left <<< show) decode res
