module Daedalus.Api where

import Prelude
import Control.Monad.Aff (attempt, Aff)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Either (either, Either(Left))
import Network.HTTP.Affjax (AJAX, get)
import Pos.Wallet.Web.ClientTypes (CAddress)

getAddresses :: forall eff. Aff (ajax::AJAX | eff) (Either String (Array CAddress))
getAddresses = do
  res <- attempt $ get "/addresses"
  let decode r = decodeJson r.response
  pure $ either (Left <<< show) decode res
