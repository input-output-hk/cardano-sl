{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Cardano.Wallet.API.V1.Addresses where

import Cardano.Wallet.API.V1.Types

import Servant
import Data.Text

type API = "addresses" :> QueryParam "page"     Int
                       :> QueryParam "per_page" Int
                       :> QueryParam "extended" Bool
                       :> Header "Daedalus-Response-Format" Text
                       :> Get '[JSON] (OneOf [Address] (ExtendedResponse [Address]))
      :<|> "addresses" :> ReqBody '[JSON] Address :> Post '[JSON] Address
