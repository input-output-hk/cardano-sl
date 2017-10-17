{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Cardano.Wallet.API.V1.Addresses where

import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Types

import           Data.Text
import           Servant

type API = "addresses" :> QueryParam "page"     Int
                       :> QueryParam "per_page" Int
                       :> QueryParam "extended" Bool
                       :> Header "Daedalus-Response-Format" Text
                       :> Summary "Returns all the addresses."
                       :> Get '[JSON] (OneOf [Address] (ExtendedResponse [Address]))
      :<|> "addresses" :> ReqBody '[JSON] Address
                       :> Summary "Creates a new Address."
                       :> Post '[JSON] Address
