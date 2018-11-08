module Cardano.Wallet.API.V1.Internal.Update where

import           Servant

import           Cardano.Wallet.API.Response (ValidJSON)

type API =
    "update"
    :> ( "apply"
        :> Post '[ValidJSON] NoContent
    :<|> "postpone"
        :> Post '[ValidJSON] NoContent
    )
