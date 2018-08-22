-- | This module contains the top level API definition for frontend-related
-- tasks.  The API endpoints presented here are intended for use with the
-- Daedalus client, and aren't useful for wallets, exchanges, and other users.
module Cardano.Wallet.API.Internal where

import           Pos.Core.Update (SoftwareVersion)

import           Servant

import           Cardano.Wallet.API.Response (ValidJSON)
import           Cardano.Wallet.API.Types (Tags)
import           Cardano.Wallet.API.V1.Types (V1)

type API = Tags '["Internal"] :>
    (    "next-update"
        :> Summary "Version of the next update (404 if none)"
        :> Get '[ValidJSON] (V1 SoftwareVersion)

    :<|> "apply-update"
        :> Summary "Apply the next available update"
        :> Post '[ValidJSON] NoContent

    :<|> "postpone-update"
        :> Summary "Discard and postpone the next available update"
        :> Post '[ValidJSON] NoContent

    :<|> "reset-wallet-state"
        :> Summary "Clear wallet state and all associated secret keys"
        :> DeleteNoContent '[ValidJSON] NoContent
    )
