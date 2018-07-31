-- | This module contains the top level API definition for frontend-related
-- tasks.  The API endpoints presented here are intended for use with the
-- Daedalus client, and aren't useful for wallets, exchanges, and other users.
module Cardano.Wallet.API.Internal where

import           Cardano.Wallet.API.Response (ValidJSON)
import           Cardano.Wallet.API.Types (Tags)
import           Servant ((:<|>), (:>), DeleteNoContent, NoContent, Post,
                     Summary)

type API = Tags '["Internal"] :>
    (    "apply-update"
        :> Summary "Apply the next available update"
        :> Post '[ValidJSON] NoContent

    :<|> "postpone-update"
        :> Summary "Discard and postpone the next available update"
        :> Post '[ValidJSON] NoContent

    :<|> "reset-wallet-state"
        :> Summary "Clear wallet state and all associated secret keys"
        :> DeleteNoContent '[ValidJSON] NoContent
    )
