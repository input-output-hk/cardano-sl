-- | This module contains the top level API definition for frontend-related
-- tasks.  The API endpoints presented here are intended for use with the
-- Daedalus client, and aren't useful for wallets, exchanges, and other users.
module Cardano.Wallet.API.V1.Daedalus where

-- migrate everything except for import/export

type API =
    "daedalus"
        :> ( "update"
            :> ( "apply" :> Get '[ValidJSON] (WalletResponse ())
            :<|> "postpone" :> Get '[ValidJSON] (WalletResponse ())
            )
        :<|> "papervend" :>  "redemptions" :> "ada" :> Get '[ValidJSON] (WalletResponse ())
        :<|> "redemptions" :> "ada" :> Get '[ValidJSON] (WalletResponse ())
        )



