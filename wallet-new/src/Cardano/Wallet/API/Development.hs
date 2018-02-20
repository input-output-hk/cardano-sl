module Cardano.Wallet.API.Development where

import           Servant.API.ContentTypes (OctetStream)
import           Cardano.Wallet.API.Response (WalletResponse)
import           Pos.Wallet.Web.Methods.Misc (WalletStateSnapshot)

import           Servant

type API = "dump-wallet-state" :> Summary "Dump wallet state."
                               :> Get '[OctetStream] (WalletResponse WalletStateSnapshot)
