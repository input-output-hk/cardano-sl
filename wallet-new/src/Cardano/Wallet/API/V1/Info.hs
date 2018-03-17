module Cardano.Wallet.API.V1.Info where

import           Cardano.Wallet.API.Response (ValidJSON, WalletResponse)
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Types

import           Servant

type API = Tags '["Info"] :>
         ( "node-info" :> Summary "Retrieves the dynamic information for this node."
                       :> Get '[ValidJSON] (WalletResponse NodeInfo)
         )
