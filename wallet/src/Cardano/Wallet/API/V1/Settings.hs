module Cardano.Wallet.API.V1.Settings where

import           Cardano.Wallet.API.Response (APIResponse, ValidJSON)
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Types

import           Servant

type API = Tag "Settings" 'NoTagDescription :>
         ( "node-settings"  :> Summary "Retrieves the static settings for this node."
                            :> Get '[ValidJSON] (APIResponse NodeSettings)
         )
