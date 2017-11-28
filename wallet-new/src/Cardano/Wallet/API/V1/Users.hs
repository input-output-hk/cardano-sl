module Cardano.Wallet.API.V1.Users where

import           Cardano.Wallet.API.V1.Types

import           Servant

type API =
         "user-profile" :> Summary "Returns the user profile for this wallet."
                        :> Get '[JSON] UserProfile
    :<|> "user-profile" :> Summary "Update user-related information."
                        :> ReqBody '[JSON] (Update UserProfile)
                        :> Put '[JSON] UserProfile
