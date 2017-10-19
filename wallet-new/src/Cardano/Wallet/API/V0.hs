{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Cardano.Wallet.API.V0 where

import           Cardano.Wallet.API.Types

import           Servant

-- | "Mount" the legacy API here.
type API
   = "version" :> Summary "Returns the version for this API."
               :> Get '[JSON] WalletVersion
