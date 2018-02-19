module Cardano.Wallet.API.Dev where


import           Servant ((:>))

import           Cardano.Wallet.API.Types
import qualified Cardano.Wallet.API.Dev.Test as Test

type API = Tags '["Test"] :> Test.API
