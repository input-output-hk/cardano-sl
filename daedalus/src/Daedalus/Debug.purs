module Daedalus.Debug where

import Prelude
import Control.Monad.Aff (attempt, Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, error)
import Daedalus.BackendApi (getWallets, getWallet, newAddress)
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Argonaut.Printer (printJson)
import Data.Either (Either(..))
import Data.Generic (class Generic)
import Network.HTTP.Affjax (AJAX)

-- by @akegalj: I think this module is not necessary any longer

-- All following functions are just for debugging only

-- | log error
logFail :: forall e. String -> String -> Eff (console :: CONSOLE| e) Unit
logFail msg err = error $ msg <> " " <> err

-- | encode result and log it
logSuccess :: forall e a. (Generic a) => String -> a -> Eff (console :: CONSOLE| e) Unit
logSuccess msg result = log <<< (<>) msg <<< printJson $ encodeJson result

-- | debug '/api/address'
logAddresses :: forall e. Aff (ajax :: AJAX, console :: CONSOLE| e) Unit
logAddresses = do
  result <- attempt getWallets
  case result of
    Right addr -> liftEff $ logSuccess "[RESULT of '/api/get_wallets'] " addr
    Left err -> liftEff $ logFail "[Error of '/api/get_wallets']" $ show err
