module Daedalus.Debug where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, error)
import Daedalus.BackendApi (getAddresses, getBalances, newAddress)
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Argonaut.Printer (printJson)
import Data.Either (Either(..))
import Data.Generic (class Generic)
import Network.HTTP.Affjax (AJAX)

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
  result <- getAddresses
  case result of
    Right addr -> liftEff $ logSuccess "[RESULT of '/api/address'] " addr
    Left err -> liftEff $ logFail "[Error of '/api/address']" err

-- | debug '/api/balances'
logBalances :: forall e. Aff (ajax :: AJAX, console :: CONSOLE| e) Unit
logBalances = do
  result <- getBalances
  case result of
    Right addr -> liftEff $ logSuccess "[RESULT of '/api/balances'] " addr
    Left err -> liftEff $ logFail "[Error of '/api/balances']" err

-- | debug '/api/new_address'
logNewAddress :: forall e. Aff (ajax :: AJAX, console :: CONSOLE| e) Unit
logNewAddress = do
  result <- newAddress
  case result of
    Right addr -> liftEff $ logSuccess "[RESULT of '/api/new_address'] " addr
    Left err -> liftEff $ logFail "[Error of '/api/apinew_address']" err
