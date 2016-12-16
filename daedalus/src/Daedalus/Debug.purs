module Daedalus.Debug where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, error)
import Daedalus.BackendApi (getAddresses, getBalances)
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

-- | log result or error of '/address'
logAddresses :: forall e. Aff (ajax :: AJAX, console :: CONSOLE| e) Unit
logAddresses = do
  result <- getAddresses
  case result of
    Right addr -> liftEff $ logSuccess "[RESULT of '/address'] " addr
    Left err -> liftEff $ logFail "[Error of '/address']" err

-- | log result or error of '/address'
logBalances :: forall e. Aff (ajax :: AJAX, console :: CONSOLE| e) Unit
logBalances = do
  result <- getBalances
  case result of
    Right addr -> liftEff $ logSuccess "[RESULT of '/balances'] " addr
    Left err -> liftEff $ logFail "[Error of '/balances']" err
