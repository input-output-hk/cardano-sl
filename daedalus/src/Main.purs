module Main where

import Prelude
import Control.Monad.Aff (Canceler, launchAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, error)
import Control.Monad.Eff.Exception (EXCEPTION)
import Daedalus.Api (getAddresses)
import Data.Argonaut.Generic.Aeson (encodeJson)
import Data.Argonaut.Printer (printJson)
import Data.Either (Either(..))
import Network.HTTP.Affjax (AJAX)

-- TODO jk: clean it up!!
-- it's just a first (unsafe) entry point
main :: forall e. Eff (err :: EXCEPTION, ajax :: AJAX, console :: CONSOLE| e)
  (Canceler (ajax :: AJAX, console :: CONSOLE | e))
main = launchAff do
  result <- getAddresses
  case result of
    Right addr -> liftEff <<< log <<< (<>) "RESULT of '[CAddress]': " <<<
      printJson $ encodeJson addr
    Left err -> do
      liftEff <<< error $ "ERROR " <> err
