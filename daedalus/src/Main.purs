module Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Daedalus.Api (getAddresses)
import Data.Argonaut.Printer (printJson)
import Data.Either (Either(Right))
import Data.Argonaut.Generic.Aeson (encodeJson)

-- TODO jk: clean it up!!
-- it's just a first (unsafe) entry point
main = launchAff do
  Right addr <- getAddresses
  liftEff $ log <<< printJson $ encodeJson addr
