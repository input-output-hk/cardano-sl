module Main where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

main :: Eff (console :: CONSOLE) Unit
main = log "Mr. Cardano would like to talk to you, but Mr. Cardano does not exist."
