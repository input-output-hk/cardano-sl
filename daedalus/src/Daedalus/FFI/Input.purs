module Deadalus.FFI.Input where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

hello :: String -> Eff (console :: CONSOLE) Unit
hello = log <<< (<>) "Hello "

helloCallback :: forall cb. (String -> cb) -> cb
helloCallback cb = cb "hello"
