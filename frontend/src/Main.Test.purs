module Main.Test where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Explorer.Util.String.Test (testStringUtil)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
    testStringUtil
