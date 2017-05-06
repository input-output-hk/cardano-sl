module Main.Test where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Time.Test (testNominalDiffTime)
import Explorer.Routes.Test (testRoutes)
import Explorer.Update.Test (testUpdate)
import Explorer.Util.Data.Test (testDataUtil)
import Explorer.Util.String.Test (testStringUtil)
import Explorer.Util.Time.Test (testPrettyDuration)
import Explorer.View.CSS.Test (testCSS)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
    testCSS
    testNominalDiffTime
    testPrettyDuration
    testDataUtil
    testStringUtil
    testRoutes
    testUpdate
