module Main.Test where

import Prelude
import BigNumber (BIGNUMBER)
import BigNumber.Test (testBigNumber)
import Control.Monad.Eff (Eff)
import Data.Time.Test (testNominalDiffTime)
import Explorer.Api.Socket.Test (testApiSocket)
import Explorer.Routes.Test (testRoutes)
import Explorer.State.Test (testState)
import Explorer.Update.Test (testUpdate)
import Explorer.Util.Config.Test (testConfigUtil)
import Explorer.Util.Data.Test (testDataUtil)
import Explorer.Util.String.Test (testStringUtil)
import Explorer.Util.Time.Test (testPrettyDuration)
import Explorer.View.CSS.Test (testCSS)
import Explorer.View.Common.Test (testCommonViews)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects (bigNumber :: BIGNUMBER)) Unit
main = run [consoleReporter] do
    testApiSocket
    testBigNumber
    testCommonViews
    testConfigUtil
    testCSS
    testNominalDiffTime
    testPrettyDuration
    testDataUtil
    testStringUtil
    testRoutes
    testState
    testUpdate
