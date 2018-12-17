import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import           Test.Pos.Configuration (defaultTestConf)
import qualified Test.Pos.Launcher.Json
import           Test.Pos.Util.Tripping (runTests)

main :: IO ()
main = do
    putText $ "default configuration: " <> show defaultTestConf
    hspec spec
    runTests [ Test.Pos.Launcher.Json.tests ]
