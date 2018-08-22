import           Universum

import           Test.Hspec (hspec, parallel)

import           Spec (spec)

import qualified Test.Pos.Core.Bi
import qualified Test.Pos.Core.EnumEmpTest
import qualified Test.Pos.Core.Json
import           Test.Pos.Util.Parallel.Parallelize (parallelizeAllCores)
import           Test.Pos.Util.Tripping (runTests)

main :: IO ()
main = do
    parallelizeAllCores
    hspec $ parallel spec
    runTests
        [ Test.Pos.Core.Bi.tests
        , Test.Pos.Core.EnumEmpTest.tests
        , Test.Pos.Core.Json.tests
        ]
