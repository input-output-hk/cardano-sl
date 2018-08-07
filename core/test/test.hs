import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import           Test.Pos.Binary.Helpers (runTests)
import qualified Test.Pos.Core.Bi
import qualified Test.Pos.Core.EnumEmpTest
import qualified Test.Pos.Core.Json

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Core.Bi.tests
        , Test.Pos.Core.EnumEmpTest.tests
        , Test.Pos.Core.Json.tests
        ]
