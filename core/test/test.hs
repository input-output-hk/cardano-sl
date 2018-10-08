import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Pos.Core.Bi
import qualified Test.Pos.Core.Json
import qualified Test.Pos.Core.SafeCopy
import           Test.Pos.Util.Tripping (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Core.Bi.tests
        , Test.Pos.Core.Json.tests
        , Test.Pos.Core.SafeCopy.tests
        ]
