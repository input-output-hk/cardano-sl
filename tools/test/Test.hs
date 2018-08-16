import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Pos.Tools.Dbgen.Json
import           Test.Pos.Util.Tripping (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Tools.Dbgen.Json.tests
        ]
