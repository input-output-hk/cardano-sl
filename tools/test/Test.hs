import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Dbgen.Json
import           Test.Pos.Binary.Helpers (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Dbgen.Json.tests
        ]
