import           Test.Pos.Binary.Helpers (runTests)
import qualified Test.Pos.Ssc.Json

main :: IO ()
main = do
    runTests
        [ Test.Pos.Ssc.Json.tests
        ]
