import           Universum

import qualified Test.Pos.DB.Rocks.Functions
import           Test.Pos.Binary.Helpers (runTests)

main :: IO ()
main = runTests [Test.Pos.DB.Rocks.Functions.tests]
