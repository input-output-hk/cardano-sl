import           Universum

import qualified Test.Pos.DB.Epoch.Index
import           Test.Pos.Util.Tripping (runTests)

main :: IO ()
main = runTests [Test.Pos.DB.Epoch.Index.tests]
