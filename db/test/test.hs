import           Universum

import           Test.Pos.Binary.Helpers (runTests)
import qualified Test.Pos.DB.Epoch.Index

main :: IO ()
main = runTests [Test.Pos.DB.Epoch.Index.tests]
