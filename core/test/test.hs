import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import           Test.Pos.Binary.Helpers (runTests)
import qualified Test.Pos.Core.Bi

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Core.Bi.tests
        ]
