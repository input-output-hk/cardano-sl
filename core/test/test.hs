import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Pos.Core.Bi
import           Test.Pos.Binary.Helpers (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Core.Bi.tests
        ]
