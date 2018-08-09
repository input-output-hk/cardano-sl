import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Pos.Binary.BiSerialize
import           Test.Pos.Binary.Helpers (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Binary.BiSerialize.tests
        ]
