import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Pos.Binary.BiSerialize
import qualified Test.Pos.Binary.BiSizeBounds
import           Test.Pos.Util.Tripping (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Binary.BiSerialize.tests
        , Test.Pos.Binary.BiSizeBounds.tests
        ]
