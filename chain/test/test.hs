import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import           Test.Pos.Binary.Helpers (runTests)
import qualified Test.Pos.Chain.Block.Bi
import qualified Test.Pos.Chain.Ssc.Json

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Chain.Block.Bi.tests
        , Test.Pos.Chain.Ssc.Json.tests
        ]
