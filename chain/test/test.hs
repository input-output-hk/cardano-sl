import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Pos.Chain.Block.Bi
import qualified Test.Pos.Chain.Ssc.Json
import qualified Test.Pos.Chain.Txp.Json
import           Test.Pos.Util.Tripping (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Chain.Block.Bi.tests
        , Test.Pos.Chain.Ssc.Json.tests
        , Test.Pos.Chain.Txp.Json.tests
        ]
