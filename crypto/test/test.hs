import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Pos.Crypto.Bi
import qualified Test.Pos.Crypto.Json
import           Test.Pos.Util.Tripping (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Crypto.Bi.tests
        , Test.Pos.Crypto.Json.tests
        ]
