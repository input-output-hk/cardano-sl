import           Prelude (IO)

import           Test.Hspec (hspec)

import           Spec (spec)
import qualified Test.Pos.Infra.Bi
import qualified Test.Pos.Infra.Json
import           Test.Pos.Util.Tripping (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Infra.Bi.tests
        , Test.Pos.Infra.Json.tests
        ]
