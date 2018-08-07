import           Prelude (IO)

import           Test.Hspec (hspec)

import           Spec (spec)
import           Test.Pos.Binary.Helpers (runTests)
import qualified Test.Pos.Infra.Bi

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Infra.Bi.tests
        ]
