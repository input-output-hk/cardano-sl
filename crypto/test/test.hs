import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import           Test.Pos.Binary.Helpers (runTests)
import qualified Test.Pos.Crypto.Bi

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Crypto.Bi.tests
        ]
