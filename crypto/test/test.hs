import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Pos.Crypto.Bi
import           Test.Pos.Binary.Helpers (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Crypto.Bi.tests
        ]
