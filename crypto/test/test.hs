import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Pos.Crypto.Bi
import           Test.Pos.Crypto.TempHelpers (runTests)

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Crypto.Bi.tests
        ]
