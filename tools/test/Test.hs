import           Test.Hspec (hspec)

import           Spec (spec)

import           Test.Pos.Util.Tripping (runTests)

main :: IO ()
main = do
    hspec spec
    runTests []
