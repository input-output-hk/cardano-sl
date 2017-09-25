import           Universum

import           Test.Hspec             (hspec)

import           Spec                   (spec)
import           Test.Pos.Configuration (testConf)

main :: IO ()
main = do
    putText (show testConf)
    hspec spec
