import           Universum

import           Data.Default (def)
import           Spec         (spec)
import           Test.Hspec   (hspec)

import           Pos.Launcher (applyConfigInfo)

main :: IO ()
main = do
    applyConfigInfo def
    hspec spec
