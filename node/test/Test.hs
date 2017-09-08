import           Universum

import           Data.Default (def)
import           Spec         (spec)
import           Test.Hspec   (hspec)

import           Pos.Launcher (applyConfigInfo)

main :: IO ()
main = do
    let configInfo = def
    applyConfigInfo configInfo
    putText (pretty configInfo)
    hspec spec
