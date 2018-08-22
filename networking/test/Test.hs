import           Spec (spec)
import           Test.Hspec (hspec, parallel)

import           Test.Pos.Util.Parallel.Parallelize (parallelizeAllCores)

main :: IO ()
main = do
    parallelizeAllCores
    hspec $ parallel spec
