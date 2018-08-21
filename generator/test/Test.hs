import           Universum

import           Test.Hspec (hspec, parallel)

import           Spec (spec)
import           Test.Pos.Configuration (defaultTestConf)
import           Test.Pos.Util.Parallel.Parallelize (parallelizeAllCores)

main :: IO ()
main = do
    parallelizeAllCores
    putText $ "default configuration: " <> show defaultTestConf
    hspec $ parallel spec
