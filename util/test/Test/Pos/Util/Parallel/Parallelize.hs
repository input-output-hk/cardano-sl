module Test.Pos.Util.Parallel.Parallelize
       ( parallelizeAllCores
       ) where

import           Universum

import           GHC.Conc (getNumProcessors, setNumCapabilities)

-- | `parallelizeAllCores` gets the number of processors on a machine and
-- sets the number of threads equal to the number of processors on the machine.
parallelizeAllCores :: IO ()
parallelizeAllCores = getNumProcessors >>= setNumCapabilities
