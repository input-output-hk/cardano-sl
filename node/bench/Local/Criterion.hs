module Main
  ( main
  ) where

import           Universum

import           Data.Default                       (def)
import           System.IO                          (hSetEncoding, stdout, utf8)

import           Pos.Launcher                       (applyConfigInfo)

-- import qualified Bench.Pos.Criterion.FollowTheSatoshiBench as FTS
import qualified Bench.Pos.Criterion.TxSigningBench as TS

main :: IO ()
main = do
    applyConfigInfo def
    hSetEncoding stdout utf8
    -- FTS.runBenchmark
    TS.runBenchmark
