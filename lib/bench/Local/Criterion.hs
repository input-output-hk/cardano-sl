module Main
  ( main
  ) where

import           Universum

import           System.IO (hSetEncoding, stdout, utf8)

import qualified Bench.Pos.Criterion.FollowTheSatoshiBench as FTS
import qualified Bench.Pos.Criterion.TxSigningBench as TS

main :: IO ()
main = do
    hSetEncoding stdout utf8
    FTS.runBenchmark
    TS.runBenchmark
