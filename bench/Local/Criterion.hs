module Main where

import           System.IO                                 (hSetEncoding,
                                                            stdout, utf8)
import           Universum

import qualified Bench.Pos.Criterion.FollowTheSatoshiBench as FTS

main :: IO ()
main = do
  hSetEncoding stdout utf8
  FTS.runBenchmark
