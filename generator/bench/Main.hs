module Main
  ( main
  ) where

import           Universum

import           System.IO (hSetEncoding, stdout, utf8)

import qualified Bench.Pos.Criterion.Block.Logic as L

main :: IO ()
main = do
    hSetEncoding stdout utf8
    L.runBenchmark
