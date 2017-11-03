module Main
  ( main
  ) where

import           Universum

import           System.IO                              (hSetEncoding, stdout, utf8)

-- import qualified Bench.Pos.Criterion.FollowTheSatoshi as FTS
-- import qualified Bench.Pos.Criterion.TxSigning        as TS
import qualified Bench.Pos.Criterion.BlockSerialization as BSR

main :: IO ()
main = do
    hSetEncoding stdout utf8
    -- FTS.runBenchmark
    -- TS.runBenchmark
    BSR.runBenchmark
