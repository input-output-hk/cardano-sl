module Main
  ( main
  ) where

import           Universum

import           Data.Default                            (def)
import           System.IO                               (hSetEncoding, stdout, utf8)

import           Pos.Core                                (giveStaticConsts)
import           Pos.Launcher                            (applyConfigInfo)

import qualified Bench.Pos.Explorer.ServerCriterionBench as SCB


-- stack bench cardano-sl-explorer
main :: IO ()
main = do
    let configInfo = def
    applyConfigInfo configInfo
    putText (pretty configInfo)
    hSetEncoding stdout utf8

    giveStaticConsts SCB.runBenchmark