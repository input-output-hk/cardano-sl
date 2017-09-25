module Main
  ( main
  ) where

import           Universum

import           Data.Default                   (def)
import           System.IO                      (hSetEncoding, stdout, utf8)

import           Pos.Launcher                   (applyConfigInfo)

import qualified Bench.Pos.Explorer.ServerBench as SB

-- stack bench cardano-sl-explorer
main :: IO ()
main = do
    let configInfo = def
    applyConfigInfo configInfo
    putText $ pretty configInfo
    hSetEncoding stdout utf8

    -- SB.runTimeBenchmark
    SB.runSpaceBenchmark
