module Main (main) where

import Pos.Util.Log.LoggerConfig (LoggerConfig)
import Pos.Infra.Network.Yaml (Topology)
import Data.List.Split
import System.Environment
import Data.Yaml
import Control.Monad.Catch
import Data.Monoid

main :: IO ()
main = do
  runTest "logConfigs" checkLogConfig
  runTest "topologyConfigs" checkTopology
  outpath <- getEnv "out"
  writeFile outpath "done"

runTest :: FromJSON a => String -> (String -> IO a) -> IO ()
runTest var func = do
  paths <- getEnv var
  let
    pathList = splitOn " " paths
    doTest path = do
      putStrLn $ "testing: " <> path
      func path
  mapM_ doTest pathList

checkLogConfig :: String -> IO LoggerConfig
checkLogConfig path = do
  either throwM return =<< decodeFileEither path

checkTopology :: String -> IO Topology
checkTopology path = either throwM return =<< decodeFileEither path

