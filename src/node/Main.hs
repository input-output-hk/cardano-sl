{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Protolude                hiding (for, wait, (%))

import           Control.TimeWarp.Logging (LoggerName (..), Severity (Info), initLogging)

import           Pos.Constants            (n)
import           Pos.Launcher             (fullNode, runNodesReal)
import           Pos.Types.Types          (NodeId (..))

main :: IO ()
-- Here's how to run a simple system with two nodes pinging each other:
-- runNodes [node_ping 1, node_ping 0]
main = do
    let loggers = "xx" : map (LoggerName . show) [NodeId 0 .. NodeId (n-1)]
    initLogging loggers Info
    runNodesReal [fullNode, fullNode, fullNode]
