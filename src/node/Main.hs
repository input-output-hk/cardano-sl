{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Formatting               (sformat)
import           Universum

import           Control.TimeWarp.Logging (LoggerName (..), Severity (Info), initLogging)

import           Pos.Communication        (fullNode)
import           Pos.Launcher             (runNodesReal)
import           Pos.Types                (NodeId (..), nodeF)

main :: IO ()
-- Here's how to run a simple system with two nodes pinging each other:
-- runNodes [node_ping 1, node_ping 0]
main = do
    let n = 3
    let loggers = "xx" : map (LoggerName . toS . sformat nodeF)
                             [NodeId 0 .. NodeId (n-1)]
    initLogging loggers Info
    runNodesReal (replicate n fullNode)
