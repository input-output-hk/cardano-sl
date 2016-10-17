{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Universum

import           Pos.Launcher (NodeParams (..), runNodeReal)

main :: IO ()
main
-- let n = 3
-- let loggers = "xx" : map (LoggerName . toS . sformat nodeF)
--                          [NodeId 0 .. NodeId (n - 1)]
-- initLogging loggers Info
 = do
    let params =
            NodeParams
            { npDbPath = Just "node-db"
            , npRebuildDb = True
            , npSystemStart = Nothing
            , npLoggerName = "node"
            }
    runNodeReal params
