{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.TimeWarp.Logging (Severity (Info))
import           Universum

import           Pos.Launcher             (NodeParams (..), runNodeReal)

main :: IO ()
main = runNodeReal params
  where
    params =
        NodeParams
        { npDbPath = Just "node-db"
        , npRebuildDb = True
        , npSystemStart = Nothing
        , npLoggerName = "node"
        , npLoggingSeverity = Info
        }
