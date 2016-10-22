{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.TimeWarp.Logging (Severity (Info))
import           Data.List                ((!!))
import           Universum

import           Pos.DHT                  (Peer (..))
import           Pos.Genesis              (genesisSecretKeys,
                                           genesisVssKeyPairs)
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
        , npSecretKey = genesisSecretKeys !! 0
        , npVssKeyPair = genesisVssKeyPairs !! 0
        , npPort = 1000
        , npDHTPort = 2000
        , npDHTPeers = []
        }
