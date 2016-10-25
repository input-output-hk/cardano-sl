{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.TimeWarp.Logging (Severity (Debug))
import           Data.List                ((!!))
import qualified Options.Applicative      as Opts
import           Universum

import           Pos.Genesis              (genesisSecretKeys, genesisVssKeyPairs)
import           Pos.Launcher             (NodeParams (..), submitTxReal)

main :: IO ()
main = submitTxReal params undefined undefined undefined
  where
    params =
        NodeParams
        { npDbPath = Just "node-db"
        , npRebuildDb = True
        , npSystemStart = Nothing
        , npLoggerName = "node"
        , npLoggingSeverity = Debug
        , npSecretKey = genesisSecretKeys !! 0
        , npVssKeyPair = genesisVssKeyPairs !! 0
        , npPort = 1000
        , npDHTPort = 2000
        , npDHTPeers = []
        }
