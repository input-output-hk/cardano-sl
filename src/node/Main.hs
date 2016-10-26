{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.TimeWarp.Logging (Severity (Debug))
import           Data.Default             (def)
import           Data.List                ((!!))
import           Universum

import           Pos.Genesis              (genesisSecretKeys, genesisVssKeyPairs)
import           Pos.Launcher             (LoggingParams (..), NodeParams (..),
                                           runNodeReal)

main :: IO ()
main = runNodeReal params
  where
    loggingParams =
        def
        { lpRootLogger = "node"
        , lpMainSeverity = Debug
        }
    params =
        NodeParams
        { npDbPath = Just "node-db"
        , npRebuildDb = True
        , npSystemStart = Nothing
        , npLogging = loggingParams
        , npSecretKey = genesisSecretKeys !! 0
        , npVssKeyPair = genesisVssKeyPairs !! 0
        , npPort = 3000
        , npDHTPeers = []
        }
