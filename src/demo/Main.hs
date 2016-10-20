module Main where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.TimeWarp.Logging (Severity (Error, Info))
import           Data.List                ((!!))
import           Data.String              (fromString)
import           Universum

import           Pos.Genesis              (genesisSecretKeys, genesisVssKeyPairs)
import           Pos.Launcher             (NodeParams (..), getCurTimestamp, runNodeReal)
import           Pos.Slotting             (Timestamp)

runSingleNode :: Timestamp -> Word -> IO ()
runSingleNode start i = runNodeReal params
  where
    params =
        NodeParams
        { npDbPath = Just ("node-db-" ++ show i)
        , npRebuildDb = True
        , npSystemStart = Just start
        , npLoggerName = "node" <> fromString (show i)
        , npLoggingSeverity = if i == 0 then Info else Error
        , npSecretKey = genesisSecretKeys !! 0
        , npVssKeyPair = genesisVssKeyPairs !! 0
        }

main :: IO ()
main = do
    let n = 3
    -- let loggers = "xx" : map (LoggerName . toS . sformat nodeF)
    --                         [NodeId 0 .. NodeId (n - 1)]
    -- initLogging loggers Info
    systemStart <- getCurTimestamp
    () <$ mapConcurrently (runSingleNode systemStart) [0 .. n - 1]
