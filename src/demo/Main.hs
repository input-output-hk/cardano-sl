module Main where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.TimeWarp.Logging (Severity (Error, Info))
import           Data.List                ((!!))
import           Data.String              (fromString)
import           Universum

import           Pos.DHT                  (Peer (..))
import           Pos.Genesis              (genesisSecretKeys,
                                           genesisVssKeyPairs)
import           Pos.Launcher             (NodeParams (..), getCurTimestamp,
                                           runNodeReal)
import           Pos.Slotting             (Timestamp)


runSingleNode :: Timestamp -> [Peer] -> Word16 -> IO ()
runSingleNode start peers i = runNodeReal params
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
        , npPort = 1000 + i
        , npDHTPort = 2000 + i
        , npDHTPeers = peers
        }

main :: IO ()
main = do
    let n = 3
    systemStart <- getCurTimestamp
    let peers = (\i -> Peer "127.0.0.1" $ 2000 + i) <$> [0 .. n - 1]
    () <$ mapConcurrently (runSingleNode systemStart peers) [0 .. n - 1]
