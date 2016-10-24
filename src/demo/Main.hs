module Main where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.TimeWarp.Logging (Severity (Debug, Info), initLogging, logInfo,
                                           usingLoggerName)
import           Control.TimeWarp.Timed   (fork_, repeatForever, runTimedIO, sec)
import           Data.List                ((!!))
import           Data.String              (fromString)
import           Formatting               (build, sformat, (%))
import           Pos.DHT                  (DHTNode (..), DHTNodeType (..), Peer (..),
                                           currentNodeKey, getKnownPeers)
import           Pos.DHT.Real             (runKademliaDHT)
import           Pos.Genesis              (genesisSecretKeys, genesisVssKeyPairs)
import           Pos.Launcher             (NodeParams (..), getCurTimestamp, runNodeReal)
import           Pos.Slotting             (Timestamp)
import           Universum


runSingleNode :: Timestamp -> [DHTNode] -> Word16 -> IO ()
runSingleNode start peers i = runNodeReal params
  where
    params =
        NodeParams
        { npDbPath = Just ("node-db-" ++ show i)
        , npRebuildDb = True
        , npSystemStart = Just start
        , npLoggerName = "node" <> fromString (show i)
        , npLoggingSeverity = Debug --if i == 0 then Info else Error
        , npSecretKey = genesisSecretKeys !! (fromInteger . toInteger $ i)
        , npVssKeyPair = genesisVssKeyPairs !! (fromInteger . toInteger $ i)
        , npPort = 1000 + i
        , npDHTPort = 2001 + i
        , npDHTPeers = peers
        }

main :: IO ()
main = do initLogging ["supporter"] Info
          runTimed . runKademliaDHT supporterKadConfig $ currentNodeKey >>= main''
  where
    supporterKadConfig = KademliaDHTConfig
                  { kdcType = DHTFull
                  , kdcPort = npDHTPort
                  , kdcListeners = []
                  , kdcMessageCacheSize = 1000000
                  }
    runTimed = runTimedIO . usingLoggerName "supporter"
    n = 3
    main'' supporterKey = do
      fork_ $ repeatForever (sec 30) (const . return $ sec 30) $ do
        getKnownPeers >>= logInfo . sformat ("Known peers: " % build)
      liftIO (main' supporterKey)
    main' supporterKey = do
      systemStart <- getCurTimestamp
      let peers = [ DHTNode (Peer "127.0.0.1" 2000) supporterKey ]
      () <$ mapConcurrently (runSingleNode systemStart peers) [0 .. n - 1]
