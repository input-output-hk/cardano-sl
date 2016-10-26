module Main where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.TimeWarp.Logging (Severity (..), initLogging, logInfo,
                                           usingLoggerName)
import           Control.TimeWarp.Rpc     (runBinaryDialog, runTransfer)
import           Control.TimeWarp.Timed   (fork_, repeatForever, runTimedIO, sec)
import           Data.Default             (def)
import           Data.List                ((!!))
import           Data.String              (fromString)
import           Formatting               (build, sformat, (%))
import           Pos.DHT                  (DHTNode (..), DHTNodeType (..), currentNodeKey,
                                           getKnownPeers)
import           Pos.DHT.Real             (KademliaDHTConfig (..), runKademliaDHT)
import           Pos.Genesis              (genesisSecretKeys, genesisVssKeyPairs)
import           Pos.Launcher             (LoggingParams (..), NodeParams (..),
                                           getCurTimestamp, runNodeReal)
import           Pos.Slotting             (Timestamp)
import           Universum


runSingleNode :: Timestamp -> [DHTNode] -> Word16 -> IO ()
runSingleNode start peers i = runNodeReal params
  where
    loggingParams =
        def
        { lpRootLogger = "node" <> fromString (show i)
        , lpMainSeverity =
              if i == 0
                  then Debug
                  else Info
        , lpDhtSeverity = Just Warning
        }
    params =
        NodeParams
        { npDbPath = Just ("node-db-" ++ show i)
        , npRebuildDb = True
        , npSystemStart = Just start
        , npLogging = loggingParams
        , npSecretKey = genesisSecretKeys !! (fromInteger . toInteger $ i)
        , npVssKeyPair = genesisVssKeyPairs !! (fromInteger . toInteger $ i)
        , npPort = 3000 + i
        , npDHTPeers = peers
        }

main :: IO ()
main = do initLogging ["supporter"] Info
          runTimed . runKademliaDHT supporterKadConfig $ currentNodeKey >>= main''
  where
    supporterKadConfig = KademliaDHTConfig
                  { kdcType = DHTFull
                  , kdcPort = 2000
                  , kdcListeners = []
                  , kdcMessageCacheSize = 1000000
                  , kdcEnableBroadcast = False
                  }
    runTimed = runTimedIO . usingLoggerName "supported" . runTransfer . runBinaryDialog
    n = 3
    main'' supporterKey = do
      fork_ $ repeatForever (sec 30) (const . return $ sec 30) $ do
        getKnownPeers >>= logInfo . sformat ("Known peers: " % build)
      liftIO (main' supporterKey)
    main' supporterKey = do
      systemStart <- getCurTimestamp
      let peers = [ DHTNode ("127.0.0.1", 2000) supporterKey ]
      () <$ mapConcurrently (runSingleNode systemStart peers) [0 .. n - 1]
