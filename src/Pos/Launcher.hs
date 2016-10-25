{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Launcher of full node or simple operations.

module Pos.Launcher
       ( NodeParams (..)
       , getCurTimestamp
       , runNode
       , runNodeReal
       ) where

import           Control.TimeWarp.Timed   (currentTime, runTimedIO)
import           Universum

import           Control.TimeWarp.Logging (logInfo)
import           Formatting               (build, sformat, (%))
import           Pos.Communication        (serve)
import           Pos.DHT                  (DHTNodeType (..), discoverPeers)
import           Pos.Slotting             (Timestamp (Timestamp))
import           Pos.Worker               (runWorkers)
import           Pos.WorkMode             (NodeParams (..), WorkMode, runRealMode)

-- | Get current time as Timestamp. It is intended to be used when you
-- launch the first node. It doesn't make sense in emulation mode.
getCurTimestamp :: IO Timestamp
getCurTimestamp = Timestamp <$> runTimedIO currentTime

-- | Run full node in any WorkMode.
runNode :: WorkMode m => NodeParams -> m ()
runNode NodeParams {..} = do
    peers <- discoverPeers DHTFull
    logInfo $ sformat ("Known peers: " % build) peers

    runWorkers
    serve (fromIntegral npPort)

-- | Run full node in real mode.
runNodeReal :: NodeParams -> IO ()
runNodeReal p = runRealMode p $ runNode p
