-- | Start and stop EKG monitoring server using metrics from a 'Node' from
-- cardano-sl-networking.

module Pos.Util.Monitor
    ( startMonitor
    , stopMonitor
    ) where

import           Universum

import           Control.Concurrent (killThread)
import           Node (Node)
import           Node.Util.Monitor (registerMetrics)
import           Pos.System.Metrics.Constants (cardanoNamespace)
import qualified System.Metrics as Monitoring
import qualified System.Remote.Monitoring.Wai as Monitoring

startMonitor
    :: Int
    -> Node
    -> IO Monitoring.Server
startMonitor port node = do
    store <- Monitoring.newStore
    registerMetrics (Just cardanoNamespace) node store
    Monitoring.registerGcMetrics store
    server <- Monitoring.forkServerWith store "127.0.0.1" port
    liftIO . putStrLn $ "Forked EKG server on port " ++ show port
    return server

stopMonitor
    :: Monitoring.Server
    -> IO ()
stopMonitor server = killThread (Monitoring.serverThreadId server)
