{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

module NodeLowLevel (NodeId, Node, startNode, stopNode, sendMsg) where

import Data.Binary     as Bin
import Data.Binary.Put as Bin
import Data.Binary.Get as Bin
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable
import Control.Exception
import Control.Concurrent
import qualified Network.Transport as NT


-- A node id wraps a network-transport endpoint address
newtype NodeId = NodeId NT.EndPointAddress
  deriving (Eq, Ord)

data Node = Node {
       nodeEndPoint         :: NT.EndPoint,
       nodeDispatcherThread :: ThreadId
     }

data NodeException =
       ProtocolError String
     | InternalError String
  deriving (Show, Typeable)

instance Exception NodeException


startNode :: NT.Transport
          -> (NodeId -> Chan (Maybe BS.ByteString) -> IO ())
          -> IO Node
startNode transport handler = do
    Right endpoint <- NT.newEndPoint transport --TODO: error handling
    tid  <- forkIO (nodeDispatcher endpoint handler)
    --TODO: exceptions in the forkIO
    return Node {
      nodeEndPoint         = endpoint,
      nodeDispatcherThread = tid
    }


stopNode :: Node -> IO ()
stopNode Node {..} =
    NT.closeEndPoint nodeEndPoint
    -- This eventually will shut down the dispatcher thread, which in turn
    -- ought to stop the connection handling threads.
    -- It'll also close all TCP connections.


type DispatcherState = Map NT.ConnectionId ConnectionState
data ConnectionState =

       -- We got a new connection and are waiting on the first chunk of data
       ConnectionNew !NodeId

       -- We got the first chunk of data, we're now waiting either for more
       -- data or for the connection to be closed. This supports the small
       -- message optimisation.
     | ConnectionNewChunks !NodeId ![BS.ByteString]

       -- We've forked a thread to handle the message. The connection is still
       -- open and data is still arriving. We have a channel to pass the
       -- incoming chunks off to the other thread.
     | ConnectionReceiving !ThreadId !(Chan (Maybe BS.ByteString))

       -- We've forked a thread to handle the message. The connection is now
       -- closed and we have all the data already, but the thread we forked
       -- to handle it is still active.
     | ConnectionClosed !ThreadId

--TODO: extend this to keep track of the number of active threads and total
-- amount of in flight incoming data. This will be needed to inform the
-- back-pressure policy.

--TODO: need to fill in how threads are cleaned up. The 'finally' handler for
-- the forked thread will post a cleanup event to the dispatcher thread which
-- will eventually get it and remove the ConnectionId from the DispatcherState
-- and update any stats (ie decrement counts of resources in use)

-- | The one thread that handles /all/ incoming messages and dispatches them
-- to various handlers.
--
nodeDispatcher :: NT.EndPoint
               -> (NodeId -> Chan (Maybe BS.ByteString) -> IO ())
               -> IO ()
nodeDispatcher endpoint handler = undefined
    runInUnboundThread $ loop Map.empty
  where
    loop :: DispatcherState -> IO ()
--    loop state
--      | overloaded = do ... TODO

    loop !state = do
      event <- NT.receive endpoint
      case event of

        NT.ConnectionOpened connid NT.ReliableOrdered peer ->
          -- Just keep track of the new connection, nothing else
          loop (Map.insert connid (ConnectionNew (NodeId peer)) state)

        -- receiving data for an existing connection (ie a multi-chunk message)
        NT.Received connid chunks ->
          case Map.lookup connid state of
            Nothing ->
              throwIO (InternalError "received data on unknown connection")

            -- TODO: need policy here on queue size
            Just (ConnectionNew peer) ->
             loop (Map.insert connid (ConnectionNewChunks peer chunks) state)

            -- fork a new thread with a new queue filled with the first data chunks
            Just (ConnectionNewChunks peer chunks0) -> do
              chan <- newChan
              mapM_ (writeChan chan . Just) (chunks0 ++ chunks)
              tid  <- forkIO (handler peer chan)
              loop (Map.insert connid (ConnectionReceiving tid chan) state)

            Just (ConnectionReceiving tid chan) -> do
              mapM_ (writeChan chan . Just) chunks
              loop state

            Just (ConnectionClosed tid) ->
              throwIO (InternalError "received data on closed connection")

        NT.ConnectionClosed connid ->
          case Map.lookup connid state of
            Nothing ->
              throwIO (InternalError "closed unknown connection")

            -- empty message
            Just (ConnectionNew peer) -> do
              chan <- newChan
              writeChan chan Nothing
              tid  <- forkIO (handler peer chan)
              loop (Map.insert connid (ConnectionClosed tid) state)

            -- small message
            Just (ConnectionNewChunks peer chunks0) -> do
              chan <- newChan
              mapM_ (writeChan chan . Just) chunks0
              writeChan chan Nothing
              tid  <- forkIO (handler peer chan)
              loop (Map.insert connid (ConnectionClosed tid) state)

            Just (ConnectionReceiving tid chan) -> do
              writeChan chan Nothing
              loop (Map.insert connid (ConnectionClosed tid) state)

            Just (ConnectionClosed tid) ->
              throwIO (InternalError "closed a closed connection")


        NT.EndPointClosed ->
          --TODO: decide what to do with all active handlers
          return ()

        NT.ErrorEvent (NT.TransportError (NT.EventConnectionLost peer) _msg) -> undefined

        NT.ErrorEvent (NT.TransportError NT.EventEndPointFailed  msg) -> undefined

        NT.ErrorEvent (NT.TransportError NT.EventTransportFailed msg) -> undefined

        NT.ConnectionOpened _ _ _ ->
          throwIO (ProtocolError "unexpected connection reliability")

        NT.ReceivedMulticast {} ->
          throwIO (ProtocolError "unexpected multicast")

sendMsg :: Node -> NodeId -> LBS.ByteString -> IO ()
sendMsg Node{nodeEndPoint} (NodeId endpointaddr) msg = do
    mconn <- NT.connect
               nodeEndPoint
               endpointaddr
               NT.ReliableOrdered
               NT.ConnectHints{ connectTimeout = Nothing } --TODO use timeout

    -- TODO: Any error detected here needs to be reported because it's not
    -- reported via the dispatcher thread. It means we cannot establish a
    -- connection in the first place, e.g. timeout.
    case mconn of
      Left  _err -> return ()
      Right conn -> sendChunks conn (LBS.toChunks msg)

  where
    sendChunks :: NT.Connection -> [BS.ByteString] -> IO ()
    sendChunks conn [] = NT.close conn
    sendChunks conn (chunk:chunks) = do
      res <- NT.send conn [chunk]
      -- Any error detected here will be reported to the dispatcher thread
      -- so we don't need to do anything
       --TODO: though we could log here
      case res of
        Left _err -> return ()
        Right _   -> sendChunks conn chunks

