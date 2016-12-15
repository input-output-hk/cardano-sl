{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

module NodeLowLevel (
    NodeId,
    Node,
    startNode,
    stopNode,
    sendMsg,
    ChannelIn,
    ChannelOut,
    connectOutChannel,
    connectInOutChannel,
    closeChannel,
    withOutChannel,
    withInOutChannel,
    writeChannel,
    readChannel,
  ) where

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
import Data.Word
import Data.Monoid
import Data.Typeable
import Control.Exception
import Control.Concurrent
import System.Random
import qualified Network.Transport as NT


-- A node id wraps a network-transport endpoint address
newtype NodeId = NodeId NT.EndPointAddress
  deriving (Eq, Ord)

data Node = Node {
       nodeEndPoint         :: NT.EndPoint,
       nodeDispatcherThread :: ThreadId,
       nodeExpectedIncoming :: MVar (StdGen, Map Nonce (ThreadId, ChannelIn))
     }

type Nonce = Word64

data NodeException =
       ProtocolError String
     | InternalError String
  deriving (Show, Typeable)

instance Exception NodeException

newtype ChannelIn = ChannelIn (Chan (Maybe BS.ByteString))

newtype ChannelOut = ChannelOut NT.Connection

startNode :: NT.Transport
          -> (NodeId -> ChannelIn -> IO ())
          -> (NodeId -> ChannelIn -> ChannelOut -> IO ())
          -> IO Node
startNode transport handlerIn handlerOut = do
    Right endpoint <- NT.newEndPoint transport --TODO: error handling
    prng           <- newStdGen
    incomingVar    <- newMVar (prng, Map.empty)
    tid  <- forkIO (nodeDispatcher endpoint incomingVar handlerIn handlerOut)
    --TODO: exceptions in the forkIO
    return Node {
      nodeEndPoint         = endpoint,
      nodeDispatcherThread = tid,
      nodeExpectedIncoming = incomingVar
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
               -> MVar (StdGen, Map Nonce (ThreadId, ChannelIn))
               -> (NodeId -> ChannelIn -> IO ())
               -> (NodeId -> ChannelIn -> ChannelOut -> IO ())
               -> IO ()
nodeDispatcher endpoint incomingVar handlerIn handlerInOut =
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
            Just (ConnectionNewChunks peer@(NodeId peerEndpointAddr) chunks0) ->
              case LBS.uncons (LBS.fromChunks (chunks0 ++ chunks)) of
                Nothing -> loop state -- all empty, wait for more data
                Just (w, ws)
                  | w == controlHeaderCodeUnidirectional -> do
                    chan <- newChan
                    mapM_ (writeChan chan . Just) (LBS.toChunks ws)
                    tid  <- forkIO (handlerIn peer (ChannelIn chan))
                    loop (Map.insert connid (ConnectionReceiving tid chan) state)

                  | w == controlHeaderCodeBidirectionalAck ||
                    w == controlHeaderCodeBidirectionalSyn
                  , LBS.length ws < 8 -> -- need more data
                    loop (Map.insert connid (ConnectionNewChunks peer (chunks0 ++ chunks)) state)

                  | w == controlHeaderCodeBidirectionalSyn
                  , Right (ws',_,nonce) <- decodeOrFail ws -> do
                    chan <- newChan
                    mapM_ (writeChan chan . Just) (LBS.toChunks ws')
                    tid <- forkIO $ do
                      mconn <- NT.connect
                                 endpoint
                                 peerEndpointAddr
                                 NT.ReliableOrdered
                                 NT.ConnectHints{ connectTimeout = Nothing } --TODO use timeout
                      case mconn of
                        Left  err  -> throwIO err
                        Right conn -> do
                          NT.send conn [controlHeaderBidirectionalSyn nonce]
                          handlerInOut peer (ChannelIn chan) (ChannelOut conn)
                    loop (Map.insert connid (ConnectionReceiving tid chan) state)


                  | w == controlHeaderCodeBidirectionalAck
                  , Right (ws',_,nonce) <- decodeOrFail ws -> do
                    (tid, ChannelIn chan) <- modifyMVar incomingVar $ \(prng, expected) -> do
                      case Map.lookup nonce expected of
                        Nothing -> throwIO (ProtocolError $ "unexpected ack nonce " ++ show nonce)
                        Just info -> do
                          let !expected' = Map.delete nonce expected
                          return ((prng, expected'), info)
                    mapM_ (writeChan chan . Just) (LBS.toChunks ws')
                    loop (Map.insert connid (ConnectionReceiving tid chan) state)

                  | otherwise ->
                    throwIO (ProtocolError $ "unexpected control header " ++ show w)

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
              tid  <- forkIO (handlerIn peer (ChannelIn chan))
              loop (Map.insert connid (ConnectionClosed tid) state)

            -- small message
            Just (ConnectionNewChunks peer chunks0) -> do
              chan <- newChan
              mapM_ (writeChan chan . Just) chunks0
              writeChan chan Nothing
              tid  <- forkIO (handlerIn peer (ChannelIn chan))
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


controlHeaderCodeBidirectionalSyn :: Word8
controlHeaderCodeBidirectionalSyn = fromIntegral (fromEnum 'S')

controlHeaderCodeBidirectionalAck :: Word8
controlHeaderCodeBidirectionalAck = fromIntegral (fromEnum 'A')

controlHeaderCodeUnidirectional :: Word8
controlHeaderCodeUnidirectional = fromIntegral (fromEnum 'U')

controlHeaderUnidirectional :: BS.ByteString
controlHeaderUnidirectional =
    BS.singleton controlHeaderCodeUnidirectional

controlHeaderBidirectionalSyn :: Nonce -> BS.ByteString
controlHeaderBidirectionalSyn nonce =
    fixedSizeBuilder 9 $
        BS.word8 controlHeaderCodeBidirectionalSyn
     <> BS.word64BE nonce

controlHeaderBidirectionalAck :: Nonce -> BS.ByteString
controlHeaderBidirectionalAck nonce =
    fixedSizeBuilder 9 $
        BS.word8 controlHeaderCodeBidirectionalAck
     <> BS.word64BE nonce

fixedSizeBuilder :: Int -> BS.Builder -> BS.ByteString
fixedSizeBuilder n =
    LBS.toStrict . BS.toLazyByteStringWith (BS.untrimmedStrategy n n) LBS.empty

connectInOutChannel :: Node -> NodeId -> IO (ChannelIn, ChannelOut)
connectInOutChannel node@Node{nodeEndPoint, nodeExpectedIncoming}
                    nodeid@(NodeId endpointaddr) = do
    mconn <- NT.connect
               nodeEndPoint
               endpointaddr
               NT.ReliableOrdered
               NT.ConnectHints{ connectTimeout = Nothing } --TODO use timeout

    -- TODO: Any error detected here needs to be reported because it's not
    -- reported via the dispatcher thread. It means we cannot establish a
    -- connection in the first place, e.g. timeout.
    case mconn of
      Left  err  -> throwIO err
      Right outconn -> do
        (nonce, inchan) <- allocateInChannel
        NT.send outconn [controlHeaderBidirectionalSyn nonce]
        return (ChannelIn inchan, ChannelOut outconn)
  where
    allocateInChannel = do
      tid   <- myThreadId
      chan  <- newChan
      nonce <- modifyMVar nodeExpectedIncoming $ \(prng, expected) -> do
                 let (nonce, !prng') = random prng
                     !expected' = Map.insert nonce (tid, ChannelIn chan) expected
                 return ((prng', expected), nonce)
      return (nonce, chan)

connectOutChannel :: Node -> NodeId -> IO ChannelOut
connectOutChannel Node{nodeEndPoint} (NodeId endpointaddr) = do
    mconn <- NT.connect
               nodeEndPoint
               endpointaddr
               NT.ReliableOrdered
               NT.ConnectHints{ connectTimeout = Nothing } --TODO use timeout

    -- TODO: Any error detected here needs to be reported because it's not
    -- reported via the dispatcher thread. It means we cannot establish a
    -- connection in the first place, e.g. timeout.
    case mconn of
      Left  err  -> throwIO err
      Right conn -> do
        NT.send conn [controlHeaderUnidirectional]
        return (ChannelOut conn)

closeChannel :: ChannelOut -> IO ()
closeChannel (ChannelOut conn) = NT.close conn

withInOutChannel :: Node -> NodeId -> (ChannelIn -> ChannelOut -> IO a) -> IO a
withInOutChannel node nodeid action =
    bracket (connectInOutChannel node nodeid)
            (\(_, outchan) -> closeChannel outchan)
            (\(inchan, outchan) -> action inchan outchan)

withOutChannel :: Node -> NodeId -> (ChannelOut -> IO a) -> IO a
withOutChannel node nodeid =
    bracket (connectOutChannel node nodeid) closeChannel

writeChannel :: ChannelOut -> [BS.ByteString] -> IO ()
writeChannel (ChannelOut conn) [] = NT.close conn
writeChannel (ChannelOut conn) (chunk:chunks) = do
    res <- NT.send conn [chunk]
    -- Any error detected here will be reported to the dispatcher thread
    -- so we don't need to do anything
     --TODO: though we could log here
    case res of
      Left _err -> return ()
      Right _   -> writeChannel (ChannelOut conn) chunks

readChannel :: ChannelIn -> IO (Maybe BS.ByteString)
readChannel (ChannelIn chan) = readChan chan

