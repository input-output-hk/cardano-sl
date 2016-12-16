{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Node.Internal (
    NodeId(..),
    Node(..),
    startNode,
    stopNode,
    sendMsg,
    ChannelIn(..),
    ChannelOut(..),
    connectOutChannel,
    connectInOutChannel,
    closeChannel,
    withOutChannel,
    withInOutChannel,
    writeChannel,
    readChannel,
  ) where

import Data.Binary     as Bin
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Typeable
import Control.Exception hiding (bracket, throw)
import qualified Network.Transport.Abstract as NT
import qualified Network.Transport as NT (EventErrorCode(EventConnectionLost, EventEndPointFailed, EventTransportFailed))
import System.Random (StdGen, random)
import Mockable.Class
import Mockable.Concurrent
import Mockable.Exception
import qualified Mockable.Channel as Channel
import Mockable.SharedAtomic


-- A node id wraps a network-transport endpoint address
newtype NodeId = NodeId NT.EndPointAddress
  deriving (Eq, Ord, Show)

data Node (m :: * -> *) = Node {
       nodeEndPoint         :: NT.EndPoint m,
       nodeDispatcherThread :: ThreadId m,
       nodeExpectedIncoming :: SharedAtomicT m (StdGen, Map Nonce (ThreadId m, ChannelIn m))
     }

type Nonce = Word64

data NodeException =
       ProtocolError String
     | InternalError String
  deriving (Show, Typeable)

instance Exception NodeException

newtype ChannelIn m = ChannelIn (Channel.ChannelT m (Maybe BS.ByteString))

newtype ChannelOut m = ChannelOut (NT.Connection m)

startNode :: ( Mockable SharedAtomic m, Mockable Fork m
             , Mockable Channel.Channel m, Mockable Throw m
             , Mockable RunInUnboundThread m )
          => NT.Transport m
          -> StdGen
          -> (NodeId -> ChannelIn m -> m ())
          -> (NodeId -> ChannelIn m -> ChannelOut m -> m ())
          -> m (Node m)
startNode transport prng handlerIn handlerOut = do
    Right endpoint <- NT.newEndPoint transport --TODO: error handling
    incomingVar    <- newSharedAtomic (prng, Map.empty)
    tid  <- fork (nodeDispatcher endpoint incomingVar handlerIn handlerOut)
    --TODO: exceptions in the forkIO
    return Node {
      nodeEndPoint         = endpoint,
      nodeDispatcherThread = tid,
      nodeExpectedIncoming = incomingVar
    }


stopNode :: Node m -> m ()
stopNode Node {..} =
    NT.closeEndPoint nodeEndPoint
    -- This eventually will shut down the dispatcher thread, which in turn
    -- ought to stop the connection handling threads.
    -- It'll also close all TCP connections.


type DispatcherState m = Map NT.ConnectionId (ConnectionState m)
data ConnectionState m =

       -- We got a new connection and are waiting on the first chunk of data
       ConnectionNew !NodeId

       -- We got the first chunk of data, we're now waiting either for more
       -- data or for the connection to be closed. This supports the small
       -- message optimisation.
     | ConnectionNewChunks !NodeId ![BS.ByteString]

       -- We've forked a thread to handle the message. The connection is still
       -- open and data is still arriving. We have a channel to pass the
       -- incoming chunks off to the other thread.
     | ConnectionReceiving !(ThreadId m) !(Channel.ChannelT m (Maybe BS.ByteString))

       -- We've forked a thread to handle the message. The connection is now
       -- closed and we have all the data already, but the thread we forked
       -- to handle it is still active.
     | ConnectionClosed !(ThreadId m)

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
nodeDispatcher :: forall m .
                  ( Mockable SharedAtomic m, Mockable Fork m
                  , Mockable Channel.Channel m, Mockable RunInUnboundThread m
                  , Mockable Throw m )
               => NT.EndPoint m
               -> SharedAtomicT m (StdGen, Map Nonce (ThreadId m, ChannelIn m))
               -> (NodeId -> ChannelIn m -> m ())
               -> (NodeId -> ChannelIn m -> ChannelOut m -> m ())
               -> m ()
nodeDispatcher endpoint incomingVar handlerIn handlerInOut =
    runInUnboundThread $ loop Map.empty
  where
    loop :: DispatcherState m -> m ()
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
              throw (InternalError "received data on unknown connection")

            -- TODO: need policy here on queue size
            Just (ConnectionNew peer) ->
             loop (Map.insert connid (ConnectionNewChunks peer chunks) state)

            -- fork a new thread with a new queue filled with the first data chunks
            Just (ConnectionNewChunks peer@(NodeId peerEndpointAddr) chunks0) ->
              case LBS.uncons (LBS.fromChunks (chunks0 ++ chunks)) of
                Nothing -> loop state -- all empty, wait for more data
                Just (w, ws)
                  | w == controlHeaderCodeUnidirectional -> do
                    chan <- Channel.newChannel
                    mapM_ (Channel.writeChannel chan . Just) (LBS.toChunks ws)
                    tid  <- fork (handlerIn peer (ChannelIn chan))
                    loop (Map.insert connid (ConnectionReceiving tid chan) state)

                  | w == controlHeaderCodeBidirectionalAck ||
                    w == controlHeaderCodeBidirectionalSyn
                  , LBS.length ws < 8 -> -- need more data
                    loop (Map.insert connid (ConnectionNewChunks peer (chunks0 ++ chunks)) state)

                  | w == controlHeaderCodeBidirectionalSyn
                  , Right (ws',_,nonce) <- decodeOrFail ws -> do
                    chan <- Channel.newChannel
                    mapM_ (Channel.writeChannel chan . Just) (LBS.toChunks ws')
                    tid <- fork $ do
                      mconn <- NT.connect
                                 endpoint
                                 peerEndpointAddr
                                 NT.ReliableOrdered
                                 NT.ConnectHints{ connectTimeout = Nothing } --TODO use timeout
                      case mconn of
                        Left  err  -> throw err
                        Right conn -> do
                          NT.send conn [controlHeaderBidirectionalSyn nonce]
                          handlerInOut peer (ChannelIn chan) (ChannelOut conn)
                    loop (Map.insert connid (ConnectionReceiving tid chan) state)


                  | w == controlHeaderCodeBidirectionalAck
                  , Right (ws',_,nonce) <- decodeOrFail ws -> do
                    (tid, ChannelIn chan) <- modifySharedAtomic incomingVar $ \(prng, expected) ->
                      case Map.lookup nonce expected of
                        Nothing -> throw (ProtocolError $ "unexpected ack nonce " ++ show nonce)
                        Just info -> do
                          let !expected' = Map.delete nonce expected
                          return ((prng, expected'), info)
                    mapM_ (Channel.writeChannel chan . Just) (LBS.toChunks ws')
                    loop (Map.insert connid (ConnectionReceiving tid chan) state)

                  | otherwise ->
                      throw (ProtocolError $ "unexpected control header " ++ show w)

            Just (ConnectionReceiving tid chan) -> do
              mapM_ (Channel.writeChannel chan . Just) chunks
              loop state

            Just (ConnectionClosed tid) ->
              throw (InternalError "received data on closed connection")

        NT.ConnectionClosed connid ->
          case Map.lookup connid state of
            Nothing ->
              throw (InternalError "closed unknown connection")

            -- empty message
            Just (ConnectionNew peer) -> do
              chan <- Channel.newChannel
              Channel.writeChannel chan Nothing
              tid  <- fork (handlerIn peer (ChannelIn chan))
              loop (Map.insert connid (ConnectionClosed tid) state)

            -- small message
            Just (ConnectionNewChunks peer chunks0) -> do
              chan <- Channel.newChannel
              mapM_ (Channel.writeChannel chan . Just) chunks0
              Channel.writeChannel chan Nothing
              tid  <- fork (handlerIn peer (ChannelIn chan))
              loop (Map.insert connid (ConnectionClosed tid) state)

            Just (ConnectionReceiving tid chan) -> do
              Channel.writeChannel chan Nothing
              loop (Map.insert connid (ConnectionClosed tid) state)

            Just (ConnectionClosed tid) ->
              throw (InternalError "closed a closed connection")


        NT.EndPointClosed ->
          --TODO: decide what to do with all active handlers
          return ()

        NT.ErrorEvent (NT.TransportError (NT.EventErrorCode (NT.EventConnectionLost peer)) _msg) -> undefined

        NT.ErrorEvent (NT.TransportError (NT.EventErrorCode NT.EventEndPointFailed)  msg) -> undefined

        NT.ErrorEvent (NT.TransportError (NT.EventErrorCode NT.EventTransportFailed) msg) -> undefined

        NT.ErrorEvent (NT.TransportError NT.UnsupportedEvent msg) -> undefined

        NT.ConnectionOpened _ _ _ ->
          throw (ProtocolError "unexpected connection reliability")

sendMsg :: forall m . ( Monad m ) => Node m -> NodeId -> LBS.ByteString -> m ()
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
    sendChunks :: NT.Connection m -> [BS.ByteString] -> m ()
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

connectInOutChannel
    :: ( Mockable Channel.Channel m, Mockable Fork m, Mockable SharedAtomic m
       , Mockable Throw m )
    => Node m
    -> NodeId
    -> m (ChannelIn m, ChannelOut m)
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
      Left  err  -> throw err
      Right outconn -> do
        (nonce, inchan) <- allocateInChannel
        NT.send outconn [controlHeaderBidirectionalSyn nonce]
        return (ChannelIn inchan, ChannelOut outconn)
  where
    allocateInChannel = do
      tid   <- myThreadId
      chan  <- Channel.newChannel
      nonce <- modifySharedAtomic nodeExpectedIncoming $ \(prng, expected) -> do
                 let (nonce, !prng') = random prng
                     !expected' = Map.insert nonce (tid, ChannelIn chan) expected
                 pure ((prng', expected), nonce)
      return (nonce, chan)

connectOutChannel
    :: ( Monad m, Mockable Throw m )
    => Node m
    -> NodeId
    -> m (ChannelOut m)
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
      Left  err  -> throw err
      Right conn -> do
        NT.send conn [controlHeaderUnidirectional]
        return (ChannelOut conn)

closeChannel :: ChannelOut m -> m ()
closeChannel (ChannelOut conn) = NT.close conn

withInOutChannel
    :: ( Mockable Bracket m, Mockable Fork m, Mockable Channel.Channel m
       , Mockable SharedAtomic m, Mockable Throw m )
    => Node m
    -> NodeId
    -> (ChannelIn m -> ChannelOut m -> m a)
    -> m a
withInOutChannel node nodeid action =
    bracket (connectInOutChannel node nodeid)
            (\(_, outchan) -> closeChannel outchan)
            (\(inchan, outchan) -> action inchan outchan)

withOutChannel
    :: ( Mockable Bracket m, Mockable Throw m )
    => Node m
    -> NodeId
    -> (ChannelOut m -> m a)
    -> m a
withOutChannel node nodeid =
    bracket (connectOutChannel node nodeid) closeChannel

writeChannel :: ( Monad m ) => ChannelOut m -> [BS.ByteString] -> m ()
writeChannel (ChannelOut conn) [] = NT.close conn
writeChannel (ChannelOut conn) (chunk:chunks) = do
    res <- NT.send conn [chunk]
    -- Any error detected here will be reported to the dispatcher thread
    -- so we don't need to do anything
     --TODO: though we could log here
    case res of
      Left _err -> return ()
      Right _   -> writeChannel (ChannelOut conn) chunks

readChannel :: ( Mockable Channel.Channel m ) => ChannelIn m -> m (Maybe BS.ByteString)
readChannel (ChannelIn chan) = Channel.readChannel chan
