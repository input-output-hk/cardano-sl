{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Node.Internal (
    NodeId(..),
    Node(..),
    ChannelIn(..),
    ChannelOut(..),
    startNode,
    stopNode,
    withOutChannel,
    withInOutChannel,
    writeChannel,
    readChannel
  ) where

import Data.Binary     as Bin
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Typeable
import Data.List (foldl')
import Control.Exception hiding (bracket, throw, catch, finally)
import qualified Network.Transport.Abstract as NT
import qualified Network.Transport as NT (EventErrorCode(EventConnectionLost, EventEndPointFailed, EventTransportFailed))
import System.Random (StdGen, random, Random)
import Mockable.Class
import Mockable.Concurrent
import Mockable.Exception
import qualified Mockable.Channel as Channel
import Mockable.SharedAtomic

-- | A 'NodeId' wraps a network-transport endpoint address
newtype NodeId = NodeId NT.EndPointAddress
  deriving (Eq, Ord, Show)

-- | The state of a Node, to be held in a shared atomic cell because other
--   threads will mutate it in order to set up bidirectional connections.
data NodeState m = NodeState {
      nodeStateGen :: !StdGen
      -- ^ To generate nonces.
    , nodeStateNonces :: !(Map Nonce (NonceState m))
      -- ^ Nonces identify bidirectional connections, and this gives the state
      --   of each one.
    , nodeStateFinished :: ![(Either NT.ConnectionId Nonce, Maybe SomeException)]
      -- ^ Connection identifiers or nonces for handlers which have finished.
      --   'Nonce's for bidirectional connections, 'ConnectionId's for handlers
      --   spawned to respond to incoming connections.
    }

-- | A 'Node' is a network-transport 'EndPoint' with bidirectional connection
--   state and a thread to dispatch network-transport events.
data Node (m :: * -> *) = Node {
       nodeEndPoint         :: NT.EndPoint m,
       nodeDispatcherThread :: ThreadId m,
       nodeState :: SharedAtomicT m (NodeState m)
     }

-- | Used to identify bidirectional connections.
newtype Nonce = Nonce {
      getNonce :: Word64
    }

deriving instance Show Nonce
deriving instance Eq Nonce
deriving instance Ord Nonce
deriving instance Random Nonce
deriving instance Binary Nonce

data NodeException =
       ProtocolError String
     | InternalError String
  deriving (Show, Typeable)

instance Exception NodeException

-- | Input from the wire. Nothing means there's no more to come.
newtype ChannelIn m = ChannelIn (Channel.ChannelT m (Maybe BS.ByteString))

-- | Output to the wire.
newtype ChannelOut m = ChannelOut (NT.Connection m)

-- | Bring up a 'Node' using a network transport.
startNode :: ( Mockable SharedAtomic m, Mockable Fork m, Mockable Bracket m
             , Mockable Channel.Channel m, Mockable Throw m, Mockable Catch m )
          => NT.EndPoint m
          -> StdGen
          -> (NodeId -> ChannelIn m -> m ())
          -- ^ Handle incoming unidirectional connections.
          -> (NodeId -> ChannelIn m -> ChannelOut m -> m ())
          -- ^ Handle incoming bidirectional connections.
          -> m (Node m)
startNode endPoint prng handlerIn handlerOut = do
    sharedState <- newSharedAtomic (NodeState prng Map.empty [])
    tid <- fork $
        nodeDispatcher endPoint sharedState handlerIn handlerOut
    --TODO: exceptions in the forkIO
    --they should be raised in this thread.
    return Node {
      nodeEndPoint         = endPoint,
      nodeDispatcherThread = tid,
      nodeState            = sharedState
    }

-- | Stop a 'Node', closing its network transport endpoint.
stopNode :: Node m -> m ()
stopNode Node {..} =
    NT.closeEndPoint nodeEndPoint
    -- This eventually will shut down the dispatcher thread, which in turn
    -- ought to stop the connection handling threads.
    -- It'll also close all TCP connections.

-- | State which is local to the dispatcher, and need not be accessible to
--   any other thread.
type DispatcherState m = Map NT.ConnectionId (ConnectionState m)

-- | The state of a connection (associated with a 'ConnectionId', see
--   'DispatcherState'.
data ConnectionState m =

       -- | We got a new connection and are waiting on the first chunk of data
      ConnectionNew !NodeId

      -- | We got the first chunk of data, we're now waiting either for more
      --   data or for the connection to be closed. This supports the small
      --   message optimisation.
    | ConnectionNewChunks !NodeId ![BS.ByteString]

      -- | We've forked a thread to handle the message. The connection is still
      --   open and data is still arriving. We have a channel to pass the
      --   incoming chunks off to the other thread.
    | ConnectionReceiving !(ThreadId m) !(Channel.ChannelT m (Maybe BS.ByteString))

      -- | We've forked a thread to handle the message. The connection is now
      --   closed and we have all the data already, but the thread we forked
      --   to handle it is still active.
    | ConnectionClosed !(ThreadId m)

      -- | The handler which we forked to process the data has finished.
      --   Subsequent incoming data has nowhere to go.
    | ConnectionHandlerFinished !(Maybe SomeException)

instance Show (ConnectionState m) where
    show term = case term of
        ConnectionNew nodeid -> "ConnectionNew " ++ show nodeid
        ConnectionNewChunks nodeid _ -> "ConnectionNewChunks " ++ show nodeid
        ConnectionReceiving _ _ -> "ConnectionReceiving"
        ConnectionClosed _ -> "ConnectionClosed"
        ConnectionHandlerFinished e -> "ConnectionHandlerFinished " ++ show e

-- | Bidirectional connections (conversations) are identified not by
--   'ConnectionId' but by 'Nonce', because their handlers run before any
--   connection is established.
--
--   Once a connection for a conversation is established, its nonce is
--   associated with it via 'NonceHandlerConnected', but prior to this it's
--   'NonceHandlerNotConnected', meaning the handler is running but a
--   SYN/ACK handshake has not been completed.
--
--   The NonceState must be accessible by the dispatcher and by other threads,
--   because other threads may initiate a conversation.
data NonceState m =

      NonceHandlerNotConnected !(ThreadId m) !(ChannelIn m)

    | NonceHandlerConnected !NT.ConnectionId

instance Show (NonceState m) where
    show term = case term of
        NonceHandlerNotConnected _ _ -> "NonceHandlerNotConnected"
        NonceHandlerConnected connid -> "NonceHandlerConnected " ++ show connid

--TODO: extend this to keep track of the number of active threads and total
-- amount of in flight incoming data. This will be needed to inform the
-- back-pressure policy.

-- | The one thread that handles /all/ incoming messages and dispatches them
-- to various handlers.
nodeDispatcher :: forall m .
                  ( Mockable SharedAtomic m, Mockable Fork m, Mockable Bracket m
                  , Mockable Channel.Channel m, Mockable Throw m
                  , Mockable Catch m )
               => NT.EndPoint m
               -> SharedAtomicT m (NodeState m)
               -- ^ Nonce states and a StdGen to generate nonces. It's in a
               --   shared atomic because other threads must be able to alter
               --   it when they start a conversation.
               --   The third element of the triple will be updated by handler
               --   threads when they finish.
               -> (NodeId -> ChannelIn m -> m ())
               -> (NodeId -> ChannelIn m -> ChannelOut m -> m ())
               -> m ()
nodeDispatcher endpoint nodeState handlerIn handlerInOut =
    loop Map.empty
  where

    finally :: m t -> m () -> m t
    finally action after = bracket (pure ()) (const after) (const action)

    -- Take the dead threads from the shared atomic and release them all from
    -- the map if their connection is also closed.
    -- Only if the connection is closed *and* the handler is finished, can we
    -- forget about a connection id. It could be that the handler is finished,
    -- but we receive more data, in which case we want to somehow make the
    -- peer stop pushing any data.
    -- If the connection is closed after the handler finishes, then the entry
    -- in the state map will be cleared by the dispatcher loop.
    --
    -- TBD use the reported exceptions to inform the dispatcher somehow?
    -- If a lot of threads are giving exceptions, should we change dispatcher
    -- behavior?
    updateStateForFinishedHandlers :: DispatcherState m -> m (DispatcherState m)
    updateStateForFinishedHandlers state = modifySharedAtomic nodeState $ \(NodeState prng nonces finished) -> do
        -- For every Left (c :: NT.ConnectionId) we can remove it from the map
        -- if its connection is closed, or indicate that the handler is finished
        -- so that it will be removed when the connection is closed.
        --
        -- For every Right (n :: Nonce) we act depending on the nonce state.
        -- If the handler isn't connected, we'll just drop the nonce state
        -- entry.
        -- If the handler is connected, we'll update the connection state for
        -- that ConnectionId to say the handler has finished.
        let (state', nonces') = foldl' folder (state, nonces) finished
        --() <- trace ("DEBUG: state map size is " ++ show (Map.size state')) (pure ())
        --() <- trace ("DEBUG: nonce map size is " ++ show (Map.size nonces')) (pure ())
        pure ((NodeState prng nonces' []), state')
        where

        -- Updates connection and nonce states in a left fold.
        folder (connIdState, nonces) (connidOrNonce, e) = case connidOrNonce of
            Left connid -> (Map.update (connUpdater e) connid connIdState, nonces)
            Right nonce -> folderNonce (connIdState, nonces) (nonce, e)

        folderNonce (connIdState, nonces) (nonce, e) = case Map.lookup nonce nonces of
            Nothing -> error "Handler for unknown nonce finished"
            Just (NonceHandlerNotConnected _ _) ->
                (connIdState, Map.delete nonce nonces)
            Just (NonceHandlerConnected connid) ->
                ( Map.update (connUpdater e) connid connIdState
                , Map.delete nonce nonces
                )

        connUpdater :: Maybe SomeException -> ConnectionState m -> Maybe (ConnectionState m)
        connUpdater e connState = case connState of
            ConnectionClosed _ -> Nothing
            _ -> Just (ConnectionHandlerFinished e)

    -- Handle the first chunks received, interpreting the control byte(s).
    -- TODO: review this. It currently does not use the 'DispatcherState' but
    -- that's dubious. Implementing good error handling will probably demand
    -- using it.
    handleFirstChunks
        :: NT.ConnectionId
        -> NodeId
        -> [BS.ByteString]
        -> DispatcherState m
        -> m (Maybe (ConnectionState m))
    handleFirstChunks connid peer@(NodeId peerEndpointAddr) chunks _ =
        case LBS.uncons (LBS.fromChunks chunks) of
            -- Empty. Wait for more data.
            Nothing -> pure Nothing
            Just (w, ws)
                -- Peer wants a unidirectional (peer -> local)
                -- connection. Make a channel for the incoming
                -- data and fork a thread to consume it.
                | w == controlHeaderCodeUnidirectional -> do
                  chan <- Channel.newChannel
                  Channel.writeChannel chan (Just (BS.concat (LBS.toChunks ws)))
                  tid  <- fork $ finishHandler nodeState (Left connid) (handlerIn peer (ChannelIn chan))
                  pure . Just $ ConnectionReceiving tid chan

                -- Bidirectional header without the nonce
                -- attached. Wait for the nonce.
                | w == controlHeaderCodeBidirectionalAck ||
                  w == controlHeaderCodeBidirectionalSyn
                , LBS.length ws < 8 -> -- need more data
                  pure . Just $ ConnectionNewChunks peer chunks

                -- Peer wants a bidirectional connection.
                -- Fork a thread to reply with an ACK and then
                -- handle according to handlerInOut.
                | w == controlHeaderCodeBidirectionalSyn
                , Right (ws',_,nonce) <- decodeOrFail ws -> do
                  chan <- Channel.newChannel
                  Channel.writeChannel chan (Just (BS.concat (LBS.toChunks ws')))
                  let action = do
                          mconn <- NT.connect
                                     endpoint
                                     peerEndpointAddr
                                     NT.ReliableOrdered
                                     NT.ConnectHints{ connectTimeout = Nothing } --TODO use timeout
                          case mconn of
                            Left  err  -> throw err
                            Right conn -> do
                              -- TODO: error handling
                              NT.send conn [controlHeaderBidirectionalAck nonce]
                              handlerInOut peer (ChannelIn chan) (ChannelOut conn)
                                  `finally`
                                  closeChannel (ChannelOut conn)
                  tid <- fork $ finishHandler nodeState (Left connid) action
                  pure . Just $ ConnectionReceiving tid chan

                -- We want a bidirectional connection and the
                -- peer has acknowledged. Check that their nonce
                -- matches what we sent and if so start writing
                -- to the channel associated with that nonce.
                -- See connectInOutChannel/withInOutChannel for the other half
                -- of the story (where we send SYNs and record
                -- nonces).
                --
                -- A call to withInOutChannel ensures that when the action using
                -- the in/out channel completes, it will update the shared
                -- atomic 'finished' variable to indicate that thread
                -- corresponding to the *nonce* is completed. It may have
                -- already completed at this point, which is weird but not
                -- out of the question (the handler didn't ask to receive
                -- anything). 
                --
                | w == controlHeaderCodeBidirectionalAck
                , Right (ws',_,nonce) <- decodeOrFail ws -> do
                  (tid, ChannelIn chan) <- modifySharedAtomic nodeState $ \(NodeState prng expected finished) ->
                      case Map.lookup nonce expected of
                          Nothing -> throw (ProtocolError $ "unexpected ack nonce " ++ show nonce)
                          Just (NonceHandlerNotConnected tid inchan) -> do
                              let !expected' = Map.insert nonce (NonceHandlerConnected connid) expected
                              return ((NodeState prng expected' finished), (tid, inchan))
                          Just _ -> throw (InternalError $ "duplicate or delayed ACK for " ++ show nonce)
                  Channel.writeChannel chan (Just (BS.concat (LBS.toChunks ws')))
                  pure . Just $ ConnectionReceiving tid chan

                | otherwise ->
                    throw (ProtocolError $ "unexpected control header " ++ show w)

    loop :: DispatcherState m -> m ()
    loop !state = do
      !state <- updateStateForFinishedHandlers state
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

                  Just (ConnectionNewChunks peer@(NodeId _) chunks0) -> do
                      mConnState <- handleFirstChunks connid peer (chunks0 ++ chunks) state
                      loop (maybe state (\cs -> Map.insert connid cs state) mConnState)

                  -- Connection is receiving data and there's some handler
                  -- at 'tid' to run it. Dump the new data to its ChannelIn.
                  Just (ConnectionReceiving tid chan) -> do
                    Channel.writeChannel chan (Just (BS.concat chunks))
                    loop state

                  Just (ConnectionClosed tid) ->
                    throw (InternalError "received data on closed connection")

                  -- The peer keeps pushing data but our handler is finished.
                  -- What to do? Would like to close the connection but I'm
                  -- not sure that's possible in network-transport. Would like
                  -- to say "stop receiving on this ConnectionId".
                  -- We could maintain an association between ConnectionId and
                  -- EndPointId of the peer, and then maybe patch
                  -- network-transport to allow for selective closing of peer
                  -- connection based on EndPointAddress.
                  Just (ConnectionHandlerFinished maybeException) ->
                    throw (InternalError "received too much data")

          NT.ConnectionClosed connid ->
              case Map.lookup connid state of
                  Nothing ->
                      throw (InternalError "closed unknown connection")

                  -- Connection closed, handler already finished. We're done
                  -- with the connection. The case in which the handler finishes
                  -- *after* the connection closes is taken care of in
                  -- 'updateStateForFinishedHandlers'.
                  Just (ConnectionHandlerFinished _) ->
                      loop (Map.delete connid state)

                  -- Empty message
                  Just (ConnectionNew peer) -> do
                      chan <- Channel.newChannel
                      Channel.writeChannel chan Nothing
                      tid  <- fork $ finishHandler nodeState (Left connid) (handlerIn peer (ChannelIn chan))
                      loop (Map.insert connid (ConnectionClosed tid) state)

                  -- Small message
                  Just (ConnectionNewChunks peer chunks0) -> do
                      mConnState <- handleFirstChunks connid peer chunks0 state
                      case mConnState of
                          Just (ConnectionReceiving tid chan) -> do
                              -- Write Nothing to indicate end of input.
                              Channel.writeChannel chan Nothing
                              loop (Map.insert connid (ConnectionClosed tid) state)
                          _ -> throw (InternalError "malformed small message")

                  -- End of incoming data. Signal that by writing 'Nothing'
                  -- to the ChannelIn.
                  Just (ConnectionReceiving tid chan) -> do
                      Channel.writeChannel chan Nothing
                      loop (Map.insert connid (ConnectionClosed tid) state)

                  Just (ConnectionClosed tid) ->
                      throw (InternalError "closed a closed connection")

          NT.EndPointClosed ->
              -- TODO: decide what to do with all active handlers
              -- Throw them a special exception?
              return ()

          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode (NT.EventConnectionLost peer)) _msg) ->
              throw (InternalError "Connection lost")

          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode NT.EventEndPointFailed)  msg) ->
              throw (InternalError "EndPoint failed")

          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode NT.EventTransportFailed) msg) ->
              throw (InternalError "Transport failed")

          NT.ErrorEvent (NT.TransportError NT.UnsupportedEvent msg) ->
              throw (InternalError "Unsupported event")

          NT.ConnectionOpened _ _ _ ->
              throw (ProtocolError "unexpected connection reliability")

-- | Augment some m term so that it always updates a 'NodeState' mutable
--   cell when finished, along with the exception if one was raised. We catch
--   all exceptions in order to do this, but they are re-thrown.
--   Use an 'NT.ConnectionId' if the handler is spawned in response to a
--   connection, or a 'Nonce' if the handler is spawned for a locally
--   initiated bidirectional connection.
finishHandler
    :: forall m t .
       ( Mockable SharedAtomic m, Mockable Throw m, Mockable Catch m )
    => SharedAtomicT m (NodeState m)
    -> Either NT.ConnectionId Nonce
    -> m t
    -> m t
finishHandler stateVar connidOrNonce action = normal `catch` exceptional
    where
    normal :: m t
    normal = do
        t <- action
        signalFinished (connidOrNonce, Nothing)
        pure t
    exceptional :: SomeException -> m t
    exceptional e = do
        signalFinished (connidOrNonce, Just e)
        throw e
    -- Signal that a thread handling a given ConnectionId is finished.
    -- It's very important that this is run to completion for every thread
    -- spawned by the dispatcher, else the DispatcherState will never forget
    -- the entry for this ConnectionId.
    signalFinished :: (Either NT.ConnectionId Nonce, Maybe SomeException) -> m ()
    signalFinished outcome = modifySharedAtomic stateVar $ \(NodeState prng nonces finished) ->
            pure ((NodeState prng nonces (outcome : finished)), ())

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
controlHeaderBidirectionalSyn (Nonce nonce) =
    fixedSizeBuilder 9 $
        BS.word8 controlHeaderCodeBidirectionalSyn
     <> BS.word64BE nonce

controlHeaderBidirectionalAck :: Nonce -> BS.ByteString
controlHeaderBidirectionalAck (Nonce nonce) =
    fixedSizeBuilder 9 $
        BS.word8 controlHeaderCodeBidirectionalAck
     <> BS.word64BE nonce

fixedSizeBuilder :: Int -> BS.Builder -> BS.ByteString
fixedSizeBuilder n =
    LBS.toStrict . BS.toLazyByteStringWith (BS.untrimmedStrategy n n) LBS.empty

-- | Connect to a peer given by a 'NodeId' bidirectionally.
connectInOutChannel
    :: ( Mockable Channel.Channel m, Mockable Fork m, Mockable SharedAtomic m
       , Mockable Throw m )
    => Node m
    -> NodeId
    -> m (Nonce, ChannelIn m, ChannelOut m)
connectInOutChannel node@Node{nodeEndPoint, nodeState}
                    (NodeId endpointaddr) = do
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
        -- TODO: error handling
        NT.send outconn [controlHeaderBidirectionalSyn nonce]
        return (nonce, ChannelIn inchan, ChannelOut outconn)
  where
    allocateInChannel = do
      tid   <- myThreadId
      chan  <- Channel.newChannel
      -- Create a nonce and update the shared atomic so that the nonce indicates
      -- that there's a handler for it.
      nonce <- modifySharedAtomic nodeState $ \(NodeState prng expected finished) -> do
                 let (nonce, !prng') = random prng
                     !expected' = Map.insert nonce (NonceHandlerNotConnected tid (ChannelIn chan)) expected
                 pure ((NodeState prng' expected' finished), nonce)
      return (nonce, chan)

-- | Connect to a peer given by a 'NodeId' unidirectionally.
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
        -- TODO error handling
        NT.send conn [controlHeaderUnidirectional]
        return (ChannelOut conn)

closeChannel :: ChannelOut m -> m ()
closeChannel (ChannelOut conn) = NT.close conn

-- | Create, use, and tear down a conversation channel with a given peer
--   (NodeId).
withInOutChannel
    :: forall m a .
       ( Mockable Bracket m, Mockable Fork m, Mockable Channel.Channel m
       , Mockable SharedAtomic m, Mockable Throw m, Mockable Catch m )
    => Node m
    -> NodeId
    -> (ChannelIn m -> ChannelOut m -> m a)
    -> m a
withInOutChannel node@Node{nodeState} nodeid action =
    -- connectInOurChannel will update the nonce state to indicate that there's
    -- a handler for it. When the handler is finished (whether normally or
    -- exceptionally) we have to update it to say so.
    bracket (connectInOutChannel node nodeid)
            (\(_, _, outchan) -> closeChannel outchan)
            (\(nonce, inchan, outchan) -> action' nonce inchan outchan)
    where
    -- Updates the nonce state map always, and re-throws any caught exception.
    action' nonce inchan outchan = finishHandler nodeState (Right nonce) (action inchan outchan)

-- | Create, use, and tear down a unidirectional channel to a peer identified
--   by 'NodeId'.
withOutChannel
    :: ( Mockable Bracket m, Mockable Throw m )
    => Node m
    -> NodeId
    -> (ChannelOut m -> m a)
    -> m a
withOutChannel node nodeid =
    bracket (connectOutChannel node nodeid) closeChannel

-- | Write some ByteStrings to an out channel. It does not close the
--   transport when finished. If you want that, use withOutChannel or
--   withInOutChannel.
writeChannel :: ( Monad m ) => ChannelOut m -> [BS.ByteString] -> m ()
writeChannel (ChannelOut _) [] = pure ()
writeChannel (ChannelOut conn) (chunk:chunks) = do
    res <- NT.send conn [chunk]
    -- Any error detected here will be reported to the dispatcher thread
    -- so we don't need to do anything
     --TODO: though we could log here
    case res of
      Left _err -> return ()
      Right _   -> writeChannel (ChannelOut conn) chunks

-- | Read a 'ChannelIn', blocking until the next 'ByteString' arrives, or end
--   of input is signalled via 'Nothing'.
readChannel :: ( Mockable Channel.Channel m ) => ChannelIn m -> m (Maybe BS.ByteString)
readChannel (ChannelIn chan) = Channel.readChannel chan
