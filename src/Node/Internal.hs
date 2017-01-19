{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Node.Internal (
    NodeId(..),
    Node(..),
    nodeId,
    nodeEndPointAddress,
    ChannelIn,
    ChannelOut,
    startNode,
    stopNode,
    withOutChannel,
    withInOutChannel,
    writeChannel,
    ConnectionEvent(..),
    readChannel
  ) where

import           Control.Exception             hiding (bracket, catch, finally, throw)
import           Control.Monad                 (forM_)
import           Control.Monad.Fix             (MonadFix)
import           Data.Binary                   as Bin
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Foldable                 (foldlM)
import           Data.Hashable                 (Hashable)
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Monoid
import           Data.Typeable
import           Formatting                    (sformat, shown, (%))
import qualified Mockable.Channel              as Channel
import           Mockable.Class
import           Mockable.Concurrent
import           Mockable.Exception
import           Mockable.SharedAtomic
import           Mockable.SharedExclusive
import qualified Network.Transport             as NT (EventErrorCode (EventConnectionLost, EventEndPointFailed, EventTransportFailed))
import qualified Network.Transport.Abstract    as NT
import           Network.Transport.ConnectionBuffers
import           System.Random                 (Random, StdGen, random)
import           System.Wlog                   (WithLogger, logDebug, logError)
import qualified Node.Message                  as Message

-- | A 'NodeId' wraps a network-transport endpoint address
newtype NodeId = NodeId NT.EndPointAddress
  deriving (Eq, Ord, Show, Hashable)

-- | The state of a Node, to be held in a shared atomic cell because other
--   threads will mutate it in order to set up bidirectional connections.
data NodeState m = NodeState {
      _nodeStateGen                :: !StdGen
      -- ^ To generate nonces.
    , _nodeStateNonces             :: !(Map Nonce (Promise m (), Maybe (SharedExclusiveT m (ConnectionBuffer m))))
      -- ^ Handlers for each nonce which we generated (locally-initiated
      --   bidirectional connections).
      --   If the second component is Just, then it's an empty shared exclusive.
    , _noceStateConnectionHandlers :: !(Set (Promise m ()))
      -- ^ A set of handlers for connections (remotely-initiated unidirectional
      --   or bidirectional connections).
    , _nodeStateClosed             :: !Bool
      -- ^ Indicates whether the Node has been closed and is no longer capable
      --   of establishing or accepting connections (its EndPoint is closed).
    }

-- | A 'Node' is a network-transport 'EndPoint' with bidirectional connection
--   state and a thread to dispatch network-transport events.
data Node (m :: * -> *) = Node {
       nodeEndPoint         :: NT.EndPoint m (EndPointEvent m),
       nodeDispatcherThread :: Promise m (),
       nodeState            :: SharedAtomicT m (NodeState m)
     }

nodeId :: Node m -> NodeId
nodeId = NodeId . NT.address . nodeEndPoint

nodeEndPointAddress :: NodeId -> NT.EndPointAddress
nodeEndPointAddress (NodeId addr) = addr

-- | Used to identify bidirectional connections.
newtype Nonce = Nonce {
      _getNonce :: Word64
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

data ChannelIn m =
    -- | A channel for a remotely-initiated connection, whether bidirectional
    --   or unidirectional.
    ChannelRemotelyInitiated !(ConnectionBuffer m)
    -- | A channel for a locally-initiated bidirectional connection. The
    --   shared exclusive will become full once the peer establishes the other
    --   direction.
  | ChannelLocallyInitiated !(SharedExclusiveT m (ConnectionBuffer m))

-- | Output to the wire.
newtype ChannelOut m = ChannelOut (NT.Connection m)

closeChannel :: ChannelOut m -> m ()
closeChannel (ChannelOut conn) = NT.close conn

-- | Write some ByteStrings to an out channel. It does not close the
--   transport when finished. If you want that, use withOutChannel or
--   withInOutChannel.
writeChannel
    :: ( Monad m, WithLogger m, Mockable Throw m )
    => ChannelOut m
    -> [BS.ByteString]
    -> m ()
writeChannel (ChannelOut conn) chunks = do
    res <- NT.send conn chunks
    -- TBD error handling? Throw exception or report it as a Left?
    case res of
      Left err -> throw err
      Right _  -> pure ()

-- | Read from a ChannelIn with the option to replace unconsumed output.
readChannel
    :: ( Mockable SharedExclusive m, Mockable Buffer m
       , Message.Unpackable packingType thing )
    => packingType
    -> t -- ^ Use for closed channel
    -> t -- ^ Use for lost channel
    -> t -- ^ Use for failed parse
    -> (thing -> t) -- ^ Use for successful parse.
    -> ChannelIn m -- ^ Parse from here (if data is needed).
    -> m t
readChannel packingType closed lost fail done inputChan = case Message.unpackMsg packingType of
    Message.Done _ _ t -> return $ done t
    Message.Fail _ _ _ -> return $ fail
    Message.Partial continue -> case inputChan of
      ChannelRemotelyInitiated buffer -> go buffer continue
      ChannelLocallyInitiated exclusive -> do
          buffer <- readSharedExclusive exclusive
          go buffer continue
    where
    go buffer continue = do
        next <- readBuffer buffer $ \ev -> case ev of
            Closed -> (Nothing, Right closed)
            Lost -> (Nothing, Right lost)
            Data bs -> case continue (Just (LBS.toStrict bs)) of
                Message.Done trailing _ t -> (leftover trailing, Right (done t))
                Message.Fail trailing _ _ -> (leftover trailing, Right fail)
                Message.Partial continue' -> (Nothing, Left continue')
        case next of 
            Left continue' -> go buffer continue'
            Right t -> return t

    leftover bs = if BS.null bs then Nothing else Just (Data (LBS.fromStrict bs))
    

-- | Bring up a 'Node' using a network transport.
startNode :: ( Mockable SharedAtomic m, Mockable Bracket m
             , Mockable Buffer m, Mockable Throw m, Mockable Catch m
             , Mockable Async m, Mockable Concurrently m, Ord (Promise m ())
             , Mockable Buffer m, Mockable SharedExclusive m
             , MonadFix m, WithLogger m )
          => NT.Transport m
          -> StdGen
          -> (NodeId -> ChannelIn m -> m ())
          -- ^ Handle incoming unidirectional connections.
          -> (NodeId -> ChannelIn m -> ChannelOut m -> m ())
          -- ^ Handle incoming bidirectional connections.
          -> m (Node m)
startNode transport prng handlerIn handlerOut = do
    qdisc <- connectionBufferQDisc $ ConnectionBuffersParams {
                   qdiscEventBufferSize = 1024
                   -- At most 1024 * 4096 + c bytes in the pipe for a
                   -- connection
                 , qdiscConnectionBufferSize = 1024
                 , qdiscConnectionDataSize = 4096
                 , qdiscInternalError = \ie -> do
                       logDebug (sformat shown ie)
                       throw ie
                 }
    Right endPoint <- NT.newEndPoint transport qdisc
    sharedState <- newSharedAtomic (NodeState prng Map.empty Set.empty False)
    -- TODO this thread should get exceptions from the dispatcher thread.
    dispatcherThread <- async $
        nodeDispatcher endPoint sharedState handlerIn handlerOut
    return Node {
      nodeEndPoint         = endPoint,
      nodeDispatcherThread = dispatcherThread,
      nodeState            = sharedState
    }

-- | Stop a 'Node', closing its network transport endpoint.
stopNode :: ( WithLogger m, Mockable Async m, Mockable SharedAtomic m ) => Node m -> m ()
stopNode Node {..} = do
    modifySharedAtomic nodeState $ \(NodeState prng nonces handlers closed) ->
        if closed
        then error "Node internal error : already stopped"
        else pure (NodeState prng nonces handlers True, ())
    -- This eventually will shut down the dispatcher thread, which in turn
    -- ought to stop the connection handling threads.
    -- It'll also close all TCP connections.
    NT.closeEndPoint nodeEndPoint
    -- Must wait on any handler threads. The dispatcher thread will eventually
    -- see an event indicating that the end point has closed, after which it
    -- will wait on all running handlers. Since the end point has been closed,
    -- no new handler threads will be created, so this will block indefinitely
    -- only if some handler is blocked indefinitely or looping.
    wait nodeDispatcherThread

-- | The state of a connection to a peer.
data ConnectionState m =
      -- | New connection from some peer.
      Unprocessed
      -- | Handshake completed and it's unidirectional.
    | UnidirectionalEstablished (ChannelIn m)
      -- | Handhsake completed and it's bidirectional.
    | BidirectionalEstablished (ChannelIn m) (ChannelOut m)
      -- | We got an ACK from the peer.
    | BidirectionalGotACK
      -- | We got a SYN from the peer.
    | BidirectionalGotSYN

-- | The one thread that handles /all/ incoming messages and dispatches them
-- to various handlers.
nodeDispatcher :: forall m .
                  ( Mockable SharedAtomic m, Mockable Async m, Mockable Concurrently m
                  , Ord (Promise m ()), Mockable Bracket m, Mockable SharedExclusive m
                  , Mockable Buffer m, Mockable Throw m
                  , Mockable Catch m
                  , MonadFix m, WithLogger m )
               => NT.EndPoint m (EndPointEvent m)
               -> SharedAtomicT m (NodeState m)
               -- ^ Nonce states and a StdGen to generate nonces. It's in a
               --   shared atomic because other threads must be able to alter
               --   it when they start a conversation.
               --   The third element of the triple will be updated by handler
               --   threads when they finish.
               -> (NodeId -> ChannelIn m -> m ())
               -> (NodeId -> ChannelIn m -> ChannelOut m -> m ())
               -> m ()
nodeDispatcher endpoint nodeState handlerIn handlerInOut = loop

    where

    -- Much work is done by the ConnectionBuffers QDisc to organize
    -- network-transport opened, received, and closed events into a single
    -- 'PeerOpenedConnection' which comes with an input buffer.
    --
    -- The dispatcher here is responsible for spawning handlers for each of
    -- those 'PeerOpenedConnection' events. They will do a handshake with the
    -- peer to establish a unidirectional or bidirectional connection, and
    -- then give control to the relevant caller-supplied handler.
    --
    -- The simplest case is a unidirectional incoming connection, in which
    -- the dispatcher needs only to match the first byte of input with the
    -- unidirectional control byte, and then pass the input buffer to the
    -- caller-supplied handler. This is done in a new thread, and the handler
    -- runs in the same thread. The dispatcher will keep this thread in
    -- shared state so that, if it is shut down, it can wait until the thread
    -- finishes before finishing itself.

    -- A loop to handle 'EndPointEvent's.
    loop :: m ()
    loop = do
      event <- NT.receive endpoint
      case event of

          -- Peer opened a connection.
          -- Spawn a thread to handle it.
          PeerOpenedConnection connid NT.ReliableOrdered peer connBuffer -> do
              spawnHandler nodeState (Left connid) (peerConnectionHandler peer connid connBuffer Unprocessed)
              loop

          -- Only reliable, ordered connections are accepted.
          -- TODO close the connection. No sense receiving any data on it.
          PeerOpenedConnection _ _ addr _ -> do
              logError $ sformat ("Unexpected connection reliability from" % shown) addr
              loop

          -- Wait for the handlers to finish before returning.
          LocalEndPointClosed -> waitForHandlers

          LocalEndPointFailed -> do
              waitForHandlers
              throw (InternalError "EndPoint failed")

          LocalTransportFailed -> do
              waitForHandlers
              throw (InternalError "Transport failed")

    -- | Handle a connection from a peer. Spawned in a new thread for every
    --   'PeerOpenedConnection' event.
    --   Wait for a control header indicating whether the connection is
    --   unidirectional or bidirectional, then pass control onto the relevant
    --   handler supplied by the call to 'startNode'.
    peerConnectionHandler
        :: NT.EndPointAddress
        -> NT.ConnectionId
        -> ConnectionBuffer m
        -> ConnectionState m
        -> m ()
    peerConnectionHandler peer connid connBuffer state = case state of

        Unprocessed -> do
            -- Take 1 byte and check for a control header.
            next <- recvAtMost 1 connBuffer
            case next of
                -- TODO Debug messages.
                Lost -> logError $ sformat ("unprocessed connection lost " % shown) peer
                Closed -> logError $ sformat ("unprocessed connection closed " % shown) peer
                -- How to handle too much input? We want to only take at most
                -- n bytes from the buffer... 
                Data bs -> case LBS.unpack bs of
                    [control] -> do
                        if control == controlHeaderCodeUnidirectional
                        then peerConnectionHandler peer connid connBuffer (UnidirectionalEstablished (ChannelRemotelyInitiated connBuffer))
                        else if control == controlHeaderCodeBidirectionalSyn
                        then peerConnectionHandler peer connid connBuffer BidirectionalGotSYN
                        else if control == controlHeaderCodeBidirectionalAck
                        then peerConnectionHandler peer connid connBuffer BidirectionalGotACK
                        else logError $ sformat ("unrecognized control byte " % shown) peer
                    [] -> do
                        let err = InternalError "recvAtMost 1 gave 0 bytes"
                        logError $ sformat shown err
                        throw err
                    _ -> do
                        let err = InternalError "recvAtMost 1 gave >1 bytes"
                        logError $ sformat shown err
                        throw err

        -- We got an ACK. We use shared state to determine whether there's a
        -- running handler corresponding to that nonce.
        -- If there isn't, it's not necessarily an error. It could be that the
        -- handler finished before the peer even acknowledged.
        --
        -- All we must do here is update the shared exclusive for that nonce to
        -- have the input buffer for this connection.
        BidirectionalGotACK -> do
            nonce <- recvNonce peer connBuffer
            modifySharedAtomic nodeState $ \(NodeState prng locally remotely closed) -> do
                locally' <- case Map.lookup nonce locally of
                    -- Not an error. Could be that the nonce handler already
                    -- finished.
                    Nothing -> return locally
                    Just (promise, Nothing) -> do
                        logError $ sformat ("duplicate ACK " % shown % shown) peer nonce
                        undefined
                    Just (promise, Just var) -> do
                        putSharedExclusive var connBuffer
                        return $ Map.insert nonce (promise, Nothing) locally
                return (NodeState prng locally' remotely closed, ())
            -- This thread ends, but the locally-initiated thread for that nonce
            -- may live on.

        -- We got a SYN. Receive the nonce, try to establish a connection and
        -- send an ACK, then run the handler.
        BidirectionalGotSYN -> do
            nonce <- recvNonce peer connBuffer
            mconn <- modifySharedAtomic nodeState $ \ns@(NodeState _prng _nonces _finishedHandlers closed) ->
                if closed
                then pure (ns, Nothing)
                else do
                    mconn <- NT.connect
                        endpoint
                        peer
                        NT.ReliableOrdered
                        NT.ConnectHints{ connectTimeout = Nothing } --TODO use timeout
                    case mconn of
                      Left  err  -> throw err
                      Right conn -> pure (ns, Just conn)
            case mconn of
                -- The EndPoint is closed, so we do nothing.
                -- TODO should probably log this.
                Nothing -> logDebug $ sformat ("Got connection\
                    \request from "%shown%", but endpoint is closed") peer
                Just conn -> do
                    outcome <- NT.send conn [controlHeaderBidirectionalAck nonce]
                    case outcome of
                        Left err -> logDebug $
                            sformat ("Error sending ack to "%shown%": " %shown) peer err
                        Right _ -> do
                            let input = ChannelRemotelyInitiated connBuffer
                            let output = ChannelOut conn
                            peerConnectionHandler peer connid connBuffer (BidirectionalEstablished input output)

        UnidirectionalEstablished input -> handlerIn (NodeId peer) input

        BidirectionalEstablished input output -> handlerInOut (NodeId peer) input output

    -- Try to receive an 8-byte nonce.
    recvNonce :: NT.EndPointAddress -> ConnectionBuffer m -> m Nonce
    recvNonce peer connBuffer = do
        next <- recvAtMost 8 connBuffer
        case next of
            Lost -> do
                logError $ sformat ("expected nonce lost " % shown) peer
                undefined
            Closed -> do
                logError $ sformat ("expected nonce closed " % shown) peer
                undefined
            Data bs -> case decodeOrFail bs of
                Right (_,_,nonce) -> pure nonce
                _ -> do
                    logError $ sformat ("malformed nonce " % shown) peer
                    undefined

    -- | Wait for all running handlers to finish.
    waitForHandlers :: m ()
    waitForHandlers = do
        (locally, remotely) <- modifySharedAtomic nodeState $ \ns@(NodeState _ locally remotely _) ->
            pure (ns, (locally, remotely))
        let locallyInitiated :: [Promise m ()]
            locallyInitiated = fmap fst (Map.elems locally)
        let remotelyInitiated :: [Promise m ()]
            remotelyInitiated = Set.toList remotely
        let allHandlerPromises = locallyInitiated ++ remotelyInitiated
        -- All we have to do is wait. Sending on the out channels will fail
        -- because the transport is assumed to be closed, and receiving on
        -- the in channels will eventually give Lost or Closed and certainly
        -- not block forever.
        _ <- forConcurrently allHandlerPromises wait
        pure ()

-- | Spawn a thread and track it in shared state, taking care to remove it from
--   shared state when it's finished.
spawnHandler
    :: forall m t .
       ( Mockable SharedAtomic m, Mockable Throw m, Mockable Catch m
       , Mockable Async m, Ord (Promise m ()), MonadFix m )
    => SharedAtomicT m (NodeState m)
    -> Either NT.ConnectionId (Nonce, SharedExclusiveT m (ConnectionBuffer m))
    -> m t
    -> m (Promise m t)
spawnHandler stateVar connidOrNonce action =
    modifySharedAtomic stateVar $ \(NodeState prng locally remotely closed) -> do
        -- Spawn the thread to get a 'SomeHandler'.
        rec { promise <- async $ normal waitForIt `catch` exceptional waitForIt
            -- A new promise which finishes when the other promise does, but returns
            -- ().
            ; waitForIt <- async $ do
                  wait promise
                  pure ()
            }
        -- It is assumed that different promises do not compare equal.
        -- It is assumed to be highly unlikely that there will be nonce
        -- collisions (that we have a good prng).
        let (locally', remotely') = case connidOrNonce of
                Left connid -> (locally, Set.insert waitForIt remotely)
                Right (nonce, var) -> (Map.insert nonce (waitForIt, Just var) locally, remotely)
        return (NodeState prng locally' remotely' closed, promise)
    where

    normal :: Promise m () -> m t
    normal promise = do
        t <- action
        signalFinished promise Nothing
        pure t

    exceptional :: Promise m () -> SomeException -> m t
    exceptional promise e = do
        signalFinished promise (Just e)
        throw e

    signalFinished :: Promise m () -> Maybe SomeException -> m ()
    signalFinished promise outcome = modifySharedAtomic stateVar $ \(NodeState prng locally remotely closed) -> do
        let (locally', remotely') = case connidOrNonce of
                Left connid -> (locally, Set.delete promise remotely)
                Right (nonce, _) -> (Map.delete nonce locally, remotely)
        pure ((NodeState prng locally' remotely' closed), ())

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
    :: ( Mockable Buffer m, Mockable SharedAtomic m
       , Mockable Throw m, WithLogger m )
    => Node m
    -> NodeId
    -> Nonce
    -> m (ChannelOut m)
connectInOutChannel Node{nodeEndPoint, nodeState} (NodeId endpointaddr) nonce = do
    mconn <- NT.connect
                 nodeEndPoint
                 endpointaddr
                 NT.ReliableOrdered
                 NT.ConnectHints{ connectTimeout = Nothing } --TODO use timeout

    -- TODO: Any error detected here needs to be reported because it's not
    -- reported via the dispatcher thread. It means we cannot establish a
    -- connection in the first place, e.g. timeout.
    case mconn of
      Left err -> throw err
      Right outconn -> do
        outcome <- NT.send outconn [controlHeaderBidirectionalSyn nonce]
        case outcome of
            Left err -> logDebug $ sformat
                ("Error initializing bidirectional connection to "%shown%": "%shown)
                endpointaddr err
            Right _ -> return ()
        return (ChannelOut outconn)

-- | Create, use, and tear down a conversation channel with a given peer
--   (NodeId).
withInOutChannel
    :: forall m a .
       ( Mockable Bracket m, Mockable Async m, Ord (Promise m ())
       , Mockable SharedAtomic m, Mockable SharedExclusive m, Mockable Throw m
       , Mockable Catch m, Mockable Buffer m, MonadFix m, WithLogger m )
    => Node m
    -> NodeId
    -> (ChannelIn m -> ChannelOut m -> m a)
    -> m a
withInOutChannel node@Node{nodeState} nodeid action = do
    input <- newSharedExclusive
    nonce <- modifySharedAtomic nodeState $ \(NodeState prng locally remotely closed) -> do
               let (nonce, !prng') = random prng
               pure ((NodeState prng' locally remotely closed), nonce)
    let action' :: ChannelOut m -> m a
        action' = action (ChannelLocallyInitiated input)
    -- connectInOurChannel will update the nonce state to indicate that there's
    -- a handler for it. When the handler is finished (whether normally or
    -- exceptionally) we have to update it to say so.
    rec { promise <- spawnHandler nodeState (Right (nonce, input)) $
              bracket (connectInOutChannel node nodeid nonce)
                      (closeChannel)
                      (action')
        }
    wait promise

-- | Connect to a peer given by a 'NodeId' unidirectionally.
connectOutChannel
    :: ( Mockable Throw m, WithLogger m )
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
    --
    -- TODO: don't report the error if the end point is closed.
    case mconn of
      Left  err  -> throw err
      Right conn -> do
        outcome <- NT.send conn [controlHeaderUnidirectional]
        case outcome of
            Left err -> logDebug $ sformat
                ("Error initializing unidirectional connection to "%shown%": "%shown)
                endpointaddr err
            Right _ -> return ()
        return (ChannelOut conn)

-- | Create, use, and tear down a unidirectional channel to a peer identified
--   by 'NodeId'.
withOutChannel
    :: ( Mockable Bracket m, Mockable Throw m, WithLogger m )
    => Node m
    -> NodeId
    -> (ChannelOut m -> m a)
    -> m a
withOutChannel node nodeid =
    bracket (connectOutChannel node nodeid) closeChannel
