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
    ChannelIn(..),
    ChannelOut(..),
    startNode,
    stopNode,
    withOutChannel,
    withInOutChannel,
    writeChannel,
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
import           Data.Monoid
import           Data.NonEmptySet              (NonEmptySet)
import qualified Data.NonEmptySet              as NESet
import           Data.Typeable
import           Formatting                    (sformat, shown, (%))
import qualified Mockable.Channel              as Channel
import           Mockable.Class
import           Mockable.Concurrent
import           Mockable.Exception
import           Mockable.SharedAtomic
import qualified Network.Transport             as NT (EventErrorCode (EventConnectionLost, EventEndPointFailed, EventTransportFailed))
import qualified Network.Transport.Abstract    as NT
import           System.Random                 (Random, StdGen, random)
import           System.Wlog                   (WithLogger, logDebug)

-- | A 'NodeId' wraps a network-transport endpoint address
newtype NodeId = NodeId NT.EndPointAddress
  deriving (Eq, Ord, Show, Hashable)

-- | The state of a Node, to be held in a shared atomic cell because other
--   threads will mutate it in order to set up bidirectional connections.
data NodeState m = NodeState {
      _nodeStateGen              :: !StdGen
      -- ^ To generate nonces.
    , _nodeStateNonces           :: !(Map Nonce (NonceState m))
      -- ^ Nonces identify bidirectional connections, and this gives the state
      --   of each one.
    , _nodeStateFinishedHandlers :: ![(Either NT.ConnectionId Nonce, Maybe SomeException)]
      -- ^ Connection identifiers or nonces for handlers which have finished.
      --   'Nonce's for bidirectional connections, 'ConnectionId's for handlers
      --   spawned to respond to incoming connections.
    , _nodeStateClosed           :: !Bool
      -- ^ Indicates whether the Node has been closed and is no longer capable
      --   of establishing or accepting connections (its EndPoint is closed).
    }

-- | A 'Node' is a network-transport 'EndPoint' with bidirectional connection
--   state and a thread to dispatch network-transport events.
data Node (m :: * -> *) = Node {
       nodeEndPoint         :: NT.EndPoint m,
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

-- | Input from the wire. Nothing means there's no more to come.
newtype ChannelIn m = ChannelIn (Channel.ChannelT m (Maybe BS.ByteString))

-- | Output to the wire.
newtype ChannelOut m = ChannelOut (NT.Connection m)

-- | Bring up a 'Node' using a network transport.
startNode :: ( Mockable SharedAtomic m, Mockable Bracket m
             , Mockable Channel.Channel m, Mockable Throw m, Mockable Catch m
             , Mockable Async m
             , WithLogger m )
          => NT.Transport m
          -> StdGen
          -> (NodeId -> ChannelIn m -> m ())
          -- ^ Handle incoming unidirectional connections.
          -> (NodeId -> ChannelIn m -> ChannelOut m -> m ())
          -- ^ Handle incoming bidirectional connections.
          -> m (Node m)
startNode transport prng handlerIn handlerOut = do
    Right endPoint <- NT.newEndPoint transport
    sharedState <- newSharedAtomic (NodeState prng Map.empty [] False)
    dispatcherThread <- async $
        nodeDispatcher endPoint sharedState handlerIn handlerOut
    return Node {
      nodeEndPoint         = endPoint,
      nodeDispatcherThread = dispatcherThread,
      nodeState            = sharedState
    }

-- | Stop a 'Node', closing its network transport endpoint.
stopNode :: ( Mockable Async m, Mockable SharedAtomic m ) => Node m -> m ()
stopNode Node {..} = do
    modifySharedAtomic nodeState $ \(NodeState prng nonces finishedHandlers closed) ->
        if closed
        then error "Node internal error : already stopped"
        else pure (NodeState prng nonces finishedHandlers True, ())
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

-- | State which is local to the dispatcher, and need not be accessible to
--   any other thread.
data DispatcherState m = DispatcherState {
      dispatcherConnections :: !(Map NT.ConnectionId (NT.EndPointAddress, ConnectionState m))
    , dispatcherPeers :: !(Map NT.EndPointAddress (NonEmptySet NT.ConnectionId))
      -- ^ For each peer 'EndPointAddress', the set of all 'ConnectionId's such
      -- that
      --
      --   elem connId <$> lookup endPointAddress (dispatcherPeers state) == Just True
      --   => fst <$> lookup connId (dispatcherConnections state) == Just endPointAddress
      --
      --
    }

emptyDispatcherState :: DispatcherState m
emptyDispatcherState = DispatcherState Map.empty Map.empty

data SomeHandler m = forall t . SomeHandler (Promise m t)

asyncHandler :: ( Mockable Async m ) => m t -> m (SomeHandler m)
asyncHandler = fmap SomeHandler . async

waitForHandler :: ( Mockable Async m ) => SomeHandler m -> m ()
waitForHandler (SomeHandler promise) = wait promise >> pure ()

-- | The state of a connection (associated with a 'ConnectionId', see
--   'DispatcherState'.
data ConnectionState m =

       -- | We got a new connection and are waiting on the first chunk of data
      ConnectionNew

      -- | We got the first chunk of data, we're now waiting either for more
      --   data or for the connection to be closed. This supports the small
      --   message optimisation.
    | ConnectionNewChunks ![BS.ByteString]

      -- | We've forked a thread to handle the message. The connection is still
      --   open and data is still arriving. We have a channel to pass the
      --   incoming chunks off to the other thread.
    | ConnectionReceiving !(SomeHandler m) !(ChannelIn m)

      -- | We've forked a thread to handle the message. The connection is now
      --   closed and we have all the data already, but the thread we forked
      --   to handle it is still active.
    | ConnectionClosed !(SomeHandler m)

      -- | The handler which we forked to process the data has finished.
      --   Subsequent incoming data has nowhere to go.
    | ConnectionHandlerFinished !(Maybe SomeException)

instance Show (ConnectionState m) where
    show term = case term of
        ConnectionNew               -> "ConnectionNew"
        ConnectionNewChunks _       -> "ConnectionNewChunks"
        ConnectionReceiving _ _     -> "ConnectionReceiving"
        ConnectionClosed _          -> "ConnectionClosed"
        ConnectionHandlerFinished e -> "ConnectionHandlerFinished" ++ maybe " " ((++) " " . show) e

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

      -- | The local handler for the conversation is running but no ACK has
      -- yet been received from the peer.
      NonceHandlerNotConnected !(SomeHandler m) !(ChannelIn m)

      -- | SYN/ACK completed and the local handler for the conversation
      -- is now a typical connection.
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
                  ( Mockable SharedAtomic m, Mockable Async m, Mockable Bracket m
                  , Mockable Channel.Channel m, Mockable Throw m
                  , Mockable Catch m
                  , WithLogger m )
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
    loop emptyDispatcherState
    where

    finally' :: m t -> m () -> m t
    finally' action after = bracket (pure ()) (const after) (const action)

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
    updateStateForFinishedHandlers state = modifySharedAtomic nodeState $ \(NodeState prng nonces finished closed) -> do
        -- For every Left (c :: NT.ConnectionId) we can remove it from the map
        -- if its connection is closed, or indicate that the handler is finished
        -- so that it will be removed when the connection is closed.
        --
        -- For every Right (n :: Nonce) we act depending on the nonce state.
        -- If the handler isn't connected, we'll just drop the nonce state
        -- entry.
        -- If the handler is connected, we'll update the connection state for
        -- that ConnectionId to say the handler has finished.
        (!state', !nonces') <- foldlM folder (state, nonces) finished
        --() <- trace ("DEBUG: state map size is " ++ show (Map.size state')) (pure ())
        --() <- trace ("DEBUG: nonce map size is " ++ show (Map.size nonces')) (pure ())
        pure ((NodeState prng nonces' [] closed), state')
        where

        -- Updates connection and nonce states in a left fold.
        folder
            :: (DispatcherState m, Map Nonce (NonceState m))
            -> (Either NT.ConnectionId Nonce, Maybe SomeException)
            -> m (DispatcherState m, Map Nonce (NonceState m))
        folder (dispatcherState, nonces) (connidOrNonce, e) = case connidOrNonce of
            -- The handler for a ConnectionId has finished.
            Left connid -> (,) <$> updateDispatcherState dispatcherState connid e <*> pure nonces
            -- The handler for a locally-iniated conversation has finished.
            Right nonce -> folderNonce (dispatcherState, nonces) (nonce, e)

        folderNonce (dispatcherState, nonces) (nonce, e) = case Map.lookup nonce nonces of
            Nothing -> throw $ InternalError "handler for unknown nonce finished"
            -- The handler did not yet connect (SYN/ACK handshake did not go
            -- through). It's weird, but normal: the conversation did not
            -- 'recv' any data and ended before the peer could respond with the
            -- ACK.
            Just (NonceHandlerNotConnected _ _) ->
                pure (dispatcherState, Map.delete nonce nonces)
            -- The handler has connected, so we update the relevant parts of
            -- the DispatcherState.
            Just (NonceHandlerConnected connid) ->
                (,) <$> updateDispatcherState dispatcherState connid e <*> pure (Map.delete nonce nonces)

        updateDispatcherState dispatcherState connid e = case Map.lookup connid (dispatcherConnections dispatcherState) of
            Nothing -> throw $ InternalError "handler for unknown connection finished"
            -- Handler finished after the connection closed. Now we can
            -- remove this entry from the map, as well as from the reverse
            -- lookup map.
            Just (peer, ConnectionClosed _) -> pure $ dispatcherState {
                  dispatcherConnections = Map.delete connid (dispatcherConnections dispatcherState)
                , dispatcherPeers = Map.update (NESet.delete connid) peer (dispatcherPeers dispatcherState)
                }
            -- Handler finished before the connection closed. Update the
            -- connections map and it will be cleared when the connection
            -- closes.
            Just (addr, _) -> pure $ dispatcherState {
                  dispatcherConnections = Map.insert connid (addr, ConnectionHandlerFinished e) (dispatcherConnections dispatcherState)
                }

    -- | Wait for all running handlers to finish.
    waitForRunningHandlers :: DispatcherState m -> m ()
    waitForRunningHandlers state = do
        nonces <- modifySharedAtomic nodeState $ \ns@(NodeState _prng nonces _finished _closed) ->
            pure (ns, nonces)
        let outgoingHandlerPromises :: [(SomeHandler m, Maybe (ChannelIn m))]
            outgoingHandlerPromises = foldr pickOutgoingHandlerPromise [] nonces
        let allHandlerPromises = incomingHandlerPromises ++ outgoingHandlerPromises
        forM_ allHandlerPromises (closeChannelIn . snd)
        forM_ allHandlerPromises (waitForHandler . fst)
        where

        -- Close the in channel by writing Nothing, to signal end-of-input.
        -- TBD should we signal "premature end of input" instead, to
        -- differentiate it from the peer terminating the connection?
        closeChannelIn :: Maybe (ChannelIn m) -> m ()
        closeChannelIn Nothing                 = pure ()
        closeChannelIn (Just (ChannelIn chan)) = Channel.writeChannel chan Nothing

        incomingHandlerPromises :: [(SomeHandler m, Maybe (ChannelIn m))]
        incomingHandlerPromises = foldr pickIncomingHandlerPromise [] (dispatcherConnections state)

        pickIncomingHandlerPromise (_, ConnectionReceiving promise inchan) = (:) (promise, Just inchan)
        pickIncomingHandlerPromise (_, ConnectionClosed promise) = (:) (promise, Nothing)
        pickIncomingHandlerPromise _ = id

        pickOutgoingHandlerPromise (NonceHandlerNotConnected promise inchan) = (:) (promise, Just inchan)
        pickOutgoingHandlerPromise _ = id

    -- Handle the first chunks received, interpreting the control byte(s).
    -- TODO: review this. It currently does not use the 'DispatcherState' but
    -- that's dubious. Implementing good error handling will probably demand
    -- using it.
    handleFirstChunks
        :: NT.ConnectionId
        -> NT.EndPointAddress
        -> [BS.ByteString]
        -> DispatcherState m
        -> m (Maybe (ConnectionState m))
    handleFirstChunks connid peer chunks _ =
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
                  promise <- asyncHandler $ finishHandler nodeState (Left connid) (handlerIn (NodeId peer) (ChannelIn chan))
                  pure . Just $ ConnectionReceiving promise (ChannelIn chan)

                -- Bidirectional header without the nonce
                -- attached. Wait for the nonce.
                | w == controlHeaderCodeBidirectionalAck ||
                  w == controlHeaderCodeBidirectionalSyn
                , LBS.length ws < 8 -> -- need more data
                  pure . Just $ ConnectionNewChunks chunks

                -- Peer wants a bidirectional connection.
                -- Fork a thread to reply with an ACK and then
                -- handle according to handlerInOut.
                | w == controlHeaderCodeBidirectionalSyn
                , Right (ws',_,nonce) <- decodeOrFail ws -> do
                  chan <- Channel.newChannel
                  Channel.writeChannel chan (Just (BS.concat (LBS.toChunks ws')))
                  let action = do
                          -- It's possible that the local EndPoint is closed.
                          -- If that's the case, we must not try to establish
                          -- a connection to the peer who sent the SYN. Instead,
                          -- we just ignore it. network-transport should
                          -- ensure that the peer receives an error event
                          -- indicating that we've closed their connection.
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
                                        sformat ("Error sending ack to "%shown%": "
                                        %shown) peer err
                                      Right _  -> return ()
                                  handlerInOut (NodeId peer) (ChannelIn chan) (ChannelOut conn)
                                      `finally'`
                                      closeChannel (ChannelOut conn)
                  promise <- asyncHandler $ finishHandler nodeState (Left connid) action
                  pure . Just $ ConnectionReceiving promise (ChannelIn chan)

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
                  outcome <- modifySharedAtomic nodeState $ \(NodeState prng expected finished closed) ->
                      case Map.lookup nonce expected of
                          -- Got an ACK for a nonce that we don't know about
                          -- It could be that we never sent a SYN on that
                          -- nonce (peer made a protocol error), but it could
                          -- also be that we did send the SYN, but our handler
                          -- for that conversation has already finished!
                          -- That's just normal.
                          Nothing -> return ((NodeState prng expected finished closed), Nothing)
                          -- Got an ACK for a nonce that we *do* know about (we
                          -- sent a SYN for it).
                          Just (NonceHandlerNotConnected promise inchan) -> do
                              let !expected' = Map.insert nonce (NonceHandlerConnected connid) expected
                              return ((NodeState prng expected' finished closed), Just (promise, inchan))
                          Just _ -> throw (InternalError $ "duplicate or delayed ACK for " ++ show nonce)
                  case outcome of
                      Nothing -> pure Nothing
                      Just (promise, ChannelIn chan) -> do
                          Channel.writeChannel chan (Just (BS.concat (LBS.toChunks ws')))
                          pure . Just $ ConnectionReceiving promise (ChannelIn chan)

                | otherwise -> do
                    logDebug $ sformat ("protocol error: unexpected control header "
                        %shown%" from "%shown) w peer
                    pure Nothing

    loop :: DispatcherState m -> m ()
    loop !initialState = do
      !state <- updateStateForFinishedHandlers initialState
      event <- NT.receive endpoint
      case event of

          NT.ConnectionOpened connid NT.ReliableOrdered peer ->
              -- Just keep track of the new connection and reverse lookup
              -- from EndPointAddress.
              loop $ state {
                    dispatcherConnections = Map.insert connid (peer, ConnectionNew) (dispatcherConnections state)
                  , dispatcherPeers = Map.alter (Just . maybe (NESet.singleton connid) (NESet.insert connid)) peer (dispatcherPeers state)
                  }

          -- receiving data for an existing connection (ie a multi-chunk message)
          NT.Received connid chunks ->
              case Map.lookup connid (dispatcherConnections state) of
                  Nothing ->
                      throw (InternalError "received data on unknown connection")

                  -- TODO: need policy here on queue size
                  Just (peer, ConnectionNew) ->
                      loop $ state {
                            dispatcherConnections = Map.insert connid (peer, ConnectionNewChunks chunks) (dispatcherConnections state)
                          }

                  Just (peer, ConnectionNewChunks chunks0) -> do
                      mConnState <- handleFirstChunks connid peer (chunks0 ++ chunks) state
                      case mConnState of
                          Nothing -> loop $ state
                          Just cs -> loop $ state {
                                dispatcherConnections = Map.insert connid (peer, cs) (dispatcherConnections state)
                              }

                  -- Connection is receiving data and there's some handler
                  -- at 'tid' to run it. Dump the new data to its ChannelIn.
                  Just (_peer, ConnectionReceiving _ (ChannelIn chan)) -> do
                    Channel.writeChannel chan (Just (BS.concat chunks))
                    loop state

                  Just (_peer, ConnectionClosed _) ->
                    throw (InternalError "received data on closed connection")

                  -- The peer keeps pushing data but our handler is finished.
                  -- What to do? Would like to close the connection but I'm
                  -- not sure that's possible in network-transport. Would like
                  -- to say "stop receiving on this ConnectionId".
                  -- We could maintain an association between ConnectionId and
                  -- EndPointId of the peer, and then maybe patch
                  -- network-transport to allow for selective closing of peer
                  -- connection based on EndPointAddress.
                  Just (_peer, ConnectionHandlerFinished _) ->
                    throw (InternalError "received too much data")

          NT.ConnectionClosed connid ->
              case Map.lookup connid (dispatcherConnections state) of
                  Nothing ->
                      throw (InternalError "closed unknown connection")

                  -- Connection closed, handler already finished. We're done
                  -- with the connection. The case in which the handler finishes
                  -- __after__ the connection closes is taken care of in
                  -- 'updateStateForFinishedHandlers'.
                  Just (peer, ConnectionHandlerFinished _) ->
                      loop $ state {
                            dispatcherConnections = Map.delete connid (dispatcherConnections state)
                          , dispatcherPeers = Map.update (NESet.delete connid) peer (dispatcherPeers state)
                          }

                  -- Empty message
                  Just (peer, ConnectionNew) -> do
                      chan <- Channel.newChannel
                      Channel.writeChannel chan Nothing
                      promise <- asyncHandler $ finishHandler nodeState (Left connid) (handlerIn (NodeId peer) (ChannelIn chan))
                      loop $ state {
                            dispatcherConnections = Map.insert connid (peer, ConnectionClosed promise) (dispatcherConnections state)
                          }

                  -- Small message
                  Just (peer, ConnectionNewChunks chunks0) -> do
                      mConnState <- handleFirstChunks connid peer chunks0 state
                      case mConnState of
                          Just (ConnectionReceiving promise (ChannelIn chan)) -> do
                              -- Write Nothing to indicate end of input.
                              Channel.writeChannel chan Nothing
                              loop $ state {
                                    dispatcherConnections = Map.insert connid (peer, ConnectionClosed promise) (dispatcherConnections state)
                                  }
                          Just other -> loop $ state {
                                dispatcherConnections = Map.insert connid (peer, other) (dispatcherConnections state)
                              }
                          -- There's no handler for the chunks, possibly because
                          -- the small message was from a peer contacted in
                          -- conversation mode, but the local handler has
                          -- already finished. It's strange behavior but not
                          -- an error.
                          Nothing -> loop state

                  -- End of incoming data. Signal that by writing 'Nothing'
                  -- to the ChannelIn.
                  Just (peer, ConnectionReceiving promise (ChannelIn chan)) -> do
                      Channel.writeChannel chan Nothing
                      loop $ state {
                            dispatcherConnections = Map.insert connid (peer, ConnectionClosed promise) (dispatcherConnections state)
                          }

                  Just (_peer, ConnectionClosed _) ->
                      throw (InternalError "closed a closed connection")

          NT.EndPointClosed -> waitForRunningHandlers state

          -- Losing a connection is no reason to stop the dispatcher. It is in
          -- fact a normal thing, as it can be caused by peers failing or
          -- otherwise choosing to go down.
          --
          -- When this happens, all handlers between the local EndPoint and the
          -- given EndPointAddress must be made to realize that receiving from
          -- and sending to their peer is no longer possible. This is done by
          -- making it so that their ChannelIn and ChannelOut throws an
          -- exception when used. Thus if a peer goes down after a local handler
          -- has finished all of its sending and receiving, the local handler
          -- need not die early.
          --
          -- Sending on a ChannelOut will report an error, so no work must be
          -- done here in order to support that. But the ChannelIn for each
          -- handler must be plugged (with Nothing or perhaps with a new
          -- constructor indicating exceptional end of input). In order to
          -- do so, we must associate an EndPointAddress with all ConnectionIds
          -- of connections to/from it.
          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode (NT.EventConnectionLost peer)) _msg) -> do
              let connids :: [NT.ConnectionId]
                  connids = case Map.lookup peer (dispatcherPeers state) of
                      Nothing    -> []
                      Just neset -> let t :| ts = NESet.toList neset in t : ts
              (!state', removals) <- foldlM eliminateConnection (state, []) connids
              loop $ state' {
                    dispatcherPeers = Map.update (NESet.deleteMany removals) peer (dispatcherPeers state')
                  }
              where
              -- Every receiving connection becomes closed.
              -- Every closed connection remains closed.
              -- Every other connection can be forgotten.
              eliminateConnection
                  :: (DispatcherState m, [NT.ConnectionId])
                  -> NT.ConnectionId
                  -> m (DispatcherState m, [NT.ConnectionId])
              eliminateConnection (_state, removals) connid = case Map.lookup connid (dispatcherConnections state) of
                  Nothing -> throw (InternalError "connection associated with peer does not exist")
                  Just (_peer, ConnectionReceiving handler (ChannelIn chan)) -> do
                      -- Signal to the handler that there's no more input.
                      Channel.writeChannel chan Nothing
                      let state' = state {
                                dispatcherConnections = Map.insert connid (peer, ConnectionClosed handler) (dispatcherConnections state)
                              }
                      pure (state', removals)
                  Just (_, ConnectionClosed _) -> do
                      -- We've already written Nothing to the channel
                      pure (state, removals)
                  -- If the connection is in any other state, we can remove it.
                  _ -> let state' = state { dispatcherConnections = Map.delete connid (dispatcherConnections state) }
                       in  pure (state', connid : removals)

          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode NT.EventEndPointFailed) _) ->
              throw (InternalError "EndPoint failed")

          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode NT.EventTransportFailed) _) ->
              throw (InternalError "Transport failed")

          NT.ErrorEvent (NT.TransportError NT.UnsupportedEvent _) ->
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
    signalFinished outcome = modifySharedAtomic stateVar $ \(NodeState prng nonces finished closed) ->
            pure ((NodeState prng nonces (outcome : finished) closed), ())

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
    :: ( Mockable Channel.Channel m, Mockable SharedAtomic m
       , Mockable Throw m, WithLogger m )
    => Node m
    -> NodeId
    -> Promise m t
       -- ^ Finishes when the in/out channel is no longer used. See
       -- 'withInOutChannel'
    -> m (Nonce, ChannelIn m, ChannelOut m)
connectInOutChannel Node{nodeEndPoint, nodeState} (NodeId endpointaddr) promise = do
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
        -- TODO: proper error handling
        outcome <- NT.send outconn [controlHeaderBidirectionalSyn nonce]
        case outcome of
            Left  err -> logDebug $ sformat
                ("Error initializing bidirectional connection to "%shown%": "%shown)
                endpointaddr err
            Right _   -> return ()
        return (nonce, ChannelIn inchan, ChannelOut outconn)
    where
    allocateInChannel = do
      chan  <- Channel.newChannel
      -- Create a nonce and update the shared atomic so that the nonce indicates
      -- that there's a handler for it.
      nonce <- modifySharedAtomic nodeState $ \(NodeState prng expected finished closed) -> do
                 let (nonce, !prng') = random prng
                     !expected' = Map.insert nonce (NonceHandlerNotConnected (SomeHandler promise) (ChannelIn chan)) expected
                 pure ((NodeState prng' expected' finished closed), nonce)
      return (nonce, chan)

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
    case mconn of
      Left  err  -> throw err
      Right conn -> do
        outcome <- NT.send conn [controlHeaderUnidirectional]
        case outcome of
            Left  err -> logDebug $ sformat
                ("Error initializing unidirectional connection to "%shown%": "%shown)
                endpointaddr err
            Right _   -> return ()
        return (ChannelOut conn)

closeChannel :: ChannelOut m -> m ()
closeChannel (ChannelOut conn) = NT.close conn

-- | Create, use, and tear down a conversation channel with a given peer
--   (NodeId).
withInOutChannel
    :: forall m a .
       ( Mockable Bracket m, Mockable Async m, Mockable Channel.Channel m
       , Mockable SharedAtomic m, Mockable Throw m, Mockable Catch m
       , MonadFix m, WithLogger m )
    => Node m
    -> NodeId
    -> (ChannelIn m -> ChannelOut m -> m a)
    -> m a
withInOutChannel node@Node{nodeState} nodeid action = do
    -- connectInOurChannel will update the nonce state to indicate that there's
    -- a handler for it. When the handler is finished (whether normally or
    -- exceptionally) we have to update it to say so.
    rec { promise <- async $
              bracket (connectInOutChannel node nodeid promise)
                  (\(_, _, outchan) -> closeChannel outchan)
                  (\(nonce, inchan, outchan) -> action' nonce inchan outchan)
        }
    wait promise
    where
    -- Updates the nonce state map always, and re-throws any caught exception.
    action' nonce inchan outchan = finishHandler nodeState (Right nonce) (action inchan outchan)

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

-- | Write some ByteStrings to an out channel. It does not close the
--   transport when finished. If you want that, use withOutChannel or
--   withInOutChannel.
writeChannel
    :: ( Monad m, WithLogger m, Mockable Throw m )
    => ChannelOut m
    -> [BS.ByteString]
    -> m ()
writeChannel (ChannelOut _) [] = pure ()
writeChannel (ChannelOut conn) (chunk:chunks) = do
    res <- NT.send conn [chunk]
    -- Any error detected here will be reported to the dispatcher thread
    -- so we don't need to do anything
    case res of
      Left err -> throw err
      Right _  -> writeChannel (ChannelOut conn) chunks

-- | Read a 'ChannelIn', blocking until the next 'ByteString' arrives, or end
--   of input is signalled via 'Nothing'.
readChannel :: ( Mockable Channel.Channel m ) => ChannelIn m -> m (Maybe BS.ByteString)
readChannel (ChannelIn chan) = Channel.readChannel chan
