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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

module Node.Internal (
    NodeId(..),
    Node(..),
    nodeId,
    nodeEndPointAddress,
    Statistics(..),
    PeerStatistics(..),
    nodeStatistics,
    ChannelIn(..),
    ChannelOut(..),
    startNode,
    stopNode,
    withOutChannel,
    withInOutChannel,
    writeChannel
  ) where

import           Control.Exception             hiding (bracket, catch, finally, throw)
import           Control.Monad                 (forM_, when)
import           Control.Monad.Fix             (MonadFix)
import           Data.Int                      (Int64)
import           Data.Binary                   as Bin
import           Data.Binary.Get               as Bin
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Foldable                 (foldlM, foldl')
import           Data.Hashable                 (Hashable)
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.NonEmptySet              (NonEmptySet)
import qualified Data.NonEmptySet              as NESet
import           Data.Monoid
import           Data.Typeable
import           Data.Time.Units               (Microsecond)
import           Formatting                    (sformat, shown, (%))
import qualified Mockable.Channel              as Channel
import           Mockable.Class
import           Mockable.Concurrent
import           Mockable.Exception
import           Mockable.SharedAtomic
import           Mockable.SharedExclusive
import           Mockable.CurrentTime          (CurrentTime, currentTime)
import qualified Mockable.Metrics              as Metrics
import qualified Network.Transport             as NT (EventErrorCode (EventConnectionLost, EventEndPointFailed, EventTransportFailed))
import qualified Network.Transport.Abstract    as NT
import           System.Random                 (Random, StdGen, random)
import           System.Wlog                   (WithLogger, logDebug, logError, logWarning)
import qualified Node.Message                  as Message

import Debug.Trace

-- | A 'NodeId' wraps a network-transport endpoint address
newtype NodeId = NodeId NT.EndPointAddress
  deriving (Eq, Ord, Show, Hashable)

-- | The state of a Node, to be held in a shared atomic cell because other
--   threads will mutate it in order to set up bidirectional connections.
data NodeState peerData m = NodeState {
      _nodeStateGen                    :: !StdGen
      -- ^ To generate nonces.
    , _nodeStateOutboundUnidirectional :: !(Set (Promise m ()))
      -- ^ Handlers for each locally-initiated unidirectional connection.
    , _nodeStateOutboundBidirectional  :: !(Map Nonce (Promise m (), ChannelIn m, SharedExclusiveT m peerData, Bool))
      -- ^ Handlers for each nonce which we generated (locally-initiated
      --   bidirectional connections).
      --   The bool indicates whether we have received an ACK for this.
    , _nodeStateInbound                :: !(Set (Promise m ()))
      -- ^ Handlers for inbound connections (remotely-initiated unidirectional
      --   _or_ bidirectional connections).
    , _nodeStateConnectedTo        :: !(Map NT.EndPointAddress (Either (SharedExclusiveT m (Maybe SomeException)) Int))
      -- ^ For each peer that we have at least one open connection to, the
      --   number of connections; or an MVar in case there's some thread
      --   sending the initial data (it just opened the first connection to that
      --   peer).
    , _nodeStateStatistics         :: !(Statistics m)
      -- ^ Statistics about traffic at this node.
      --   Must be kept in mutable state so that handlers can update it when
      --   they finish.
    , _nodeStateClosed             :: !Bool
      -- ^ Indicates whether the Node has been closed and is no longer capable
      --   of establishing or accepting connections (its EndPoint is closed).
    }

-- | The initial state of a node, wrapped up in a shared atomic.
initialNodeState
    :: ( Mockable Metrics.Metrics m, Mockable SharedAtomic m )
    => StdGen
    -> m (SharedAtomicT m (NodeState peerData m))
initialNodeState prng = do
    !stats <- initialStatistics
    let nodeState = NodeState {
              _nodeStateGen = prng
            , _nodeStateOutboundUnidirectional = Set.empty
            , _nodeStateOutboundBidirectional = Map.empty
            , _nodeStateInbound = Set.empty
            , _nodeStateConnectedTo = Map.empty
            , _nodeStateStatistics = stats
            , _nodeStateClosed = False
            }
    newSharedAtomic nodeState

-- | A 'Node' is a network-transport 'EndPoint' with bidirectional connection
--   state and a thread to dispatch network-transport events.
data Node packingType peerData (m :: * -> *) = Node {
       nodeEndPoint         :: NT.EndPoint m
     , nodeDispatcherThread :: Promise m ()
     , nodeState            :: SharedAtomicT m (NodeState peerData m)
     , nodePackingType      :: packingType
     , nodePeerData         :: peerData
     }

nodeId :: Node packingType peerData m -> NodeId
nodeId = NodeId . NT.address . nodeEndPoint

nodeEndPointAddress :: NodeId -> NT.EndPointAddress
nodeEndPointAddress (NodeId addr) = addr

nodeStatistics :: ( Mockable SharedAtomic m ) => Node packingType peerData m -> m (Statistics m)
nodeStatistics Node{..} = modifySharedAtomic nodeState $ \st ->
    return (st, _nodeStateStatistics st)

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

-- | Input from the wire.
newtype ChannelIn m = ChannelIn (Channel.ChannelT m (Maybe BS.ByteString))

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

-- | Statistics concerning traffic at this node.
data Statistics m = Statistics {
      -- | How many handlers are running right now in response to a
      --   remotely initiated connection (whether unidirectional or
      --   bidirectional).
      --   NB a handler may run longer or shorter than the duration of a
      --   connection.
      stRunningHandlersRemote :: !(Metrics.Gauge m)
      -- | How many handlers are running right now which were initiated
      --   locally, i.e. corresponding to bidirectional connections.
    , stRunningHandlersLocal :: !(Metrics.Gauge m)
      -- | Statistics for each peer.
    , stPeerStatistics :: !(Map NT.EndPointAddress PeerStatistics)
      -- | How many peers are connected.
    , stPeers :: !(Metrics.Gauge m)
      -- | Average number of remotely-initiated handlers per peer.
      --   Also track the average of the number of handlers squared, so we
      --   can quickly compute the variance.
    , stRunningHandlersRemoteAverage :: !(Double, Double)
      -- | Average number of locally-initiated handlers per peer.
      --   Also track the average of the number of handlers squared, so we
      --   can quickly compute the variance.
    , stRunningHandlersLocalAverage :: !(Double, Double)
      -- | Handlers which finished normally. Distribution is on their
      --   running time.
    , stHandlersFinishedNormally :: !(Metrics.Distribution m)
      -- | Handlers which finished exceptionally. Distribution is on their
      --   running time.
    , stHandlersFinishedExceptionally :: !(Metrics.Distribution m)
    }

stRunningHandlersRemoteVariance :: Statistics m -> Double
stRunningHandlersRemoteVariance statistics = avg2 - (avg ^ 2)
    where
    (avg, avg2) = stRunningHandlersRemoteAverage statistics

stRunningHandlersLocalVariance :: Statistics m -> Double
stRunningHandlersLocalVariance statistics = avg2 - (avg ^ 2)
    where
    (avg, avg2) = stRunningHandlersLocalAverage statistics

-- | Statistics about a given peer.
data PeerStatistics = PeerStatistics {
      -- | How many handlers are running right now in response to connections
      --   from this peer (whether unidirectional or remotely-initiated
      --   bidirectional).
      pstRunningHandlersRemote :: !Int
      -- | How many handlers are running right now for locally-iniaiated
      --   bidirectional connections to this peer.
    , pstRunningHandlersLocal :: !Int
    }

pstNull :: PeerStatistics -> Bool
pstNull PeerStatistics{..} =
    let rem = pstRunningHandlersRemote
        loc = pstRunningHandlersLocal
    in  rem == 0 && loc == 0

-- | Record a new handler for a given peer. Second component is True if it's the
--   only handler for that peer.
pstAddHandler
    :: HandlerProvenance peerData m t
    -> Map NT.EndPointAddress PeerStatistics
    -> (Map NT.EndPointAddress PeerStatistics, Bool)
pstAddHandler provenance map = case provenance of

    Local peer _ -> case Map.lookup peer map of
        Nothing -> (Map.insert peer (PeerStatistics 0 1) map, True)
        Just !stats -> (Map.insert peer stats' map, False)
            where
            !stats' = stats { pstRunningHandlersLocal = pstRunningHandlersLocal stats + 1 }

    Remote peer _ _ -> case Map.lookup peer map of
        Nothing -> (Map.insert peer (PeerStatistics 1 0) map, True)
        Just !stats -> (Map.insert peer stats' map, False)
            where
            !stats' = stats { pstRunningHandlersRemote = pstRunningHandlersRemote stats + 1 }

-- | Remove a handler for a given peer. Second component is True if there
--   are no more handlers for that peer.
pstRemoveHandler
    :: HandlerProvenance peerData m t
    -> Map NT.EndPointAddress PeerStatistics
    -> (Map NT.EndPointAddress PeerStatistics, Bool)
pstRemoveHandler provenance map = case provenance of

    Local peer _ -> case Map.updateLookupWithKey updater peer map of
        (Just !stats, map') -> (map', pstNull stats)
        -- First component is Nothing only if the peer is not in the map.
        -- That should never happen.
        _ -> (map, False)
        where
        updater _ !stats =
            let !stats' = stats { pstRunningHandlersLocal = pstRunningHandlersLocal stats - 1 }
            in  if pstNull stats' then Nothing else Just stats'

    Remote peer _ _ -> case Map.updateLookupWithKey updater peer map of
        (Just !stats, map') -> (map', pstNull stats)
        _ -> (map, False)
        where
        updater _ !stats =
            let !stats' = stats { pstRunningHandlersRemote = pstRunningHandlersRemote stats - 1 }
            in  if pstNull stats' then Nothing else Just stats'

-- | Statistics when a node is launched.
initialStatistics :: ( Mockable Metrics.Metrics m ) => m (Statistics m)
initialStatistics = do
    !runningHandlersRemote <- Metrics.newGauge
    !runningHandlersLocal <- Metrics.newGauge
    !peers <- Metrics.newGauge
    !handlersFinishedNormally <- Metrics.newDistribution
    !handlersFinishedExceptionally <- Metrics.newDistribution
    return $ Statistics {
          stRunningHandlersRemote = runningHandlersRemote
        , stRunningHandlersLocal = runningHandlersLocal
        , stPeerStatistics = Map.empty
        , stPeers = peers
        , stRunningHandlersRemoteAverage = (0, 0)
        , stRunningHandlersLocalAverage = (0, 0)
        , stHandlersFinishedNormally = handlersFinishedNormally
        , stHandlersFinishedExceptionally = handlersFinishedExceptionally
        }

data HandlerProvenance peerData m t =
      -- | Initiated locally, _to_ this peer. The Nonce is present if and only
      --   if it's a bidirectional connection.
      Local !NT.EndPointAddress (Maybe (Nonce, SharedExclusiveT m peerData, t))
      -- | Initiated remotely, _by_ or _from_ this peer.
    | Remote !NT.EndPointAddress !NT.ConnectionId t

-- TODO: revise these computations to make them numerically stable (or maybe
-- use Rational?).
stAddHandler
    :: ( Mockable Metrics.Metrics m )
    => HandlerProvenance peerData m t
    -> Statistics m
    -> m (Statistics m)
stAddHandler !provenance !statistics = case provenance of

    -- TODO: generalize this computation so we can use the same thing for
    -- both local and remote. It's a copy/paste job right now swapping local
    -- for remote.
    Local !peer _ -> do
        when isNewPeer $ Metrics.incGauge (stPeers statistics)
        Metrics.incGauge (stRunningHandlersLocal statistics)
        !npeers <- Metrics.readGauge (stPeers statistics)
        !nhandlers <- Metrics.readGauge (stRunningHandlersLocal statistics)
        let runningHandlersLocalAverage =
                adjustMeans isNewPeer
                            (fromIntegral npeers)
                            nhandlers
                            (stRunningHandlersLocalAverage statistics)
        return $ statistics {
              stPeerStatistics = peerStatistics
            , stRunningHandlersLocalAverage = runningHandlersLocalAverage
            }

    Remote !peer _ _ -> do
        when isNewPeer $ Metrics.incGauge (stPeers statistics)
        Metrics.incGauge (stRunningHandlersRemote statistics)
        !npeers <- Metrics.readGauge (stPeers statistics)
        !nhandlers <- Metrics.readGauge (stRunningHandlersRemote statistics)
        let runningHandlersRemoteAverage =
                adjustMeans isNewPeer
                            (fromIntegral npeers)
                            nhandlers
                            (stRunningHandlersRemoteAverage statistics)
        return $ statistics {
              stPeerStatistics = peerStatistics
            , stRunningHandlersRemoteAverage = runningHandlersRemoteAverage
            }

    where

    (!peerStatistics, !isNewPeer) = pstAddHandler provenance (stPeerStatistics statistics)

    -- Adjust the means. The Bool is true if it's a new peer.
    -- The Double is the current number of peers (always > 0).
    -- The Int is the current number of running handlers.
    adjustMeans :: Bool -> Double -> Int64 -> (Double, Double) -> (Double, Double)
    adjustMeans !isNewPeer !npeers !nhandlers (!avg, !avg2) = case isNewPeer of

        True -> (avg', avg2')
            where
            avg' = avg * ((npeers - 1) / npeers) + (1 / npeers)
            avg2' = avg2 * ((npeers - 1) / npeers) + (1 / npeers)

        False -> (avg', avg2')
            where
            avg' = avg + (1 / npeers)
            avg2' = avg + (fromIntegral (2 * nhandlers + 1) / npeers) 

-- TODO: revise these computations to make them numerically stable (or maybe
-- use Rational?).
stRemoveHandler
    :: ( Mockable Metrics.Metrics m )
    => HandlerProvenance peerData m t
    -> Microsecond
    -> Maybe SomeException
    -> Statistics m
    -> m (Statistics m)
stRemoveHandler !provenance !elapsed !outcome !statistics = case provenance of

    -- TODO: generalize this computation so we can use the same thing for
    -- both local and remote. It's a copy/paste job right now swapping local
    -- for remote.
    Local !peer _ -> do
        when isEndedPeer $ Metrics.decGauge (stPeers statistics)
        Metrics.decGauge (stRunningHandlersLocal statistics)
        !npeers <- Metrics.readGauge (stPeers statistics)
        !nhandlers <- Metrics.readGauge (stRunningHandlersLocal statistics)
        let runningHandlersLocalAverage =
                adjustMeans isEndedPeer
                            npeers
                            nhandlers
                            (stRunningHandlersLocalAverage statistics)
        addSample
        return $ statistics {
              stPeerStatistics = peerStatistics
            , stRunningHandlersLocalAverage = runningHandlersLocalAverage
            }

    Remote !peer _ _ -> do
        when isEndedPeer $ Metrics.decGauge (stPeers statistics)
        Metrics.decGauge (stRunningHandlersRemote statistics)
        !npeers <- Metrics.readGauge (stPeers statistics)
        !nhandlers <- Metrics.readGauge (stRunningHandlersRemote statistics)
        let runningHandlersRemoteAverage =
                adjustMeans isEndedPeer
                            npeers
                            nhandlers
                            (stRunningHandlersRemoteAverage statistics)
        addSample
        return $ statistics {
              stPeerStatistics = peerStatistics
            , stRunningHandlersRemoteAverage = runningHandlersRemoteAverage
            }

    where

    (!peerStatistics, !isEndedPeer) = pstRemoveHandler provenance (stPeerStatistics statistics)

    -- Convert the elapsed time to a Double and then add it to the relevant
    -- distribution.
    addSample = case outcome of
        Nothing -> Metrics.addSample (stHandlersFinishedNormally statistics) (fromIntegral (toInteger elapsed))
        Just _ -> Metrics.addSample (stHandlersFinishedExceptionally statistics) (fromIntegral (toInteger elapsed))

    -- Adjust the means. The Bool is true if it's a stale peer (removed last
    --   handler).
    -- The first Int is the current number of peers (could be 0).
    -- The Int is the current number of running handlers.
    adjustMeans :: Bool -> Int64 -> Int64 -> (Double, Double) -> (Double, Double)
    adjustMeans !isEndedPeer !npeers !nhandlers (!avg, !avg2) = case isEndedPeer of

        True -> if npeers == 0
                then (0, 0)
                else (avg', avg2')
            where
            avg' = avg * (fromIntegral (npeers - 1) / fromIntegral npeers) + (1 / fromIntegral npeers)
            avg2' = avg2 * (fromIntegral (npeers - 1) / fromIntegral npeers) + (1 / fromIntegral npeers)

        False -> (avg', avg2')
            where
            avg' = avg - (1 / fromIntegral npeers)
            avg2' = avg - (fromIntegral (2 * nhandlers + 1) / fromIntegral npeers) 

-- | Bring up a 'Node' using a network transport.
startNode
    :: ( Mockable SharedAtomic m, Mockable Channel.Channel m
       , Mockable Bracket m, Mockable Throw m, Mockable Catch m
       , Mockable Async m, Mockable Concurrently m, Ord (Promise m ())
       , Mockable CurrentTime m, Mockable Metrics.Metrics m
       , Mockable SharedExclusive m
       , Message.Serializable packingType peerData
       , MonadFix m, WithLogger m )
    => packingType
    -> peerData
    -> NT.Transport m
    -- ^ Given a QDisc constructor, make a transport. startNode will
    --   create the transport and one end point. stopNode will close it
    --   up.
    -> StdGen
    -- ^ A source of randomness, for generating nonces.
    -> (peerData -> NodeId -> ChannelIn m -> m ())
    -- ^ Handle incoming unidirectional connections.
    -> (peerData -> NodeId -> ChannelIn m -> ChannelOut m -> m ())
    -- ^ Handle incoming bidirectional connections.
    -> m (Node packingType peerData m)
startNode packingType peerData transport prng handlerIn handlerOut = do
    mEndPoint <- NT.newEndPoint transport
    case mEndPoint of
        Left err -> throw err
        Right endPoint -> do
            sharedState <- initialNodeState prng
            -- TODO this thread should get exceptions from the dispatcher thread.
            rec { let node = Node {
                            nodeEndPoint         = endPoint
                          , nodeDispatcherThread = dispatcherThread
                          , nodeState            = sharedState
                          , nodePackingType      = packingType
                          , nodePeerData         = peerData
                          }
                ; dispatcherThread <- async $
                      nodeDispatcher node sharedState handlerIn handlerOut
                }
            return node

-- | Stop a 'Node', closing its network transport and end point.
stopNode
    :: ( WithLogger m, Mockable Throw m, Mockable Async m, Mockable SharedAtomic m )
    => Node packingType peerData m
    -> m ()
stopNode Node {..} = do
    modifySharedAtomic nodeState $ \nodeState ->
        if _nodeStateClosed nodeState
        then throw $ userError "stopNode : already stopped"
        else pure (nodeState { _nodeStateClosed = True }, ())
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

data ConnectionState peerData m =

      -- | This connection cannot proceed because peer data has not been
      --   received and parsed.
      WaitingForPeerData

      -- | This connection attempted to parse the peer data but failed.
      --   Any subsequent data will be ignored.
    | PeerDataParseFailure

      -- | This connection is waiting for a handshake and we have partial
      --   data. The peer state of the connection must be 'GotPeerData'.
    | WaitingForHandshake !peerData !BS.ByteString

      -- | This connection attempted handshake but it failed (protocol error).
      --   Any subsequent data will be ignored.
    | HandshakeFailure

      -- | This connection has made a handshake and is now feeding an
      --   application-specific handler through a channel. The peer state
      --   of this connection must be 'GotPeerData'.
    | FeedingApplicationHandler !(ChannelIn m)

instance Show (ConnectionState peerData m) where
    show term = case term of
        WaitingForPeerData -> "WaitingForPeerData"
        PeerDataParseFailure -> "PeerDataParseFailure"
        WaitingForHandshake _ _ -> "WaitingForHandshake"
        HandshakeFailure -> "HandshakeFailure"
        FeedingApplicationHandler _ -> "FeedingApplicationHandler"

data PeerState peerData =

      -- | Peer data is expected from one of these lightweight connections.
      --   If the second component is 'Just', then there's a lightweight
      --   connection which has given a partial parse of the peer data.
      ExpectingPeerData
          !(NonEmptySet NT.ConnectionId)
          !(Maybe (NT.ConnectionId, Maybe BS.ByteString -> Bin.Decoder peerData))

      -- | Peer data has been received and parsed.
    | GotPeerData !peerData !(NonEmptySet NT.ConnectionId)

instance Show (PeerState peerData) where
    show term = case term of
        ExpectingPeerData peers mleader -> "ExpectingPeerData " ++ show peers ++ " " ++ show (fmap fst mleader)
        GotPeerData _ peers -> "GotPeerData " ++ show peers

data DispatcherState peerData m = DispatcherState {
      csConnections :: Map NT.ConnectionId (NT.EndPointAddress, ConnectionState peerData m)
    , csPeers :: Map NT.EndPointAddress (PeerState peerData)
    }

deriving instance Show (DispatcherState peerData m)

initialDispatcherState :: DispatcherState peerData m
initialDispatcherState = DispatcherState Map.empty Map.empty

-- | The one thread that handles /all/ incoming messages and dispatches them
-- to various handlers.
nodeDispatcher
    :: forall m packingType peerData .
       ( Mockable SharedAtomic m, Mockable Async m, Mockable Concurrently m
       , Ord (Promise m ()), Mockable Bracket m, Mockable SharedExclusive m
       , Mockable Channel.Channel m, Mockable Throw m, Mockable Catch m
       , Mockable CurrentTime m, Mockable Metrics.Metrics m
       , Message.Serializable packingType peerData
       , MonadFix m, WithLogger m )
    => Node packingType peerData m
    -> SharedAtomicT m (NodeState peerData m)
    -- ^ Nonce states and a StdGen to generate nonces. It's in a
    --   shared atomic because other threads must be able to alter
    --   it when they start a conversation.
    --   The third element of the triple will be updated by handler
    --   threads when they finish.
    -> (peerData -> NodeId -> ChannelIn m -> m ())
    -> (peerData -> NodeId -> ChannelIn m -> ChannelOut m -> m ())
    -> m ()
nodeDispatcher node nodeState handlerIn handlerInOut =
    loop initialDispatcherState

    where

    endpoint = nodeEndPoint node

    loop :: DispatcherState peerData m -> m ()
    loop !state = do
      event <- NT.receive endpoint
      -- () <- traceM $ show (NT.address endpoint) ++ " : " ++ show event ++ " " ++ show state
      case event of

          NT.ConnectionOpened connid reliability peer ->
              connectionOpened state connid reliability peer >>= loop

          NT.Received connid bytes -> received state connid bytes >>= loop

          NT.ConnectionClosed connid -> connectionClosed state connid >>= loop

          -- When the end point closes, we're done.
          NT.EndPointClosed -> return ()

          -- When a heavyweight connection is lost we must close up all of the
          -- lightweight connections which it carried.
          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode (NT.EventConnectionLost peer)) msg) ->
              connectionLost state peer >>= loop

          -- Unsupported event is recoverable. Just log and carry on.
          NT.ErrorEvent err@(NT.TransportError NT.UnsupportedEvent _) -> do
              logWarning $ sformat shown err
              loop state

          -- End point failure is unrecoverable.
          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode NT.EventEndPointFailed) _) ->
              throw (InternalError "EndPoint failed")

          -- Transport failure is unrecoverable.
          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode NT.EventTransportFailed) _) ->
              throw (InternalError "Transport failed")

    connectionOpened
        :: DispatcherState peerData m
        -> NT.ConnectionId
        -> NT.Reliability
        -> NT.EndPointAddress
        -> m (DispatcherState peerData m)
    connectionOpened state connid reliability peer = case Map.lookup connid (csConnections state) of

        Just (peer', _) -> do
            logWarning $ sformat ("ignoring duplicate connection " % shown % shown % shown) peer peer' connid
            return state

        Nothing -> do

            -- How we handle this connection depends on whether we already have
            -- a connection from this peer.
            case Map.lookup peer (csPeers state) of

                -- If we do, we can start waiting for the handshake.
                Just (GotPeerData peerData neset) -> do
                    return $ state {
                          csConnections = Map.insert connid (peer, WaitingForHandshake peerData BS.empty) (csConnections state)
                        , csPeers = Map.insert peer (GotPeerData peerData (NESet.insert connid neset)) (csPeers state)
                        }

                -- If we don't, then we must await and decode the peer data.
                Nothing -> do
                    return $ state {
                          csConnections = Map.insert connid (peer, WaitingForPeerData) (csConnections state)
                        , csPeers = Map.insert peer (ExpectingPeerData (NESet.singleton connid) Nothing) (csPeers state)
                        }

                -- We got another connection before the peer data arrived.
                -- That's actually OK. It's only an error if we receive data
                -- on this connection before the first connection receives
                -- and parses the peer data ('received' handles this aspect).
                -- So here we just record the connection.
                Just (ExpectingPeerData neset mleader) -> do
                    return $ state {
                          csConnections = Map.insert connid (peer, WaitingForPeerData) (csConnections state)
                        , csPeers = Map.insert peer (ExpectingPeerData (NESet.insert connid neset) mleader) (csPeers state)
                        }

    received
        :: DispatcherState peerData m
        -> NT.ConnectionId
        -> [BS.ByteString]
        -> m (DispatcherState peerData m)
    received state connid chunks = case Map.lookup connid (csConnections state) of

        Nothing -> do
            logWarning $ sformat ("ignoring data on unknown connection " % shown) connid
            return state

        -- This connection gave bogus peer data. Ignore the data.
        Just (peer, PeerDataParseFailure) -> do
            logWarning $ sformat ("ignoring data on failed connection (peer data) from " % shown) peer
            return state

        -- This connection gave a bad handshake. Ignore the data.
        Just (peer, HandshakeFailure) -> do
            logWarning $ sformat ("ignoring data on failed connection (handshake) from " % shown) peer
            return state

        -- This connection is awaiting the initial peer data.
        Just (peer, WaitingForPeerData) -> case Map.lookup peer (csPeers state) of

            Just (ExpectingPeerData connids mleader) -> case mleader of

                -- There's no leader. This connection is now the leader. Begin
                -- the attempt to decode the peer data.
                Nothing -> do
                    let decoder :: Bin.Decoder peerData
                        decoder = Message.unpackMsg (nodePackingType node)
                    case Bin.pushChunk decoder (BS.concat chunks) of
                        Bin.Fail _ _ err -> do
                            logWarning $ sformat ("failed to decode peer data from " % shown) peer
                            return $ state {
                                    csConnections = Map.insert connid (peer, PeerDataParseFailure) (csConnections state)
                                  }
                        Bin.Done trailing _ peerData -> do
                            return $ state {
                                    csConnections = foldl' (awaitHandshake connid peerData trailing) (csConnections state) (NESet.toList connids)
                                  , csPeers = Map.insert peer (GotPeerData peerData connids) (csPeers state)
                                  }
                        Bin.Partial decoderContinuation -> do
                            return $ state {
                                    csPeers = Map.insert peer (ExpectingPeerData connids (Just (connid, decoderContinuation))) (csPeers state)
                                  }

                Just (connid', decoderContinuation) -> case connid == connid' of

                    -- Protocol error. We got data from some other lightweight
                    -- connection before the peer data was parsed.
                    False -> do
                        logWarning $ sformat ("peer-data protocol error")
                        return state

                    True -> case decoderContinuation (Just (BS.concat chunks)) of

                        Bin.Fail _ _ err -> do
                            logWarning $ sformat ("failed to decode peer data from " % shown) peer
                            return $ state {
                                    csConnections = Map.insert connid (peer, PeerDataParseFailure) (csConnections state)
                                  }

                        Bin.Done trailing _ peerData -> do
                            return $ state {
                                    csConnections = foldl' (awaitHandshake connid peerData trailing) (csConnections state) (NESet.toList connids)
                                  , csPeers = Map.insert peer (GotPeerData peerData connids) (csPeers state)
                                  }

                        Bin.Partial decoderContinuation' -> do
                            return $ state {
                                    csPeers = Map.insert peer (ExpectingPeerData connids (Just (connid, decoderContinuation'))) (csPeers state)

                                }

                where

                -- Update a connection's state to WaitingForHandshake. For use
                -- in a fold once the peer data has been parsed. The first
                -- parameters give the id of the connection which made the
                -- parse and the data left-over after the parse, which must
                -- be remembered in the connection state for that id.
                awaitHandshake
                    :: NT.ConnectionId
                    -> peerData
                    -> BS.ByteString
                    -> Map NT.ConnectionId (NT.EndPointAddress, ConnectionState peerData m)
                    -> NT.ConnectionId
                    -> Map NT.ConnectionId (NT.EndPointAddress, ConnectionState peerData m)
                awaitHandshake leader peerData trailing map connid = case leader == connid of

                    True -> Map.update (\(peer, _) -> Just (peer, WaitingForHandshake peerData trailing)) connid map

                    False -> Map.update (\(peer, _) -> Just (peer, WaitingForHandshake peerData BS.empty)) connid map

            -- We're waiting for peer data on this connection, but we don't
            -- have an entry for the peer. That's an internal error.
            Nothing -> do
                logWarning $ sformat ("inconsistent dispatcher state")
                return state

        -- Waiting for a handshake. Try to get a control header and then
        -- move on.
        Just (peer, WaitingForHandshake peerData partial) -> do
            -- TODO the handshake.
            let bytes = BS.append partial (BS.concat chunks)
            case BS.uncons bytes of

                Nothing -> return state

                Just (w, ws)

                    -- Got unidirectional header. Create a channel and
                    -- spawn the application handler.
                    | w == controlHeaderCodeUnidirectional -> do
                          channel <- Channel.newChannel
                          Channel.writeChannel channel (Just ws)
                          let provenance = Remote peer connid (ChannelIn channel)
                          let handler = handlerIn peerData (NodeId peer) (ChannelIn channel)
                          _ <- spawnHandler nodeState provenance handler
                          return $ state {
                                csConnections = Map.insert connid (peer, FeedingApplicationHandler (ChannelIn channel)) (csConnections state)
                              }

                    -- Got a bidirectional header but still waiting for the
                    -- nonce.
                    | w == controlHeaderCodeBidirectionalSyn ||
                      w == controlHeaderCodeBidirectionalAck
                    , BS.length ws < 8 -> return state

                    -- Got a SYN. Spawn a thread to connect to the peer using
                    -- the nonce provided and then run the bidirectional handler.
                    | w == controlHeaderCodeBidirectionalSyn
                    , Right (ws', _, nonce) <- decodeOrFail (LBS.fromStrict ws) -> do
                          channel <- Channel.newChannel
                          Channel.writeChannel channel (Just (BS.concat (LBS.toChunks ws')))
                          let provenance = Remote peer connid (ChannelIn channel)
                          let handler = do
                                  conn <- connectToPeer node (NodeId peer)
                                  outcome <- NT.send conn [controlHeaderBidirectionalAck nonce]
                                  case outcome of
                                      Left err -> throw err
                                      Right () ->
                                          handlerInOut peerData (NodeId peer) (ChannelIn channel) (ChannelOut conn)
                                          `finally`
                                          disconnectFromPeer node (NodeId peer) conn
                          -- Establish the other direction in a separate thread.
                          _ <- spawnHandler nodeState provenance handler
                          return $ state {
                                csConnections = Map.insert connid (peer, FeedingApplicationHandler (ChannelIn channel)) (csConnections state)
                              }

                    -- Got an ACK. Try to decode the nonce and check that
                    -- we actually sent it.
                    | w == controlHeaderCodeBidirectionalAck
                    , Right (ws', _, nonce) <- decodeOrFail (LBS.fromStrict ws) -> do
                          outcome <- modifySharedAtomic nodeState $ \st ->
                              case Map.lookup nonce (_nodeStateOutboundBidirectional st) of
                                  Nothing -> return (st, Nothing)
                                  Just (_, _, _, True) -> return (st, Just Nothing)
                                  Just (promise, channel, peerDataVar, False) -> return
                                      ( st { _nodeStateOutboundBidirectional = Map.insert nonce (promise, channel, peerDataVar, True) (_nodeStateOutboundBidirectional st)
                                           }
                                      , Just (Just (channel, peerDataVar))
                                      )
                          case outcome of
                              -- We don't know about the nonce. Could be that
                              -- we never sent the SYN for it (protocol error)
                              -- or the handler for it has already finished.
                              -- In any case, say the handshake failed so that
                              -- subsequent data is ignored.
                              Nothing -> do
                                  return $ state {
                                        csConnections = Map.insert connid (peer, HandshakeFailure) (csConnections state)
                                      }

                              -- Got a duplicate ACK.
                              Just Nothing -> do
                                  logWarning $ sformat ("duplicate ACK nonce from " % shown) peer
                                  return $ state {
                                        csConnections = Map.insert connid (peer, HandshakeFailure) (csConnections state)
                                      }

                              -- Got an ACK for a SYN that we sent. Start
                              -- feeding the application handler.
                              Just (Just (ChannelIn channel, peerDataVar)) -> do
                                  putSharedExclusive peerDataVar peerData
                                  Channel.writeChannel channel (Just (LBS.toStrict ws'))
                                  return $ state {
                                        csConnections = Map.insert connid (peer, FeedingApplicationHandler (ChannelIn channel)) (csConnections state)
                                      }

                    -- Handshake failure. Subsequent receives will be ignored.
                    | otherwise -> do
                          logWarning $ sformat ("unexpected control header from " % shown) peer
                          return $ state {
                                csConnections = Map.insert connid (peer, HandshakeFailure) (csConnections state)
                              }

        -- This connection is feeding a handler. Make the data available.
        -- TODO: if the handler has already finished, we want to just forget
        -- the data. How? Weak reference to the channel perhaps? Or
        -- explcitly close it down when the handler finishes by adding some
        -- mutable cell to FeedingApplicationHandler?
        Just (_, FeedingApplicationHandler (ChannelIn channel)) -> do
            Channel.writeChannel channel (Just (BS.concat chunks))
            return state

    connectionClosed
        :: DispatcherState peerData m
        -> NT.ConnectionId
        -> m (DispatcherState peerData m)
    connectionClosed state connid = case Map.lookup connid (csConnections state) of

        Nothing -> do
            logWarning $ sformat ("closed unknown connection " % shown) connid
            return state

        Just (peer, connState) -> do
            case connState of
                FeedingApplicationHandler (ChannelIn channel) -> do
                    -- Signal end of channel.
                    Channel.writeChannel channel Nothing
                _ -> return ()
            -- This connection can be removed from the connection states map.
            -- Removing it from the peers map is more involved.
            let peersUpdater existing = case existing of
                    GotPeerData peerData neset -> case NESet.delete connid neset of
                        Nothing -> Nothing
                        Just neset' -> Just (GotPeerData peerData neset')
                    ExpectingPeerData neset mleader -> case NESet.delete connid neset of
                        Nothing -> Nothing
                        Just neset' -> case mleader of
                            Nothing -> Just (ExpectingPeerData neset' mleader)
                            Just (connid', partialDecoder) -> case connid == connid' of
                                -- The connection which is giving the peer data
                                -- has closed! That's ok, just forget about it
                                -- and the partial decode of that data.
                                True -> Just (ExpectingPeerData neset' Nothing)
                                False -> Just (ExpectingPeerData neset' mleader)
            let state' = state {
                      csConnections = Map.delete connid (csConnections state)
                    , csPeers = Map.update peersUpdater peer (csPeers state)
                    }
            return state'

    -- When a connection is lost, we purge all of the connection identifiers
    -- for that peer from the dispatcher state, and plug all of their input
    -- channels with 'Nothing'.
    connectionLost
        :: DispatcherState peerData m
        -> NT.EndPointAddress
        -> m (DispatcherState peerData m)
    connectionLost state peer = do
        logWarning $ sformat ("lost connection to " % shown) peer
        case Map.lookup peer (csPeers state) of

            Nothing -> do
                logWarning $ sformat ("lost connection to peer but had no connections " % shown) peer
                return state

            Just it -> do
                let connids = case it of
                        GotPeerData _ neset -> NESet.toList neset
                        ExpectingPeerData neset _ -> NESet.toList neset
                -- For every connection to that peer we'll plug the channel with
                -- Nothing and remove it from the map.
                let folder :: Map NT.ConnectionId (NT.EndPointAddress, ConnectionState peerData m)
                           -> NT.ConnectionId
                           -> m (Map NT.ConnectionId (NT.EndPointAddress, ConnectionState peerData m))
                    folder channels connid = case Map.updateLookupWithKey (\_ _ -> Nothing) connid channels of
                        (Just (_, FeedingApplicationHandler (ChannelIn channel)), channels') -> do
                            Channel.writeChannel channel Nothing
                            return channels'
                        (Nothing, channels') -> do
                            logWarning $ sformat "inconsistent peer and connection identifier state"
                            return channels'
                channels' <- foldlM folder (csConnections state) connids
                return $ state {
                      csConnections = channels'
                    , csPeers = Map.delete peer (csPeers state)
                    }

-- | Spawn a thread and track it in shared state, taking care to remove it from
--   shared state when it's finished and updating statistics appropriately.
--   This is applicable to handlers spawned in response to inbound peer
--   connections, and also for actions which use outbound connections.
spawnHandler
    :: forall peerData m t .
       ( Mockable SharedAtomic m, Mockable Throw m, Mockable Catch m
       , Mockable Async m, Ord (Promise m ())
       , Mockable Metrics.Metrics m, Mockable CurrentTime m
       , MonadFix m )
    => SharedAtomicT m (NodeState peerData m)
    -> HandlerProvenance peerData m (ChannelIn m)
    -> m t
    -> m (Promise m t)
spawnHandler stateVar provenance action =
    modifySharedAtomic stateVar $ \nodeState -> do
        -- Spawn the thread to get a 'SomeHandler'.
        rec { promise <- async $ do
                  startTime <- currentTime
                  normal waitForIt startTime `catch` exceptional waitForIt startTime
            -- A new promise which finishes when the other promise does, but returns
            -- ().
            ; waitForIt <- async $ do
                  wait promise
                  pure ()
            }
        -- It is assumed that different promises do not compare equal.
        -- It is assumed to be highly unlikely that there will be nonce
        -- collisions (that we have a good prng).
        let nodeState' = case provenance of
                Remote _ _ _ -> nodeState {
                      _nodeStateInbound = Set.insert waitForIt (_nodeStateInbound nodeState)
                    }
                Local _ (Just (nonce, peerDataVar, channelIn)) -> nodeState {
                      _nodeStateOutboundBidirectional = Map.insert nonce (waitForIt, channelIn, peerDataVar, False) (_nodeStateOutboundBidirectional nodeState)
                    }
                Local _ Nothing -> nodeState {
                      _nodeStateOutboundUnidirectional = Set.insert waitForIt (_nodeStateOutboundUnidirectional nodeState)
                    }

        statistics' <- stAddHandler provenance (_nodeStateStatistics nodeState)
        return (nodeState' { _nodeStateStatistics = statistics' }, promise)
    where

    normal :: Promise m () -> Microsecond -> m t
    normal promise startTime = do
        t <- action
        signalFinished promise startTime Nothing
        pure t

    exceptional :: Promise m () -> Microsecond -> SomeException -> m t
    exceptional promise startTime e = do
        signalFinished promise startTime (Just e)
        throw e

    signalFinished :: Promise m () -> Microsecond -> Maybe SomeException -> m ()
    signalFinished promise startTime outcome = do
        endTime <- currentTime
        let elapsed = endTime - startTime
        modifySharedAtomic stateVar $ \nodeState -> do
            let nodeState' = case provenance of
                    Remote _ _ _ -> nodeState {
                          _nodeStateInbound = Set.delete promise (_nodeStateInbound nodeState)
                        }
                    Local _ (Just (nonce, _, _)) -> nodeState {
                          _nodeStateOutboundBidirectional = Map.delete nonce (_nodeStateOutboundBidirectional nodeState)
                        }
                    Local _ Nothing -> nodeState {
                          _nodeStateOutboundUnidirectional = Set.delete promise (_nodeStateOutboundUnidirectional nodeState)
                        }
            statistics' <- stRemoveHandler provenance elapsed outcome (_nodeStateStatistics nodeState)
            return (nodeState' { _nodeStateStatistics = statistics' }, ())

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

-- | Create, use, and tear down a conversation channel with a given peer
--   (NodeId).
withInOutChannel
    :: forall packingType peerData m a .
       ( Mockable Bracket m, Mockable Async m, Ord (Promise m ())
       , Mockable SharedAtomic m, Mockable Throw m
       , Mockable SharedExclusive m
       , Mockable Catch m, Mockable Channel.Channel m
       , Mockable CurrentTime m, Mockable Metrics.Metrics m
       , MonadFix m, WithLogger m
       , Message.Packable packingType peerData )
    => Node packingType peerData m
    -> NodeId
    -> (SharedExclusiveT m peerData -> ChannelIn m -> ChannelOut m -> m a)
    -> m a
withInOutChannel node@Node{nodeState} nodeid@(NodeId peer) action = do
    nonce <- modifySharedAtomic nodeState $ \nodeState -> do
               let (nonce, !prng') = random (_nodeStateGen nodeState)
               pure (nodeState { _nodeStateGen = prng' }, nonce)
    channel <- fmap ChannelIn Channel.newChannel
    -- The dispatcher will fill in the peer data as soon as it's available.
    -- TODO must ensure that at some point it is always filled. What if the
    -- peer never responds? All we can do is time-out I suppose.
    -- Indeed, the peer may never even ACK.
    peerDataVar <- newSharedExclusive
    let provenance = Local peer (Just (nonce, peerDataVar, channel))
    let action' :: ChannelOut m -> m a
        action' = action peerDataVar channel
    -- connectInOutChannel will update the nonce state to indicate that there's
    -- a handler for it. When the handler is finished (whether normally or
    -- exceptionally) we have to update it to say so.
    promise <- spawnHandler nodeState provenance $
        bracket (connectInOutChannel node nodeid nonce)
                (\(ChannelOut conn) -> disconnectFromPeer node nodeid conn)
                (action')
    wait promise

-- | Create, use, and tear down a unidirectional channel to a peer identified
--   by 'NodeId'.
withOutChannel
    :: ( Mockable Bracket m, Mockable Async m, Ord (Promise m ())
       , Mockable Throw m, Mockable Catch m
       , Mockable SharedAtomic m, Mockable CurrentTime m
       , Mockable SharedExclusive m
       , Mockable Metrics.Metrics m
       , MonadFix m, WithLogger m
       , Message.Packable packingType peerData )
    => Node packingType peerData m
    -> NodeId
    -> (ChannelOut m -> m a)
    -> m a
withOutChannel node@Node{nodeState} nodeid@(NodeId peer) action = do
    let provenance = Local peer Nothing
    promise <- spawnHandler nodeState provenance $
        bracket (connectOutChannel node nodeid)
                (\(ChannelOut conn) -> disconnectFromPeer node nodeid conn)
                action
    wait promise

disconnectFromPeer
    :: ( Mockable SharedExclusive m
       , Mockable SharedAtomic m
       , Mockable Bracket m )
    => Node packingType peerData m
    -> NodeId
    -> NT.Connection m
    -> m ()
disconnectFromPeer Node{nodeState} nodeid@(NodeId peer) conn =
    NT.close conn `finally` cleanup

    where

    cleanup = modifySharedAtomic nodeState $ \nodeState -> case Map.lookup peer (_nodeStateConnectedTo nodeState) of

        Nothing -> do
            -- logWarning $ sformat ("disconnectFromPeer inconsistent state")
            return (nodeState, ())

        Just (Left excl) -> do
            -- Put an early-disconnect exception.
            -- TODO should have a third option for early disconnect, so that
            -- other guys can know to try again.
            putSharedExclusive excl (Just (error "early disconnect"))
            let nodeState' = nodeState {
                      _nodeStateConnectedTo = Map.delete peer (_nodeStateConnectedTo nodeState)
                    }
            return (nodeState', ())

        Just (Right n) -> case n of
            1 -> let nodeState' = nodeState {
                           _nodeStateConnectedTo = Map.delete peer (_nodeStateConnectedTo nodeState)
                         }
                 in  return (nodeState', ())
            n -> let nodeState' = nodeState {
                           _nodeStateConnectedTo = Map.insert peer (Right (n - 1)) (_nodeStateConnectedTo nodeState)
                         }
                 in  return (nodeState', ())

-- | Connect to a peer, taking care to send the peer-data in case there are no
--   other connections to that peer. Subsequent connections to that peer
--   will block until the peer-data is sent; it must be the first thing to
--   arrive when the first lightweight connection to a peer is opened.
connectToPeer
    :: ( Mockable Throw m
       , Mockable Bracket m
       , Mockable SharedAtomic m
       , Mockable SharedExclusive m
       , Message.Packable packingType peerData
       )
    => Node packingType peerData m
    -> NodeId
    -> m (NT.Connection m)
connectToPeer Node{nodeEndPoint, nodeState, nodePackingType, nodePeerData} nodeid@(NodeId peer) = do
    mconn <- NT.connect nodeEndPoint
                        peer
                        NT.ReliableOrdered
                        -- TODO give a timeout. Can't rely on it being set at
                        -- the transport level.
                        NT.ConnectHints{ connectTimeout = Nothing }
    case mconn of

        Left err -> throw err

        Right conn -> do

            -- Check the shared state and, in case this is the first connection to
            -- that peer, send the peer data.
            let getResponsibility = modifySharedAtomic nodeState $ \nodeState -> case Map.lookup peer (_nodeStateConnectedTo nodeState) of
                    -- Nothing here. We'll send the peer data.
                    Nothing -> do
                        excl <- newSharedExclusive
                        let nodeState' = nodeState {
                                  _nodeStateConnectedTo = Map.insert peer (Left excl) (_nodeStateConnectedTo nodeState)
                                }
                        return (nodeState', Just (Left excl))
                    Just (Left excl) -> return (nodeState, Just (Right excl))
                    Just (Right !n) -> do
                        let nodeState' = nodeState {
                                  _nodeStateConnectedTo = Map.insert peer (Right (n + 1)) (_nodeStateConnectedTo nodeState)
                                }
                        return (nodeState', Nothing)

            let fulfillResponsibility responsibility = case responsibility of

                    Nothing -> return conn

                    Just (Left excl) -> do
                        let serializedPeerData = Message.packMsg nodePackingType nodePeerData
                        outcome <- NT.send conn (LBS.toChunks serializedPeerData)
                        case outcome of
                            -- Throwing here will cause the bracketed
                            -- onException to run, filling the shared exclusive
                            -- with this exception.
                            Left err -> throw err
                            Right () -> do
                                modifySharedAtomic nodeState $ \nodeState ->
                                    let nodeState' = nodeState {
                                              _nodeStateConnectedTo = Map.insert peer (Right 1) (_nodeStateConnectedTo nodeState)
                                            }
                                    in  return (nodeState', ())
                                putSharedExclusive excl Nothing
                                return conn

                    Just (Right excl) -> do
                        outcome <- readSharedExclusive excl
                        case outcome of
                            -- If the first one to connect threw an exception,
                            -- throw it here too.
                            Just exception -> throw exception
                            _ -> return ()
                        modifySharedAtomic nodeState $ \nodeState ->
                            let nodeState' = nodeState {
                                      _nodeStateConnectedTo = Map.update (Just . fmap (+ 1)) peer (_nodeStateConnectedTo nodeState)
                                    }
                            in  return (nodeState', ())
                        return conn

            let onException responsibility (exception :: Maybe SomeException) = case (responsibility, exception) of
                    (Just (Left excl), Just e) -> putSharedExclusive excl (Just e)
                    _ -> return ()

            bracketWithException getResponsibility
                                 onException
                                 fulfillResponsibility

-- | Connect to a peer given by a 'NodeId' bidirectionally.
connectInOutChannel
    :: ( Mockable Channel.Channel m
       , Mockable Throw m
       , Mockable Bracket m
       , Mockable SharedAtomic m
       , Mockable SharedExclusive m
       , Message.Packable packingType peerData
       )
    => Node packingType peerData m
    -> NodeId
    -> Nonce
    -> m (ChannelOut m)
connectInOutChannel node peer nonce = do
    conn <- connectToPeer node peer
    outcome <- NT.send conn [controlHeaderBidirectionalSyn nonce]
    case outcome of
        Left err -> throw err
        Right _ -> return ()
    return (ChannelOut conn)

-- | Connect to a peer given by a 'NodeId' unidirectionally.
connectOutChannel
    :: ( Mockable Throw m
       , Mockable Bracket m
       , Mockable SharedAtomic m
       , Mockable SharedExclusive m
       , Message.Packable packingType peerData
       )
    => Node packingType peerData m
    -> NodeId
    -> m (ChannelOut m)
connectOutChannel node peer = do
    conn <- connectToPeer node peer
    outcome <- NT.send conn [controlHeaderUnidirectional]
    case outcome of
        Left err -> throw err
        Right () -> return ()
    return (ChannelOut conn)
