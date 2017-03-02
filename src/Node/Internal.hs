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
{-# LANGUAGE UndecidableInstances       #-}

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
import           Control.Monad                 (forM_, forM, when)
import           Control.Monad.Fix             (MonadFix)
import           Data.Int                      (Int64)
import           Data.Binary                   as Bin
import           Data.Binary.Get               as Bin
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Foldable                 (foldlM, foldl', toList)
import           Data.Hashable                 (Hashable)
import           Data.List                     (intercalate)
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.NonEmptySet              (NonEmptySet)
import qualified Data.NonEmptySet              as NESet
import           Data.Sequence                 (Seq)
import qualified Data.Sequence                 as Seq
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

-- | A 'NodeId' wraps a network-transport endpoint address
newtype NodeId = NodeId NT.EndPointAddress
  deriving (Eq, Ord, Show, Hashable)

-- | The state of a Node, to be held in a shared atomic cell because other
--   threads will mutate it in order to set up bidirectional connections.
data NodeState peerData m = NodeState {
      _nodeStateGen                    :: !StdGen
      -- ^ To generate nonces.
    , _nodeStateOutboundUnidirectional :: !(Map NT.EndPointAddress (SomeHandler m))
      -- ^ Handlers for each locally-initiated unidirectional connection.
    , _nodeStateOutboundBidirectional  :: !(Map NT.EndPointAddress (Map Nonce (SomeHandler m, ChannelIn m, Int -> m (), SharedExclusiveT m peerData, NT.ConnectionBundle, Bool)))
      -- ^ Handlers for each nonce which we generated (locally-initiated
      --   bidirectional connections).
      --   The bool indicates whether we have received an ACK for this.
    , _nodeStateInbound                :: !(Set (SomeHandler m))
      -- ^ Handlers for inbound connections (remotely-initiated unidirectional
      --   _or_ bidirectional connections).
    , _nodeStateConnectedTo        :: !(Map NT.EndPointAddress (OutboundConnectionState m))
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
            , _nodeStateOutboundUnidirectional = Map.empty
            , _nodeStateOutboundBidirectional = Map.empty
            , _nodeStateInbound = Set.empty
            , _nodeStateConnectedTo = Map.empty
            , _nodeStateStatistics = stats
            , _nodeStateClosed = False
            }
    newSharedAtomic nodeState

data SomeHandler m = forall t . SomeHandler {
      someHandlerThreadId :: !(ThreadId m)
    , someHandlerPromise :: !(Promise m t)
    }

-- | Correctness relies on the assumption that the ThreadId is that of the
--   Promise, and that two Promises with the same ThreadId are the same.
--   Is this reasonable?
instance (Eq (ThreadId m)) => Eq (SomeHandler m) where
    SomeHandler tid1 _ == SomeHandler tid2 _ = tid1 == tid2

instance (Ord (ThreadId m)) => Ord (SomeHandler m) where
    SomeHandler tid1 _ `compare` SomeHandler tid2 _ = tid1 `compare` tid2

waitSomeHandler :: ( Mockable Async m ) => SomeHandler m -> m ()
waitSomeHandler (SomeHandler _ promise) = () <$ wait promise

makeSomeHandler :: ( Mockable Async m ) => Promise m t -> m (SomeHandler m)
makeSomeHandler promise = do
    tid <- asyncThreadId promise
    return $ SomeHandler tid promise

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
    , stPeerStatistics :: !(Map NT.EndPointAddress (SharedAtomicT m PeerStatistics))
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

stTotalLiveBytes
    :: (Mockable SharedAtomic m)
    => Statistics m -> m Int
stTotalLiveBytes stats = do
    allPeers <- mapM readSharedAtomic $ Map.elems (stPeerStatistics stats)
    let allBytes = fmap (pstLiveBytes) allPeers
    return $ sum allBytes

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
      -- | How many bytes have been received by running handlers for this
      --   peer.
    , pstLiveBytes :: !Int
    }

pstNull :: PeerStatistics -> Bool
pstNull PeerStatistics{..} =
    let rem = pstRunningHandlersRemote
        loc = pstRunningHandlersLocal
    in  rem == 0 && loc == 0


stIncrBytes
    :: (Mockable SharedAtomic m)
    => NT.EndPointAddress -> Int -> Statistics m -> m ()
stIncrBytes peer bytes stats =
    case Map.lookup peer (stPeerStatistics stats) of
      Nothing -> return ()
      Just peerStats -> modifySharedAtomic peerStats $ \ps ->
          return (pstIncrBytes bytes ps, ())

pstIncrBytes :: Int -> PeerStatistics -> PeerStatistics
pstIncrBytes bytes peerStatistics = peerStatistics {
      pstLiveBytes = pstLiveBytes peerStatistics + bytes
    }

-- | Record a new handler for a given peer. Second component is True if it's the
--   only handler for that peer.
pstAddHandler
    :: (Mockable SharedAtomic m)
    => HandlerProvenance peerData m t
    -> Map NT.EndPointAddress (SharedAtomicT m PeerStatistics)
    -> m (Map NT.EndPointAddress (SharedAtomicT m PeerStatistics), Bool)
pstAddHandler provenance map = case provenance of

    Local peer _ -> case Map.lookup peer map of
        Nothing ->
            newSharedAtomic (PeerStatistics 0 1 0) >>= \peerStatistics ->
            return (Map.insert peer peerStatistics map, True)
        Just !statsVar -> modifySharedAtomic statsVar $ \stats ->
            let !stats' = stats { pstRunningHandlersLocal = pstRunningHandlersLocal stats + 1 }
            in return (stats', (map, False))

    Remote peer _ _ -> case Map.lookup peer map of
        Nothing ->
            newSharedAtomic (PeerStatistics 1 0 0) >>= \peerStatistics ->
            return (Map.insert peer peerStatistics map, True)
        Just !statsVar -> modifySharedAtomic statsVar $ \stats ->
            let !stats' = stats { pstRunningHandlersLocal = pstRunningHandlersRemote stats + 1 }
            in return (stats', (map, False))

-- | Remove a handler for a given peer. Second component is True if there
--   are no more handlers for that peer.
pstRemoveHandler
    :: (WithLogger m, Mockable SharedAtomic m)
    => HandlerProvenance peerData m t
    -> Map NT.EndPointAddress (SharedAtomicT m PeerStatistics)
    -> m (Map NT.EndPointAddress (SharedAtomicT m PeerStatistics), Bool)
pstRemoveHandler provenance map = case provenance of

    Local peer _ -> case Map.lookup peer map of
        Nothing ->  do
            logWarning $ sformat ("tried to remove handler for "%shown%", but it is not in the map") peer
            return (map, False)
        Just !statsVar -> modifySharedAtomic statsVar $ \stats ->
            let stats' = stats { pstRunningHandlersLocal = pstRunningHandlersLocal stats - 1 }
            in return $ if pstNull stats'
                        then (stats', (Map.delete peer map, True))
                        else (stats', (map, False))

    Local peer _ -> case Map.lookup peer map of
        Nothing ->  do
            logWarning $ sformat ("tried to remove handler for "%shown%", but it is not in the map") peer
            return (map, False)
        Just !statsVar -> modifySharedAtomic statsVar $ \stats ->
            let stats' = stats { pstRunningHandlersRemote = pstRunningHandlersRemote stats - 1 }
            in return $ if pstNull stats'
                        then (stats', (Map.delete peer map, True))
                        else (stats', (map, False))

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
      Local !NT.EndPointAddress (Maybe (Nonce, SharedExclusiveT m peerData, NT.ConnectionBundle, t))
      -- | Initiated remotely, _by_ or _from_ this peer.
    | Remote !NT.EndPointAddress !NT.ConnectionId t

instance Show (HandlerProvenance peerData m t) where
    show prov = case prov of
        Local addr mdata -> concat [
              "Local "
            , show addr
            , show (fmap (\(x,_,_,_) -> x) mdata)
            ]
        Remote addr connid _ -> concat ["Remote ", show addr, show connid]

handlerProvenancePeer :: HandlerProvenance peerData m t -> NT.EndPointAddress
handlerProvenancePeer provenance = case provenance of
    Local peer _ -> peer
    Remote peer _ _ -> peer

-- TODO: revise these computations to make them numerically stable (or maybe
-- use Rational?).
stAddHandler
    :: ( Mockable Metrics.Metrics m
       , Mockable SharedAtomic m )
    => HandlerProvenance peerData m t
    -> Statistics m
    -> m (Statistics m)
stAddHandler !provenance !statistics = case provenance of

    -- TODO: generalize this computation so we can use the same thing for
    -- both local and remote. It's a copy/paste job right now swapping local
    -- for remote.
    Local !peer _ -> do
        (!peerStatistics, !isNewPeer) <- pstAddHandler provenance (stPeerStatistics statistics)
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
        (!peerStatistics, !isNewPeer) <- pstAddHandler provenance (stPeerStatistics statistics)
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
    :: ( Mockable Metrics.Metrics m, Mockable SharedAtomic m, WithLogger m )
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
        (!peerStatistics, !isEndedPeer) <- pstRemoveHandler provenance (stPeerStatistics statistics)
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
        (!peerStatistics, !isEndedPeer) <- pstRemoveHandler provenance (stPeerStatistics statistics)
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
       , Mockable Async m, Mockable Concurrently m
       , Ord (ThreadId m), Show (ThreadId m)
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
                      nodeDispatcher node handlerIn handlerOut
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
      --
      --   Second argument will be run with the number of bytes each time more
      --   bytes are received. It's used to update shared metrics.
    | FeedingApplicationHandler !(ChannelIn m) (Int -> m ())

instance Show (ConnectionState peerData m) where
    show term = case term of
        WaitingForPeerData -> "WaitingForPeerData"
        PeerDataParseFailure -> "PeerDataParseFailure"
        WaitingForHandshake _ _ -> "WaitingForHandshake"
        HandshakeFailure -> "HandshakeFailure"
        FeedingApplicationHandler _ _ -> "FeedingApplicationHandler"

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
      dsConnections :: Map NT.ConnectionId (NT.EndPointAddress, ConnectionState peerData m)
    , dsPeers :: Map NT.EndPointAddress (PeerState peerData)
    }

deriving instance Show (DispatcherState peerData m)

initialDispatcherState :: DispatcherState peerData m
initialDispatcherState = DispatcherState Map.empty Map.empty

-- | Wait for every running handler in a node's state to finish. Exceptions are
--   caught and gathered, not re-thrown.
waitForRunningHandlers
    :: forall m packingType peerData .
       ( Mockable SharedAtomic m
       , Mockable Async m
       , Mockable Catch m
       , WithLogger m
       , Show (ThreadId m)
       )
    => Node packingType peerData m
    -> m [Maybe SomeException]
waitForRunningHandlers node = do
    -- Gather the promises for all handlers.
    handlers <- withSharedAtomic (nodeState node) $ \st -> do
        let outbound_uni = Map.elems (_nodeStateOutboundUnidirectional st)
            -- List monad computation: grab the values of the map (ignoring
            -- peer keys), then for each of those maps grab its values (ignoring
            -- nonce keys) and then return the promise.
            outbound_bi = do
                map <- Map.elems (_nodeStateOutboundBidirectional st)
                (x, _, _, _, _, _) <- Map.elems map
                return x
            inbound = Set.toList (_nodeStateInbound st)
            all = outbound_uni ++ outbound_bi ++ inbound
        logDebug $ sformat ("waiting for " % shown % " outbound unidirectional handlers") (fmap (someHandlerThreadId) outbound_uni)
        logDebug $ sformat ("waiting for " % shown % " outbound bidirectional handlers") (fmap (someHandlerThreadId) outbound_bi)
        logDebug $ sformat ("waiting for " % shown % " outbound inbound") (fmap (someHandlerThreadId) inbound)
        return all
    let waitAndCatch someHandler = do
            logDebug $ sformat ("waiting on " % shown) (someHandlerThreadId someHandler)
            (Nothing <$ waitSomeHandler someHandler) `catch` (\(e :: SomeException) -> return (Just e))
    forM handlers waitAndCatch

-- | The one thread that handles /all/ incoming messages and dispatches them
-- to various handlers.
nodeDispatcher
    :: forall m packingType peerData .
       ( Mockable SharedAtomic m, Mockable Async m, Mockable Concurrently m
       , Ord (ThreadId m), Mockable Bracket m, Mockable SharedExclusive m
       , Mockable Channel.Channel m, Mockable Throw m, Mockable Catch m
       , Mockable CurrentTime m, Mockable Metrics.Metrics m
       , Message.Serializable packingType peerData
       , MonadFix m, WithLogger m, Show (ThreadId m) )
    => Node packingType peerData m
    -> (peerData -> NodeId -> ChannelIn m -> m ())
    -> (peerData -> NodeId -> ChannelIn m -> ChannelOut m -> m ())
    -> m ()
nodeDispatcher node handlerIn handlerInOut =
    loop initialDispatcherState

    where

    nstate :: SharedAtomicT m (NodeState peerData m)
    nstate = nodeState node

    endpoint = nodeEndPoint node

    loop :: DispatcherState peerData m -> m ()
    loop !state = do
      event <- NT.receive endpoint
      case event of

          NT.ConnectionOpened connid reliability peer ->
              connectionOpened state connid reliability peer >>= loop

          NT.Received connid bytes -> received state connid bytes >>= loop

          NT.ConnectionClosed connid -> connectionClosed state connid >>= loop

          -- When the end point closes, we're done.
          NT.EndPointClosed -> endPointClosed state

          -- When a heavyweight connection is lost we must close up all of the
          -- lightweight connections which it carried.
          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode (NT.EventConnectionLost peer bundle)) msg) ->
              connectionLost state peer bundle >>= loop

          -- Unsupported event is recoverable. Just log and carry on.
          NT.ErrorEvent err@(NT.TransportError NT.UnsupportedEvent _) -> do
              logError $ sformat shown err
              loop state

          -- End point failure is unrecoverable.
          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode NT.EventEndPointFailed) _) ->
              throw (InternalError "EndPoint failed")

          -- Transport failure is unrecoverable.
          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode NT.EventTransportFailed) _) ->
              throw (InternalError "Transport failed")

    -- EndPointClosed is the final event that we will receive. There may be
    -- connections which remain open! ConnectionClosed events may be
    -- inbound but since our end point has closed, we won't take them. So here
    -- we have to plug every remaining input channel.
    endPointClosed
        :: DispatcherState peerData m
        -> m ()
    endPointClosed state = do
        let connections = Map.toList (dsConnections state)
        -- This is a network-transport error (ConnectionClosed should have
        -- been posted for all open connections), but we're defensive and
        -- plug the channels.
        when (length connections > 0) $ do
            logError $ sformat ("end point closed with " % shown % " open connection(s)") (length connections)
            forM_ connections $ \(_, st) -> case st of
                (_, FeedingApplicationHandler (ChannelIn channel) _) -> do
                    Channel.writeChannel channel Nothing
                _ -> return ()

        -- Must plug input channels for all un-acked outbound connections, and
        -- fill the peer data vars in case they haven't yet been filled. This
        -- is to ensure that handlers never block on these things.
        _ <- modifySharedAtomic nstate $ \st -> do
            let nonceMaps = Map.elems (_nodeStateOutboundBidirectional st)
            let outbounds = nonceMaps >>= Map.elems
            forM_ outbounds $ \(_, ChannelIn chan, _, peerDataVar, _, acked) -> do
                when (not acked) $ do
                   tryPutSharedExclusive peerDataVar (error "no peer data because local node has gone down")
                   Channel.writeChannel chan Nothing
            return (st, ())

        _ <- waitForRunningHandlers node
        return ()

    connectionOpened
        :: DispatcherState peerData m
        -> NT.ConnectionId
        -> NT.Reliability
        -> NT.EndPointAddress
        -> m (DispatcherState peerData m)
    connectionOpened state connid reliability peer = case Map.lookup connid (dsConnections state) of

        Just (peer', _) -> do
            logWarning $ sformat ("ignoring duplicate connection " % shown % shown % shown) peer peer' connid
            return state

        Nothing -> do

            -- How we handle this connection depends on whether we already have
            -- a connection from this peer.
            case Map.lookup peer (dsPeers state) of

                -- If we do, we can start waiting for the handshake.
                Just (GotPeerData peerData neset) -> do
                    return $ state {
                          dsConnections = Map.insert connid (peer, WaitingForHandshake peerData BS.empty) (dsConnections state)
                        , dsPeers = Map.insert peer (GotPeerData peerData (NESet.insert connid neset)) (dsPeers state)
                        }

                -- If we don't, then we must await and decode the peer data.
                Nothing -> do
                    return $ state {
                          dsConnections = Map.insert connid (peer, WaitingForPeerData) (dsConnections state)
                        , dsPeers = Map.insert peer (ExpectingPeerData (NESet.singleton connid) Nothing) (dsPeers state)
                        }

                -- We got another connection before the peer data arrived.
                -- That's actually OK. It's only an error if we receive data
                -- on this connection before the first connection receives
                -- and parses the peer data ('received' handles this aspect).
                -- So here we just record the connection.
                Just (ExpectingPeerData neset mleader) -> do
                    return $ state {
                          dsConnections = Map.insert connid (peer, WaitingForPeerData) (dsConnections state)
                        , dsPeers = Map.insert peer (ExpectingPeerData (NESet.insert connid neset) mleader) (dsPeers state)
                        }

    received
        :: DispatcherState peerData m
        -> NT.ConnectionId
        -> [BS.ByteString]
        -> m (DispatcherState peerData m)
    received state connid chunks = case Map.lookup connid (dsConnections state) of

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
        Just (peer, WaitingForPeerData) -> case Map.lookup peer (dsPeers state) of

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
                                    dsConnections = Map.insert connid (peer, PeerDataParseFailure) (dsConnections state)
                                  }
                        Bin.Done trailing _ peerData -> do
                            return $ state {
                                    dsConnections = foldl' (awaitHandshake connid peerData trailing) (dsConnections state) (NESet.toList connids)
                                  , dsPeers = Map.insert peer (GotPeerData peerData connids) (dsPeers state)
                                  }
                        Bin.Partial decoderContinuation -> do
                            return $ state {
                                    dsPeers = Map.insert peer (ExpectingPeerData connids (Just (connid, decoderContinuation))) (dsPeers state)
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
                                    dsConnections = Map.insert connid (peer, PeerDataParseFailure) (dsConnections state)
                                  }

                        Bin.Done trailing _ peerData -> do
                            return $ state {
                                    dsConnections = foldl' (awaitHandshake connid peerData trailing) (dsConnections state) (NESet.toList connids)
                                  , dsPeers = Map.insert peer (GotPeerData peerData connids) (dsPeers state)
                                  }

                        Bin.Partial decoderContinuation' -> do
                            return $ state {
                                    dsPeers = Map.insert peer (ExpectingPeerData connids (Just (connid, decoderContinuation'))) (dsPeers state)

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
            let bytes = BS.append partial (BS.concat chunks)
            case BS.uncons bytes of

                Nothing -> return state

                Just (w, ws)

                    -- Got unidirectional header. Create a channel and
                    -- spawn the application handler.
                    | w == controlHeaderCodeUnidirectional -> do
                          channel <- Channel.newChannel
                          let provenance = Remote peer connid (ChannelIn channel)
                          let handler = handlerIn peerData (NodeId peer) (ChannelIn channel)
                          (_, incrBytes) <- spawnHandler nstate provenance handler
                          Channel.writeChannel channel (Just ws)
                          incrBytes $ BS.length ws
                          return $ state {
                                dsConnections = Map.insert connid (peer, FeedingApplicationHandler (ChannelIn channel) incrBytes) (dsConnections state)
                              }

                    -- Got a bidirectional header but still waiting for the
                    -- nonce.
                    | w == controlHeaderCodeBidirectionalSyn ||
                      w == controlHeaderCodeBidirectionalAck
                    , BS.length ws < 8 -> return $ state {
                            dsConnections = Map.insert connid (peer, WaitingForHandshake peerData bytes) (dsConnections state)
                          }

                    -- Got a SYN. Spawn a thread to connect to the peer using
                    -- the nonce provided and then run the bidirectional handler.
                    | w == controlHeaderCodeBidirectionalSyn
                    , Right (ws', _, nonce) <- decodeOrFail (LBS.fromStrict ws) -> do
                          channel <- Channel.newChannel
                          let provenance = Remote peer connid (ChannelIn channel)
                          let acquire = connectToPeer node (NodeId peer)
                          let respondAndHandle conn = do
                                  outcome <- NT.send conn [controlHeaderBidirectionalAck nonce]
                                  case outcome of
                                      Left err -> throw err
                                      Right () -> do
                                          handlerInOut peerData (NodeId peer) (ChannelIn channel) (ChannelOut conn)
                          -- Resource releaser for bracketWithException.
                          -- No matter what, we must update the node state to
                          -- indicate that we've disconnected from the peer.
                          let cleanup conn (me :: Maybe SomeException) = do
                                  disconnectFromPeer node (NodeId peer) conn
                                  case me of
                                      Nothing -> return ()
                                      Just e -> logError $
                                          sformat (shown % " error in conversation response " % shown) nonce e
                          let handler = bracketWithException
                                            acquire
                                            cleanup
                                            respondAndHandle
                          -- Establish the other direction in a separate thread.
                          (_, incrBytes) <- spawnHandler nstate provenance handler
                          let bss = LBS.toChunks ws'
                          Channel.writeChannel channel (Just (BS.concat bss))
                          incrBytes $ sum (fmap BS.length bss)
                          return $ state {
                                dsConnections = Map.insert connid (peer, FeedingApplicationHandler (ChannelIn channel) incrBytes) (dsConnections state)
                              }

                    -- Got an ACK. Try to decode the nonce and check that
                    -- we actually sent it.
                    | w == controlHeaderCodeBidirectionalAck
                    , Right (ws', _, nonce) <- decodeOrFail (LBS.fromStrict ws) -> do
                          outcome <- modifySharedAtomic nstate $ \st -> do
                              -- Lookup the nonce map for the peer, then check
                              -- that nonce map at the supplied nonce.
                              let nonces = Map.lookup peer (_nodeStateOutboundBidirectional st)
                              let thisNonce = nonces >>= Map.lookup nonce
                              case thisNonce of
                                  Nothing -> return (st, Nothing)
                                  Just (_, _, _, _, _, True) -> return (st, Just Nothing)
                                  Just (promise, channel, incrBytes, peerDataVar, connBundle, False) -> return
                                      ( st { _nodeStateOutboundBidirectional = Map.update updater peer (_nodeStateOutboundBidirectional st)
                                           }
                                      , Just (Just (channel, incrBytes, peerDataVar))
                                      )
                                      where
                                      updater map = Just $ Map.insert nonce (promise, channel, incrBytes, peerDataVar, connBundle, True) map
                          case outcome of
                              -- We don't know about the nonce. Could be that
                              -- we never sent the SYN for it (protocol error)
                              -- or the handler for it has already finished.
                              -- In any case, say the handshake failed so that
                              -- subsequent data is ignored.
                              Nothing -> do
                                  logWarning $ sformat ("got unknown nonce " % shown) nonce
                                  return $ state {
                                        dsConnections = Map.insert connid (peer, HandshakeFailure) (dsConnections state)
                                      }

                              -- Got a duplicate ACK.
                              Just Nothing -> do
                                  logWarning $ sformat ("duplicate ACK nonce from " % shown) peer
                                  return $ state {
                                        dsConnections = Map.insert connid (peer, HandshakeFailure) (dsConnections state)
                                      }

                              -- Got an ACK for a SYN that we sent. Start
                              -- feeding the application handler.
                              Just (Just (ChannelIn channel, incrBytes, peerDataVar)) -> do
                                  putSharedExclusive peerDataVar peerData
                                  let bs = LBS.toStrict ws'
                                  Channel.writeChannel channel (Just bs)
                                  incrBytes $ BS.length bs
                                  return $ state {
                                        dsConnections = Map.insert connid (peer, FeedingApplicationHandler (ChannelIn channel) incrBytes) (dsConnections state)
                                      }

                    -- Handshake failure. Subsequent receives will be ignored.
                    | otherwise -> do
                          logWarning $ sformat ("unexpected control header from " % shown % " : " % shown) peer w
                          return $ state {
                                dsConnections = Map.insert connid (peer, HandshakeFailure) (dsConnections state)
                              }

        -- This connection is feeding a handler. Make the data available.
        -- TODO: if the handler has already finished, we want to just forget
        -- the data. How? Weak reference to the channel perhaps? Or
        -- explcitly close it down when the handler finishes by adding some
        -- mutable cell to FeedingApplicationHandler?
        Just (peer, FeedingApplicationHandler (ChannelIn channel) incrBytes) -> do
            Channel.writeChannel channel (Just (BS.concat chunks))
            incrBytes $ sum (fmap BS.length chunks)
            return state

    connectionClosed
        :: DispatcherState peerData m
        -> NT.ConnectionId
        -> m (DispatcherState peerData m)
    connectionClosed state connid = case Map.lookup connid (dsConnections state) of

        Nothing -> do
            logWarning $ sformat ("closed unknown connection " % shown) connid
            return state

        Just (peer, connState) -> do
            case connState of
                FeedingApplicationHandler (ChannelIn channel) _ -> do
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
                      dsConnections = Map.delete connid (dsConnections state)
                    , dsPeers = Map.update peersUpdater peer (dsPeers state)
                    }
            return state'

    connectionLost
        :: DispatcherState peerData m
        -> NT.EndPointAddress
        -> NT.ConnectionBundle
        -> m (DispatcherState peerData m)
    connectionLost state peer bundle = do
        -- There must always be 0 connections from the peer, for
        -- network-transport must have posted the ConnectionClosed events for
        -- every inbound connection before posting EventConnectionLost.
        logWarning $ sformat ("lost connection bundle " % shown % " to " % shown) bundle peer
        state' <- case Map.lookup peer (dsPeers state) of
            Just it -> do
                -- This is a network-transport bug, but we're defensive: will
                -- clean up the state and plug the input channels anyway.
                let connids = case it of
                        GotPeerData _ neset -> NESet.toList neset
                        ExpectingPeerData neset _ -> NESet.toList neset
                logError $ sformat ("still have " % shown % " connections") (length connids)
                -- For every connection to that peer we'll plug the channel with
                -- Nothing and remove it from the map.
                let folder :: Map NT.ConnectionId (NT.EndPointAddress, ConnectionState peerData m)
                           -> NT.ConnectionId
                           -> m (Map NT.ConnectionId (NT.EndPointAddress, ConnectionState peerData m))
                    folder channels connid = case Map.updateLookupWithKey (\_ _ -> Nothing) connid channels of
                        (Just (_, FeedingApplicationHandler (ChannelIn channel) _), channels') -> do

                            Channel.writeChannel channel Nothing
                            return channels'
                        (Nothing, channels') -> do
                            logWarning $ sformat "inconsistent peer and connection identifier state"
                            return channels'
                channels' <- foldlM folder (dsConnections state) connids
                return $ state {
                      dsConnections = channels'
                    , dsPeers = Map.delete peer (dsPeers state)
                    }
            Nothing -> return state

        -- Every outbound bidirectional connection which is carried by this
        -- bundle, and which has not yet received an ACK, must have its
        -- channel plugged and its peer data shared exclusive filled in case
        -- it has not yet been. This is to ensure that the handlers do not
        -- block indefinitely when trying to access these things.
        --
        -- Outbound unidirectional connections need no attention: they will
        -- fail if they try to 'send', but since they expect no data in
        -- return, we don't have to take care of them here.
        channelsAndPeerDataVars <- modifySharedAtomic nstate $ \st -> do
            let nonces = Map.lookup peer (_nodeStateOutboundBidirectional st)
            case nonces of
                -- Perfectly normal: lost the connection but we had no
                -- outbound bidirectional connections to it.
                Nothing -> return (st, [])
                Just map -> do
                    -- Remove every element from the map which is carried by
                    -- this bundle, and then remove the map itself if it's
                    -- empty.
                    let folder (_, channelIn, _, peerDataVar, bundle', acked) channels
                            | bundle' == bundle && not acked = (channelIn, peerDataVar) : channels
                            | otherwise = channels

                    let channelsAndPeerDataVars = Map.foldr folder [] map
                    return (st, channelsAndPeerDataVars)

        logWarning $ sformat ("closing " % shown % " channels on bundle " % shown % " to " % shown) (length channelsAndPeerDataVars) bundle peer

        forM_ channelsAndPeerDataVars $ \(ChannelIn chan, peerDataVar) -> do
            tryPutSharedExclusive peerDataVar (error "no peer data because the connection was lost")
            Channel.writeChannel chan Nothing

        return state'

-- | Spawn a thread and track it in shared state, taking care to remove it from
--   shared state when it's finished and updating statistics appropriately.
--   This is applicable to handlers spawned in response to inbound peer
--   connections, and also for actions which use outbound connections.
spawnHandler
    :: forall peerData m t .
       ( Mockable SharedAtomic m, Mockable Throw m, Mockable Catch m
       , Mockable Async m, Ord (ThreadId m)
       , Mockable Metrics.Metrics m, Mockable CurrentTime m
       , WithLogger m
       , MonadFix m )
    => SharedAtomicT m (NodeState peerData m)
    -> HandlerProvenance peerData m (ChannelIn m)
    -> m t
    -> m (Promise m t, Int -> m ())
spawnHandler stateVar provenance action =
    modifySharedAtomic stateVar $ \nodeState -> do
        totalBytes <- newSharedAtomic 0
        -- Spawn the thread to get a 'SomeHandler'.
        rec { promise <- async $ do
                  startTime <- currentTime
                  normal someHandler startTime totalBytes
                      `catch` exceptional someHandler startTime totalBytes
            ; someHandler <- makeSomeHandler promise
            }
        -- It is assumed that different promises do not compare equal.
        -- It is assumed to be highly unlikely that there will be nonce
        -- collisions (that we have a good prng).
        let nodeState' = case provenance of
                Remote _ _ _ -> nodeState {
                      _nodeStateInbound = Set.insert someHandler (_nodeStateInbound nodeState)
                    }
                Local peer (Just (nonce, peerDataVar, connBundle, channelIn)) -> nodeState {
                      _nodeStateOutboundBidirectional = Map.alter alteration peer (_nodeStateOutboundBidirectional nodeState)
                    }
                    where
                    alteration Nothing = Just $ Map.singleton nonce (someHandler, channelIn, incrBytes, peerDataVar, connBundle, False)
                    alteration (Just map) = Just $ Map.insert nonce (someHandler, channelIn, incrBytes, peerDataVar, connBundle, False) map
                Local peer Nothing -> nodeState {
                      _nodeStateOutboundUnidirectional = Map.insert peer someHandler (_nodeStateOutboundUnidirectional nodeState)
                    }

            incrBytes !n = do
                nodeState <- readSharedAtomic stateVar
                stIncrBytes (handlerProvenancePeer provenance) n (_nodeStateStatistics nodeState)
                modifySharedAtomic totalBytes $ \(!m) -> return (m + n, ())

        statistics' <- stAddHandler provenance (_nodeStateStatistics nodeState)
        return (nodeState' { _nodeStateStatistics = statistics' }, (promise, incrBytes))

    where

    normal :: SomeHandler m -> Microsecond -> SharedAtomicT m Int -> m t
    normal someHandler startTime totalBytesVar = do
        t <- action
        signalFinished someHandler startTime totalBytesVar Nothing
        pure t

    exceptional :: SomeHandler m -> Microsecond -> SharedAtomicT m Int -> SomeException -> m t
    exceptional someHandler startTime totalBytesVar e = do
        signalFinished someHandler startTime totalBytesVar (Just e)
        throw e

    signalFinished :: SomeHandler m -> Microsecond -> SharedAtomicT m Int -> Maybe SomeException -> m ()
    signalFinished someHandler startTime totalBytesVar outcome = do
        endTime <- currentTime
        let elapsed = endTime - startTime
        totalBytes <- readSharedAtomic totalBytesVar
        modifySharedAtomic stateVar $ \nodeState -> do
            let nodeState' = case provenance of
                    Remote _ _ _ -> nodeState {
                          _nodeStateInbound = Set.delete someHandler (_nodeStateInbound nodeState)
                        }
                    -- Remove the nonce for this peer, and remove the whole map
                    -- if this was the only nonce for that peer.
                    Local peer (Just (nonce, _, _, _)) -> nodeState {
                          _nodeStateOutboundBidirectional = Map.update updater peer (_nodeStateOutboundBidirectional nodeState)
                        }
                        where
                        updater map =
                            let map' = Map.delete nonce map
                            in  if Map.null map' then Nothing else Just map'
                    Local peer Nothing -> nodeState {
                          _nodeStateOutboundUnidirectional = Map.delete peer (_nodeStateOutboundUnidirectional nodeState)
                        }
            -- Decrement the live bytes by the total bytes received, and
            -- remove the handler.
            stIncrBytes (handlerProvenancePeer provenance) (-totalBytes) $ _nodeStateStatistics nodeState
            statistics' <-
                stRemoveHandler provenance elapsed outcome $
                _nodeStateStatistics nodeState
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
       ( Mockable Bracket m, Mockable Async m, Ord (ThreadId m)
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
    -- When the connection is up, we can register a handler using the bundle
    -- identifier.
    -- An exception may be thrown after the connection is established but
    -- before we register, but that's OK, as disconnectFromPeer is forgiving
    -- about this.
    let action' conn = do
            let provenance = Local peer (Just (nonce, peerDataVar, NT.bundle conn, channel))
            (promise, _) <- spawnHandler nodeState provenance $ do
                -- It's essential that we only send the handshake SYN inside
                -- the handler, because at this point the nonce is guaranteed
                -- to be known in the node state. If we sent the handhsake
                -- before 'spawnHandler' we risk (although it's highly unlikely)
                -- receiving the ACK before the nonce is put into the state.
                -- This isn't so unlikely in the case of self-connections.
                outcome <- NT.send conn [controlHeaderBidirectionalSyn nonce]
                case outcome of
                    Left err -> throw err
                    Right _ -> action peerDataVar channel (ChannelOut conn)
            wait promise
    bracket (connectToPeer node nodeid)
            (\conn -> disconnectFromPeer node nodeid conn)
            action'

-- | Create, use, and tear down a unidirectional channel to a peer identified
--   by 'NodeId'.
withOutChannel
    :: ( Mockable Bracket m, Mockable Async m, Ord (ThreadId m)
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
    (promise, _) <- spawnHandler nodeState provenance $
        bracket (connectOutChannel node nodeid)
                (\(ChannelOut conn) -> disconnectFromPeer node nodeid conn)
                action
    wait promise

data OutboundConnectionState m =
      -- | A stable outbound connection has some positive number of established
      --   connections.
      Stable !(Maybe (ComingUp m)) !Int !(Maybe (GoingDown m)) !(PeerDataTransmission m)
      -- | Every connection is being brought down.
    | AllGoingDown !(GoingDown m)
      -- | Every connection is being brought up.
    | AllComingUp !(ComingUp m)

-- | The SharedExclusiveT will be filled when the last connection goes down.
data GoingDown m = GoingDown !Int !(SharedExclusiveT m ())

-- | The SharedExclusiveT will be filled when the first connection comes up.
data ComingUp m = ComingUp !Int !(SharedExclusiveT m ())

data PeerDataTransmission m =
      PeerDataToBeTransmitted
    | PeerDataInFlight !(SharedExclusiveT m (Maybe SomeException))
    | PeerDataTransmitted

disconnectFromPeer
    :: ( Mockable SharedExclusive m
       , Mockable SharedAtomic m
       , Mockable Bracket m
       , Mockable Throw m
       , WithLogger m
       )
    => Node packingType peerData m
    -> NodeId
    -> NT.Connection m
    -> m ()
disconnectFromPeer node@Node{nodeState} nodeid@(NodeId peer) conn =
    bracketWithException startClosing finishClosing (const (NT.close conn))

    where

    -- Update the OutboundConnectionState at this peer to no longer show
    -- this connection as going down, and fill the shared exclusive if it's
    -- the last to go down.
    finishClosing _ (_ :: Maybe SomeException) = do
        modifySharedAtomic nodeState $ \nodeState -> do
            let map = _nodeStateConnectedTo nodeState
            choice <- case Map.lookup peer map of

                Just (Stable comingUp established goingDown transmission)

                    | Just (GoingDown n excl) <- goingDown
                    , n == 1 -> do
                          putSharedExclusive excl ()
                          return . Just $ Stable comingUp established Nothing transmission

                    | Just (GoingDown n excl) <- goingDown
                    , n > 1 -> do
                          return . Just $ Stable comingUp established (Just (GoingDown (n - 1) excl)) transmission

                Just (AllGoingDown (GoingDown n excl))

                    | n == 1 -> do
                          putSharedExclusive excl ()
                          return Nothing

                    | otherwise -> do
                          return $ Just (AllGoingDown (GoingDown (n - 1) excl))

                _ -> throw (InternalError "finishClosing : impossible")

            let nodeState' = nodeState {
                      _nodeStateConnectedTo = Map.update (const choice) peer map
                    }
            return (nodeState', ())

    -- Update the OutboundConnectionState at this peer to show this connection
    -- as going down.
    startClosing = do
        canClose <- modifySharedAtomic nodeState $ \nodeState -> do
            let map = _nodeStateConnectedTo nodeState
            choice <- case Map.lookup peer map of
                Just (Stable comingUp established goingDown transmission)

                    | established > 1
                    , Just (GoingDown !n excl) <- goingDown ->
                          return . Right $ Stable comingUp (established - 1) (Just (GoingDown (n + 1) excl)) transmission

                    | established > 1
                    , Nothing <- goingDown -> do
                          excl <- newSharedExclusive
                          return . Right $ Stable comingUp (established - 1) (Just (GoingDown 1 excl)) transmission

                    | established == 1
                    , Nothing <- comingUp
                    , Just (GoingDown !n excl) <- goingDown ->
                          return . Right $ AllGoingDown (GoingDown (n + 1) excl)

                    | established == 1
                    , Nothing <- comingUp
                    , Nothing <- goingDown -> do
                          excl <- newSharedExclusive
                          return . Right $ AllGoingDown (GoingDown 1 excl)

                    | established == 1
                    , Just (ComingUp !m excl) <- comingUp ->
                          return . Left $ excl

                    | otherwise -> throw (InternalError "startClosing : impossible")

                Nothing -> throw (InternalError "startClosing : impossible")
                Just (AllGoingDown _) -> throw (InternalError "startClosing : impossible")
                Just (AllComingUp _) -> throw (InternalError "startClosing : impossible")

            case choice of
                Left excl -> return (nodeState, Left excl)
                Right ocs -> return (nodeState', Right ())
                    where
                    nodeState' = nodeState {
                          _nodeStateConnectedTo = Map.insert peer ocs map
                        }

        case canClose of
            Left excl -> do
                readSharedExclusive excl
                startClosing
            Right () -> return ()

-- | Connect to a peer, taking care to send the peer-data in case there are no
--   other connections to that peer. Subsequent connections to that peer
--   will block until the peer-data is sent; it must be the first thing to
--   arrive when the first lightweight connection to a peer is opened.
--
--   A use of `connectToPeer` must be followed by `disconnectFromPeer`, in order
--   to keep the node state consistent. Please use safe exceptional handling
--   functions like `bracket` to make this guarantee.
connectToPeer
    :: ( Mockable Throw m
       , Mockable Bracket m
       , Mockable SharedAtomic m
       , Mockable SharedExclusive m
       , Message.Packable packingType peerData
       , WithLogger m
       )
    => Node packingType peerData m
    -> NodeId
    -> m (NT.Connection m)
connectToPeer node@Node{nodeEndPoint, nodeState, nodePackingType, nodePeerData} nodeid@(NodeId peer) = do
    conn <- establish
    sendPeerDataIfNecessary conn
    return conn

    where

    sendPeerDataIfNecessary conn =
        bracketWithException getPeerDataResponsibility
                             dischargePeerDataResponsibility
                             (maybeSendPeerData conn)

    maybeSendPeerData conn responsibility = case responsibility of
        -- Somebody else sent it, so we can proceed.
        False -> return ()
        -- We are responsible for sending it.
        True -> sendPeerData conn

    sendPeerData conn = do
        let serializedPeerData = Message.packMsg nodePackingType nodePeerData
        outcome <- NT.send conn (LBS.toChunks serializedPeerData)
        case outcome of
            Left err -> do
                throw err
            Right () -> do
                return ()

    getPeerDataResponsibility = do
        responsibility <- modifySharedAtomic nodeState $ \nodeState -> do
            let map = _nodeStateConnectedTo nodeState
            (ocs, responsibility) <- case Map.lookup peer map of
                Just it@(Stable comingUp established goingDown transmission)
                    | PeerDataToBeTransmitted <- transmission -> do
                          excl <- newSharedExclusive
                          return (Stable comingUp established goingDown (PeerDataInFlight excl), Just (Right excl))

                    | PeerDataInFlight excl <- transmission ->
                          return (it, Just (Left excl))

                    | PeerDataTransmitted <- transmission ->
                          return (it, Nothing)

                    | otherwise -> throw (InternalError "impossible")

            let nodeState' = nodeState {
                      _nodeStateConnectedTo = Map.insert peer ocs map
                    }
            return (nodeState', responsibility)
        case responsibility of
            Just (Left excl) -> do
                readSharedExclusive excl
                getPeerDataResponsibility
            Just (Right _) -> do
                return True
            Nothing -> do
                return False

    dischargePeerDataResponsibility responsibility (merr :: Maybe SomeException) = do
        modifySharedAtomic nodeState $ \nodeState -> do
            let map = _nodeStateConnectedTo nodeState
            ocs <- case Map.lookup peer map of
                Just it@(Stable comingUp established goingDown transmission)
                    -- We were responsible for sending it and we succeeded.
                    | True <- responsibility
                    , Nothing <- merr
                    , PeerDataInFlight excl <- transmission -> do
                          putSharedExclusive excl Nothing
                          return $ Stable comingUp established goingDown PeerDataTransmitted
                    | True <- responsibility
                    , Just _ <- merr
                    , PeerDataInFlight excl <- transmission -> do
                          putSharedExclusive excl merr
                          return $ Stable comingUp established goingDown PeerDataToBeTransmitted

                    | False <- responsibility -> return it
            let nodeState' = nodeState {
                      _nodeStateConnectedTo = Map.insert peer ocs map
                    }
            return (nodeState', ())

    establish = bracketWithException startConnecting finishConnecting doConnection

    doConnection _ = do
        mconn <- NT.connect nodeEndPoint
                           peer
                           NT.ReliableOrdered
                           -- TODO give a timeout. Can't rely on it being set at
                           -- the transport level.
                           NT.ConnectHints{ connectTimeout = Nothing }

        case mconn of
            -- Throwing the error will induce the bracket resource releaser
            Left err -> throw err
            Right conn -> return conn

    -- Update the OutboundConnectionState at this peer to no longer show
    -- this connection as coming up, and fill the shared exclusive if it's
    -- the first to come up.
    finishConnecting _ (merr :: Maybe SomeException) = do
        modifySharedAtomic nodeState $ \nodeState -> do
            when (_nodeStateClosed nodeState) (throw $ InternalError "connectToPeer : node closed while establishing connection!")
            let map = _nodeStateConnectedTo nodeState
            choice <- case Map.lookup peer map of

                Just (AllComingUp (ComingUp n excl))
                    | Nothing <- merr -> do
                          let comingUp = case n of
                                  1 -> Nothing
                                  _ -> Just (ComingUp (n - 1) excl)
                          return . Just $ Stable comingUp 1 Nothing PeerDataToBeTransmitted

                    | Just _ <- merr
                    , n == 1 ->
                          return Nothing

                    | Just _ <- merr
                    , n > 1 ->
                          return . Just $ AllComingUp (ComingUp (n - 1) excl)


                Just (Stable comingUp established goingDown transmission)
                    | Just (ComingUp n excl) <- comingUp -> do
                          putSharedExclusive excl ()
                          comingUp' <- case n of
                              1 -> return Nothing
                              _ -> do
                                  excl' <- newSharedExclusive
                                  return $ Just (ComingUp (n - 1) excl')
                          let established' = case merr of
                                  Nothing -> established + 1
                                  Just _ -> established
                          return . Just $ Stable comingUp' established' goingDown transmission

                _ -> throw (InternalError "finishConnecting : impossible")

            let nodeState' = nodeState {
                      _nodeStateConnectedTo = Map.update (const choice) peer map
                    }
            return (nodeState', ())


    -- Update the OutboundConnectionState at this peer to show this connection
    -- as going up.
    startConnecting = do
        canOpen <- modifySharedAtomic nodeState $ \nodeState -> do
            when (_nodeStateClosed nodeState) (throw $ userError "connectToPeer : you're doing it wrong! Our node is closed!")
            let map = _nodeStateConnectedTo nodeState
            choice <- case Map.lookup peer map of

                -- First to connect.
                Nothing -> do
                    excl <- newSharedExclusive
                    return . Right $ AllComingUp (ComingUp 1 excl)

                -- Stable connection. There's at least one that isn't currently
                -- going down.
                Just (Stable comingUp established goingDown transmission)

                    | Just (ComingUp n excl) <- comingUp ->
                          return . Right $ Stable (Just (ComingUp (n + 1) excl)) established goingDown transmission

                    | Nothing <- comingUp -> do
                          excl <- newSharedExclusive
                          return . Right $ Stable (Just (ComingUp 1 excl)) established goingDown transmission

                Just (AllGoingDown (GoingDown _ excl)) ->
                    return . Left $ excl

                Just (AllComingUp (ComingUp n excl)) ->
                    return . Right $ AllComingUp (ComingUp (n + 1) excl)

            case choice of
                Left excl -> return (nodeState, Left excl)
                Right ocs -> return (nodeState', Right ())
                    where
                    nodeState' = nodeState {
                          _nodeStateConnectedTo = Map.insert peer ocs map
                        }

        case canOpen of
            Left excl -> do
                readSharedExclusive excl
                startConnecting
            Right () -> return ()

-- | Connect to a peer given by a 'NodeId' unidirectionally.
connectOutChannel
    :: ( Mockable Throw m
       , Mockable Bracket m
       , Mockable SharedAtomic m
       , Mockable SharedExclusive m
       , Message.Packable packingType peerData
       , WithLogger m
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
