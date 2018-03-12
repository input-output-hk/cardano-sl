{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Node.Internal (
    NodeId(..),
    Node(..),
    NodeEnvironment(..),
    defaultNodeEnvironment,
    NodeEndPoint(..),
    simpleNodeEndPoint,
    manualNodeEndPoint,
    ReceiveDelay,
    noReceiveDelay,
    constantReceiveDelay,
    NodeState(..),
    nodeId,
    nodeEndPointAddress,
    Statistics(..),
    stTotalLiveBytes,
    stRunningHandlersRemoteVariance,
    stRunningHandlersLocalVariance,
    PeerStatistics(..),
    nodeStatistics,
    ChannelIn(..),
    ChannelOut(..),
    startNode,
    stopNode,
    withInOutChannel,
    writeMany,
    Timeout(..)
  ) where

import           Control.Exception.Safe (Exception, MonadCatch, MonadMask, MonadThrow,
                                         SomeException, bracket, catch, finally, throwM)
import           Control.Monad (forM, forM_, when)
import qualified Control.Monad.Catch as UnsafeExc
import           Control.Monad.Fix (MonadFix)
import           Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (foldl', foldlM)
import           Data.Hashable (Hashable)
import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.NonEmptySet (NonEmptySet)
import qualified Data.NonEmptySet as NESet
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Units (Microsecond)
import           Formatting (sformat, shown, (%))
import           GHC.Generics (Generic)
import qualified Mockable.Channel as Channel
import           Mockable.Class
import           Mockable.Concurrent
import           Mockable.CurrentTime (CurrentTime, currentTime)
import qualified Mockable.Metrics as Metrics
import           Mockable.SharedAtomic
import           Mockable.SharedExclusive
import qualified Network.Transport as NT (EventErrorCode (..))
import qualified Network.Transport.Abstract as NT
import           Node.Message.Class (Packing, Serializable (..), pack, unpack)
import           Node.Message.Decoder (Decoder (..), DecoderStep (..), continueDecoding)
import           System.Random (Random, StdGen, random)
import           System.Wlog (WithLogger, logDebug, logError, logWarning)

-- | A 'NodeId' wraps a network-transport endpoint address
newtype NodeId = NodeId NT.EndPointAddress
  deriving (Eq, Ord, Show, Hashable, Generic)

instance Binary NodeId

-- | The state of a Node, to be held in a shared atomic cell because other
--   threads will mutate it in order to set up bidirectional connections.
data NodeState peerData m = NodeState {
      _nodeStateGen                    :: !StdGen
      -- ^ To generate nonces.
    , _nodeStateOutboundBidirectional  :: !(Map NT.EndPointAddress (Map Nonce (SomeHandler m, Maybe BS.ByteString -> m (), Int -> m (), SharedExclusiveT m peerData, NT.ConnectionBundle, Promise m (), Bool)))
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


-- | An exception which is thrown when something times out.
data Timeout = Timeout
  deriving (Show)

instance Exception Timeout

-- | The initial state of a node, wrapped up in a shared atomic.
initialNodeState
    :: ( Mockable Metrics.Metrics m, Mockable SharedAtomic m )
    => StdGen
    -> m (SharedAtomicT m (NodeState peerData m))
initialNodeState prng = do
    !stats <- initialStatistics
    let nodeState = NodeState {
              _nodeStateGen = prng
            , _nodeStateOutboundBidirectional = Map.empty
            , _nodeStateInbound = Set.empty
            , _nodeStateConnectedTo = Map.empty
            , _nodeStateStatistics = stats
            , _nodeStateClosed = False
            }
    newSharedAtomic nodeState

data SomeHandler m = forall t . SomeHandler {
      someHandlerThreadId :: !(ThreadId m)
    , _someHandlerPromise :: !(Promise m t)
    }

-- | Correctness relies on the assumption that the ThreadId is that of the
--   Promise, and that two Promises with the same ThreadId are the same.
--   Is this reasonable?
instance (Eq (ThreadId m)) => Eq (SomeHandler m) where
    SomeHandler tid1 _ == SomeHandler tid2 _ = tid1 == tid2

instance (Ord (ThreadId m)) => Ord (SomeHandler m) where
    SomeHandler tid1 _ `compare` SomeHandler tid2 _ = tid1 `compare` tid2

waitSomeHandler :: ( Mockable LowLevelAsync m ) => SomeHandler m -> m ()
waitSomeHandler (SomeHandler _ promise) = () <$ wait promise

makeSomeHandler :: ( Mockable Async m ) => Promise m t -> m (SomeHandler m)
makeSomeHandler promise = do
    tid <- asyncThreadId promise
    return $ SomeHandler tid promise

data NodeEnvironment (m :: * -> *) = NodeEnvironment {
      nodeAckTimeout :: !Microsecond
      -- | Maximum transmission unit: how many bytes can be sent in a single
      --   network-transport send. Tune this according to the transport
      --   which backs the time-warp node.
    , nodeMtu        :: !Word32
    }

defaultNodeEnvironment :: NodeEnvironment m
defaultNodeEnvironment = NodeEnvironment {
      -- 30 second timeout waiting for an ACK.
      nodeAckTimeout = 30000000
    , nodeMtu        = maxBound
    }

-- | Computation in m of a delay (or no delay).
type ReceiveDelay m = m (Maybe Microsecond)

noReceiveDelay :: Applicative m => ReceiveDelay m
noReceiveDelay = pure Nothing

constantReceiveDelay :: Applicative m => Microsecond -> ReceiveDelay m
constantReceiveDelay = pure . Just

-- | A 'Node' is a network-transport 'EndPoint' with bidirectional connection
--   state and a thread to dispatch network-transport events.
data Node packingType peerData (m :: * -> *) = Node {
       nodeEndPoint         :: NT.EndPoint m
     , nodeCloseEndPoint    :: m ()
     , nodeDispatcherThread :: Promise m ()
     , nodeEnvironment      :: NodeEnvironment m
     , nodeState            :: SharedAtomicT m (NodeState peerData m)
     , nodePacking          :: Packing packingType m
     , nodePeerData         :: peerData
       -- | How long to wait before dequeueing an event from the
       --   network-transport receive queue, where Nothing means
       --   instantaneous (different from a 0 delay).
       --   The term is evaluated once for each dequeued event, immediately
       --   before dequeueing it.
     , nodeReceiveDelay     :: ReceiveDelay m
       -- | As 'nodeReceiveDelay' but instead of a delay on every network
       --   level message, the delay applies only to establishing new
       --   incomming connections. These connect/talk/close patterns tend
       --   to correspond to application level messages or conversations
       --   so this is a way to delay per-high-level message rather than
       --   lower level events.
     , nodeConnectDelay     :: ReceiveDelay m
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
  deriving (Show)

instance Exception NodeException

-- | Input from the wire.
newtype ChannelIn m = ChannelIn (Channel.ChannelT m (Maybe BS.ByteString))

-- | Output to the wire.
newtype ChannelOut m = ChannelOut (NT.Connection m)

-- | Do multiple sends on a 'ChannelOut'.
writeMany
    :: forall m .
       ( Monad m, MonadThrow m )
    => Word32 -- ^ Split into chunks of at most this size in bytes. 0 means no split.
    -> ChannelOut m
    -> LBS.ByteString
    -> m ()
writeMany mtu (ChannelOut conn) bss = mapM_ sendUnit units
  where
    sendUnit :: [BS.ByteString] -> m ()
    sendUnit unit = NT.send conn unit >>= either throwM pure
    units :: [[BS.ByteString]]
    units = fmap LBS.toChunks (chop bss)
    chop :: LBS.ByteString -> [LBS.ByteString]
    chop lbs
        | mtu == 0     = [lbs]
        -- Non-recursive definition for the case when the input is empty, so
        -- that
        --   writeMany mtu outChan ""
        -- still induces a send. Without this case, the list would be empty.
        | LBS.null lbs = [lbs]
        | otherwise    =
              let mtuInt :: Int64
                  mtuInt = fromIntegral mtu
                  chopItUp lbs | LBS.null lbs = []
                               | otherwise =
                                     let (front, back) = LBS.splitAt mtuInt lbs
                                     in  front : chopItUp back
              in  chopItUp lbs

-- | Statistics concerning traffic at this node.
data Statistics m = Statistics {
      -- | How many handlers are running right now in response to a
      --   remotely initiated connection (whether unidirectional or
      --   bidirectional).
      --   NB a handler may run longer or shorter than the duration of a
      --   connection.
      stRunningHandlersRemote         :: !(Metrics.Gauge m)
      -- | How many handlers are running right now which were initiated
      --   locally, i.e. corresponding to bidirectional connections.
    , stRunningHandlersLocal          :: !(Metrics.Gauge m)
      -- | Statistics for each peer.
    , stPeerStatistics                :: !(Map NT.EndPointAddress (SharedAtomicT m PeerStatistics))
      -- | How many peers are connected.
    , stPeers                         :: !(Metrics.Gauge m)
      -- | Average number of remotely-initiated handlers per peer.
      --   Also track the average of the number of handlers squared, so we
      --   can quickly compute the variance.
    , stRunningHandlersRemoteAverage  :: !(Double, Double)
      -- | Average number of locally-initiated handlers per peer.
      --   Also track the average of the number of handlers squared, so we
      --   can quickly compute the variance.
    , stRunningHandlersLocalAverage   :: !(Double, Double)
      -- | Handlers which finished normally. Distribution is on their
      --   running time.
    , stHandlersFinishedNormally      :: !(Metrics.Distribution m)
      -- | Handlers which finished exceptionally. Distribution is on their
      --   running time.
    , stHandlersFinishedExceptionally :: !(Metrics.Distribution m)
    }

stTotalLiveBytes
    :: (Mockable SharedAtomic m)
    => Statistics m -> m Int
stTotalLiveBytes stats = do
    allPeers <- mapM readSharedAtomic $ Map.elems (stPeerStatistics stats)
    let allBytes = fmap pstLiveBytes allPeers
    return $ sum allBytes

stRunningHandlersRemoteVariance :: Statistics m -> Double
stRunningHandlersRemoteVariance statistics = avg2 - (avg*avg)
    where
    (avg, avg2) = stRunningHandlersRemoteAverage statistics

stRunningHandlersLocalVariance :: Statistics m -> Double
stRunningHandlersLocalVariance statistics = avg2 - (avg*avg)
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
    , pstRunningHandlersLocal  :: !Int
      -- | How many bytes have been received by running handlers for this
      --   peer.
    , pstLiveBytes             :: !Int
    }

pstNull :: PeerStatistics -> Bool
pstNull PeerStatistics{..} =
    let remote = pstRunningHandlersRemote
        local = pstRunningHandlersLocal
    in  remote == 0 && local == 0

stIncrBytes
    :: (Mockable SharedAtomic m)
    => NT.EndPointAddress -> Int -> Statistics m -> m ()
stIncrBytes peer bytes stats =
    case Map.lookup peer (stPeerStatistics stats) of
      Nothing -> return ()
      Just peerStats -> modifySharedAtomic peerStats $ \ps ->
          let !ps' = pstIncrBytes bytes ps
          in  return (ps', ())

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

    Remote peer _ -> case Map.lookup peer map of
        Nothing ->
            newSharedAtomic (PeerStatistics 1 0 0) >>= \peerStatistics ->
            return (Map.insert peer peerStatistics map, True)
        Just !statsVar -> modifySharedAtomic statsVar $ \stats ->
            let !stats' = stats { pstRunningHandlersRemote = pstRunningHandlersRemote stats + 1 }
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

    Remote peer _ -> case Map.lookup peer map of
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
    return Statistics {
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
      -- | Initiated locally, _to_ this peer.
      Local !NT.EndPointAddress (Nonce, SharedExclusiveT m peerData, NT.ConnectionBundle, Promise m (), t)
      -- | Initiated remotely, _by_ or _from_ this peer.
    | Remote !NT.EndPointAddress !NT.ConnectionId

instance Show (HandlerProvenance peerData m t) where
    show prov = case prov of
        Local addr mdata -> concat [
              "Local "
            , show addr
            , show ((\(x,_,_,_,_) -> x) $ mdata)
            ]
        Remote addr connid -> concat ["Remote ", show addr, show connid]

handlerProvenancePeer :: HandlerProvenance peerData m t -> NT.EndPointAddress
handlerProvenancePeer provenance = case provenance of
    Local peer _  -> peer
    Remote peer _ -> peer

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
    Local !_peer _ -> do
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

    Remote !_peer _ -> do
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
    Local !_peer _ -> do
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

    Remote !_peer _ -> do
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

-- | How to create and close an 'EndPoint'.
--   See 'simpleNodeEndPoint' for a very obvious example.
--   More complicated things are possible, for instance using concrete
--   transport specific features.
data NodeEndPoint m = NodeEndPoint {
      newNodeEndPoint   :: m (Either (NT.TransportError NT.NewEndPointErrorCode) (NT.EndPoint m))
    , closeNodeEndPoint :: NT.EndPoint m -> m ()
    }

-- | A 'NodeEndPoint' which uses the typical network-transport 'newEndPoint'
--   and 'closeEndPoint'.
simpleNodeEndPoint :: NT.Transport m -> NodeEndPoint m
simpleNodeEndPoint transport = NodeEndPoint {
      newNodeEndPoint = NT.newEndPoint transport
    , closeNodeEndPoint = NT.closeEndPoint
    }

-- | Use an existing 'EndPoint'. It will be closed automatically when the node
--   stops, so do not close it yourself.
manualNodeEndPoint :: Applicative m => NT.EndPoint m -> NodeEndPoint m
manualNodeEndPoint ep = NodeEndPoint {
      newNodeEndPoint = pure $ Right ep
    , closeNodeEndPoint = NT.closeEndPoint
    }

-- | Bring up a 'Node' using a network transport.
startNode
    :: forall packingType peerData m .
       ( Mockable SharedAtomic m, Mockable Channel.Channel m
       , MonadMask m
       , Mockable LowLevelAsync m, Mockable Async m
       , Ord (ThreadId m), Show (ThreadId m)
       , Mockable CurrentTime m, Mockable Metrics.Metrics m
       , Mockable SharedExclusive m
       , Mockable Delay m
       , Serializable packingType peerData
       , MonadFix m, WithLogger m )
    => Packing packingType m
    -> peerData
    -> (Node packingType peerData m -> NodeEndPoint m)
    -> (Node packingType peerData m -> ReceiveDelay m)
    -- ^ Use the node (lazily) to determine a delay in microseconds to wait
    --   before dequeueing the next network-transport event (see
    --   'nodeReceiveDelay').
    -> (Node packingType peerData m -> ReceiveDelay m)
    -- ^ See 'nodeConnectDelay'
    -> StdGen
    -- ^ A source of randomness, for generating nonces.
    -> NodeEnvironment m
    -> (peerData -> NodeId -> ChannelIn m -> ChannelOut m -> m ())
    -- ^ Handle incoming bidirectional connections.
    -> m (Node packingType peerData m)
startNode packing peerData mkNodeEndPoint mkReceiveDelay mkConnectDelay
          prng nodeEnv handlerInOut = do
    rec { let nodeEndPoint = mkNodeEndPoint node
        ; mEndPoint <- newNodeEndPoint nodeEndPoint
        ; let receiveDelay = mkReceiveDelay node
              connectDelay = mkConnectDelay node
        ; node <- case mEndPoint of
              Left err -> throwM err
              Right endPoint -> do
                  sharedState <- initialNodeState prng
                  -- TODO this thread should get exceptions from the dispatcher thread.
                  rec { let node = Node {
                                  nodeEndPoint         = endPoint
                                , nodeCloseEndPoint    = closeNodeEndPoint nodeEndPoint endPoint
                                , nodeDispatcherThread = dispatcherThread
                                , nodeEnvironment      = nodeEnv
                                , nodeState            = sharedState
                                , nodePacking          = packing
                                , nodePeerData         = peerData
                                , nodeReceiveDelay     = receiveDelay
                                , nodeConnectDelay     = connectDelay
                                }
                      ; dispatcherThread <- async $
                            nodeDispatcher node handlerInOut
                      -- Exceptions in the dispatcher are re-thrown here.
                      ; link dispatcherThread
                      }
                  return node
        }
    logDebug $ sformat ("startNode, we are " % shown % "") (nodeId node)
    return node

-- | Stop a 'Node', closing its network transport and end point.
stopNode
    :: ( WithLogger m, MonadThrow m, Mockable LowLevelAsync m, Mockable SharedAtomic m )
    => Node packingType peerData m
    -> m ()
stopNode Node {..} = do
    modifySharedAtomic nodeState $ \nodeState ->
        if _nodeStateClosed nodeState
        then throwM $ userError "stopNode : already stopped"
        else pure (nodeState { _nodeStateClosed = True }, ())
    -- This eventually will shut down the dispatcher thread, which in turn
    -- ought to stop the connection handling threads.
    -- It'll also close all TCP connections.
    nodeCloseEndPoint
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
    | FeedingApplicationHandler !(Maybe BS.ByteString -> m ()) (Int -> m ())

instance Show (ConnectionState peerData m) where
    show term = case term of
        WaitingForPeerData            -> "WaitingForPeerData"
        PeerDataParseFailure          -> "PeerDataParseFailure"
        WaitingForHandshake _ _       -> "WaitingForHandshake"
        HandshakeFailure              -> "HandshakeFailure"
        FeedingApplicationHandler _ _ -> "FeedingApplicationHandler"

data PeerState peerData m =

      -- | Peer data is expected from one of these lightweight connections.
      --   If the second component is 'Just', then there's a lightweight
      --   connection which has given a partial parse of the peer data.
      ExpectingPeerData
          !(NonEmptySet NT.ConnectionId)
          !(Maybe (NT.ConnectionId, Maybe BS.ByteString -> Decoder m peerData))

      -- | Peer data has been received and parsed.
    | GotPeerData !peerData !(NonEmptySet NT.ConnectionId)

instance Show (PeerState peerData m) where
    show term = case term of
        ExpectingPeerData peers mleader -> "ExpectingPeerData " ++ show peers ++ " " ++ show (fmap fst mleader)
        GotPeerData _ peers -> "GotPeerData " ++ show peers

data DispatcherState peerData m = DispatcherState {
      dsConnections :: Map NT.ConnectionId (NT.EndPointAddress, ConnectionState peerData m)
    , dsPeers       :: Map NT.EndPointAddress (PeerState peerData m)
    }

deriving instance Show (DispatcherState peerData m)

initialDispatcherState :: DispatcherState peerData m
initialDispatcherState = DispatcherState Map.empty Map.empty

-- | Wait for every running handler in a node's state to finish. Exceptions are
--   caught and gathered, not re-thrown.
waitForRunningHandlers
    :: forall m packingType peerData .
       ( Mockable SharedAtomic m
       , Mockable LowLevelAsync m
       , MonadCatch m
       , WithLogger m
       , Show (ThreadId m)
       )
    => Node packingType peerData m
    -> m [Maybe SomeException]
waitForRunningHandlers node = do
    -- Gather the promises for all handlers.
    handlers <- withSharedAtomic (nodeState node) $ \st -> do
        let -- List monad computation: grab the values of the map (ignoring
            -- peer keys), then for each of those maps grab its values (ignoring
            -- nonce keys) and then return the promise.
            outbound_bi = do
                map <- Map.elems (_nodeStateOutboundBidirectional st)
                (x, _, _, _, _, _, _) <- Map.elems map
                return x
            inbound = Set.toList (_nodeStateInbound st)
            all = outbound_bi ++ inbound
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
       ( Mockable SharedAtomic m, Mockable Async m
       , Ord (ThreadId m), MonadMask m, Mockable SharedExclusive m
       , Mockable LowLevelAsync m
       , Mockable Channel.Channel m
       , Mockable CurrentTime m, Mockable Metrics.Metrics m
       , Mockable Delay m
       , Serializable packingType peerData
       , MonadFix m, WithLogger m, Show (ThreadId m) )
    => Node packingType peerData m
    -> (peerData -> NodeId -> ChannelIn m -> ChannelOut m -> m ())
    -> m ()
nodeDispatcher node handlerInOut =
    loop initialDispatcherState

    where

    nstate :: SharedAtomicT m (NodeState peerData m)
    nstate = nodeState node

    receiveDelay, connectDelay :: m ()
    receiveDelay = nodeReceiveDelay node >>= maybe (return ()) delay
    connectDelay = nodeConnectDelay node >>= maybe (return ()) delay

    endpoint = nodeEndPoint node

    loop :: DispatcherState peerData m -> m ()
    loop !state = do
      receiveDelay
      event <- NT.receive endpoint
      case event of

          NT.ConnectionOpened connid _reliability peer ->
              connectDelay >> connectionOpened state connid peer >>= loop

          NT.Received connid bytes -> received state connid bytes >>= loop

          NT.ConnectionClosed connid -> connectionClosed state connid >>= loop

          -- When the end point closes, we're done.
          NT.EndPointClosed -> endPointClosed state

          -- When a heavyweight connection is lost we must close up all of the
          -- lightweight connections which it carried.
          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode (NT.EventConnectionLost peer bundle)) reason) -> do
              logError $ sformat ("EventConnectionLost received from the network layer: " % shown) reason
              connectionLost state peer bundle >>= loop

          -- Unsupported event is recoverable. Just log and carry on.
          NT.ErrorEvent err@(NT.TransportError NT.UnsupportedEvent _) -> do
              logError $ sformat shown err
              loop state

          -- End point failure is unrecoverable.
          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode NT.EventEndPointFailed) reason) ->
              throwM (InternalError $ "EndPoint failed: " ++ reason)

          -- Transport failure is unrecoverable.
          NT.ErrorEvent (NT.TransportError (NT.EventErrorCode NT.EventTransportFailed) reason) ->
              throwM (InternalError $ "Transport failed " ++ reason)

    -- EndPointClosed is the final event that we will receive. There may be
    -- connections which remain open! ConnectionClosed events may be
    -- inbound but since our end point has closed, we won't take them. So here
    -- we have to plug every remaining input channel.
    endPointClosed
        :: DispatcherState peerData m
        -> m ()
    endPointClosed state = do
        let connections = Map.toList (dsConnections state)
        -- This is *not* a network-transport error; EndPointClosed can be
        -- posted without ConnectionClosed for all open connections, as an
        -- optimization.
        when (not (null connections)) $ do
            forM_ connections $ \(_, st) -> case st of
                (_, FeedingApplicationHandler dumpBytes _) -> do
                    dumpBytes Nothing
                _ -> return ()

        -- Must plug input channels for all un-acked outbound connections, and
        -- fill the peer data vars in case they haven't yet been filled. This
        -- is to ensure that handlers never block on these things.
        _ <- modifySharedAtomic nstate $ \st -> do
            let nonceMaps = Map.elems (_nodeStateOutboundBidirectional st)
            let outbounds = nonceMaps >>= Map.elems
            forM_ outbounds $ \(_, dumpBytes, _, peerDataVar, _, _, acked) -> do
                when (not acked) $ do
                   _ <- tryPutSharedExclusive peerDataVar (error "no peer data because local node has gone down")
                   dumpBytes Nothing
            return (st, ())

        _ <- waitForRunningHandlers node

        -- Check that this node was closed by a call to 'stopNode'. If it
        -- wasn't, we throw an exception. This is important because the thread
        -- which runs 'startNode' must *not* continue after the 'EndPoint' is
        -- closed.
        withSharedAtomic nstate $ \nodeState ->
            if _nodeStateClosed nodeState
            then pure ()
            else throwM (InternalError "EndPoint prematurely closed")

    connectionOpened
        :: DispatcherState peerData m
        -> NT.ConnectionId
        -> NT.EndPointAddress
        -> m (DispatcherState peerData m)
    connectionOpened state connid peer = case Map.lookup connid (dsConnections state) of

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
                    decoderStep :: DecoderStep m peerData <- runDecoder (unpack (nodePacking node))
                    decoderStep' <- continueDecoding decoderStep (BS.concat chunks)
                    case decoderStep' of
                        Fail _ _ err -> do
                            logWarning $ sformat ("failed to decode peer data from " % shown % ": got error " % shown) peer err
                            return $ state {
                                    dsConnections = Map.insert connid (peer, PeerDataParseFailure) (dsConnections state)
                                  }
                        Done trailing _ peerData -> do
                            let state' = state {
                                      dsConnections = foldl' (awaitHandshake peerData) (dsConnections state) (NESet.toList connids)
                                    , dsPeers = Map.insert peer (GotPeerData peerData connids) (dsPeers state)
                                    }
                            received state' connid [trailing]
                        Partial decoderContinuation -> do
                            return $ state {
                                    dsPeers = Map.insert peer (ExpectingPeerData connids (Just (connid, decoderContinuation))) (dsPeers state)
                                  }

                Just (connid', decoderContinuation) -> case connid == connid' of

                    -- Protocol error. We got data from some other lightweight
                    -- connection before the peer data was parsed.
                    False -> do
                        logWarning $ sformat ("peer data protocol error from " % shown) peer
                        return state

                    True -> do
                        decoderStep <- runDecoder (decoderContinuation (Just (BS.concat chunks)))
                        case decoderStep of
                            Fail _ _ err -> do
                                logWarning $ sformat ("failed to decode peer data from " % shown % ": got error " % shown) peer err
                                return $ state {
                                        dsConnections = Map.insert connid (peer, PeerDataParseFailure) (dsConnections state)
                                      }

                            Done trailing _ peerData -> do
                                let state' = state {
                                          dsConnections = foldl' (awaitHandshake peerData) (dsConnections state) (NESet.toList connids)
                                        , dsPeers = Map.insert peer (GotPeerData peerData connids) (dsPeers state)
                                        }
                                received state' connid [trailing]

                            Partial decoderContinuation' -> do
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
                    :: peerData
                    -> Map NT.ConnectionId (NT.EndPointAddress, ConnectionState peerData m)
                    -> NT.ConnectionId
                    -> Map NT.ConnectionId (NT.EndPointAddress, ConnectionState peerData m)
                awaitHandshake peerData map connid =

                    Map.update (\(peer, _) -> Just (peer, WaitingForHandshake peerData BS.empty)) connid map


            -- We're waiting for peer data on this connection, but we don't
            -- have an entry for the peer. That's an internal error.
            Nothing -> do
                throwM $ InternalError "node dispatcher inconsistent state (waiting for peer data)"

            Just (GotPeerData _ _) -> do
                throwM $ InternalError "node dispatcher inconsistent state (already got peer data)"

        -- Waiting for a handshake. Try to get a control header and then
        -- move on.
        Just (peer, WaitingForHandshake peerData partial) -> do
            let bytes = BS.append partial (BS.concat chunks)
            case BS.uncons bytes of

                Nothing -> return state

                Just (w, ws)

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
                          chanVar <- newSharedAtomic (Just channel)
                          let dumpBytes mBytes = withSharedAtomic chanVar $
                                  maybe (return ()) (flip Channel.writeChannel mBytes)
                              provenance = Remote peer connid
                              respondAndHandle conn = do
                                  outcome <- NT.send conn [controlHeaderBidirectionalAck nonce]
                                  case outcome of
                                      Left err -> throwM err
                                      Right () -> do
                                          handlerInOut peerData (NodeId peer) (ChannelIn channel) (ChannelOut conn)
                          -- Resource releaser for bracketWithException.
                          -- No matter what, we must update the node state to
                          -- indicate that we've disconnected from the peer.
                              cleanup (me :: Maybe SomeException) = do
                                  modifySharedAtomic chanVar $ \_ -> return (Nothing, ())
                                  case me of
                                      Nothing -> return ()
                                      Just e -> logError $
                                          sformat (shown % " error in conversation response " % shown) nonce e
                              handler = bracketWithException
                                  (return ())
                                  (const cleanup)
                                  (const (connectToPeer node (NodeId peer) respondAndHandle))
                          -- Establish the other direction in a separate thread.
                          (_, incrBytes) <- spawnHandler nstate provenance handler
                          let bs = LBS.toStrict ws'
                          dumpBytes $ Just bs
                          incrBytes $ fromIntegral (BS.length bs)
                          return $ state {
                                dsConnections = Map.insert connid (peer, FeedingApplicationHandler dumpBytes incrBytes) (dsConnections state)
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
                                  Just (_, _, _, _, _, _, True) -> return (st, Just Nothing)
                                  Just (promise, dumpBytes, incrBytes, peerDataVar, connBundle, timeoutPromise, False) -> do
                                      cancel timeoutPromise
                                      return
                                          ( st { _nodeStateOutboundBidirectional = Map.update updater peer (_nodeStateOutboundBidirectional st)
                                               }
                                          , Just (Just (dumpBytes, incrBytes, peerDataVar))
                                          )
                                      where
                                      updater map = Just $ Map.insert nonce (promise, dumpBytes, incrBytes, peerDataVar, connBundle, timeoutPromise, True) map
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
                              Just (Just (dumpBytes, incrBytes, peerDataVar)) -> do
                                  putSharedExclusive peerDataVar peerData
                                  let bs = LBS.toStrict ws'
                                  dumpBytes $ Just bs
                                  incrBytes $ fromIntegral (BS.length bs)
                                  return $ state {
                                        dsConnections = Map.insert connid (peer, FeedingApplicationHandler dumpBytes incrBytes) (dsConnections state)
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
        Just (_peer, FeedingApplicationHandler dumpBytes incrBytes) -> do
            let bs = LBS.toStrict (LBS.fromChunks chunks)
            dumpBytes $ Just bs
            incrBytes $ BS.length bs
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
                FeedingApplicationHandler dumpBytes _ -> do
                    -- Signal end of channel.
                    dumpBytes Nothing
                _ -> return ()
            -- This connection can be removed from the connection states map.
            -- Removing it from the peers map is more involved.
            let peersUpdater existing = case existing of
                    GotPeerData peerData neset -> case NESet.delete connid neset of
                        Nothing     -> Nothing
                        Just neset' -> Just (GotPeerData peerData neset')
                    ExpectingPeerData neset mleader -> case NESet.delete connid neset of
                        Nothing -> Nothing
                        Just neset' -> case mleader of
                            Nothing -> Just (ExpectingPeerData neset' mleader)
                            Just (connid', _partialDecoder) -> case connid == connid' of
                                -- The connection which is giving the peer data
                                -- has closed! That's ok, just forget about it
                                -- and the partial decode of that data.
                                True  -> Just (ExpectingPeerData neset' Nothing)
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
                -- This is *not* a network-transport bug; a connection lost
                -- event can be posted without ConnectionClosed, as an
                -- optimization.
                let connids = case it of
                        GotPeerData _ neset       -> NESet.toList neset
                        ExpectingPeerData neset _ -> NESet.toList neset
                -- For every connection to that peer we'll plug the channel with
                -- Nothing and remove it from the map.
                let folder :: Map NT.ConnectionId (NT.EndPointAddress, ConnectionState peerData m)
                           -> NT.ConnectionId
                           -> m (Map NT.ConnectionId (NT.EndPointAddress, ConnectionState peerData m))
                    folder channels connid = case Map.updateLookupWithKey (\_ _ -> Nothing) connid channels of
                        (Just (_, FeedingApplicationHandler dumpBytes _), channels') -> do

                            dumpBytes Nothing
                            return channels'
                        (_, channels') -> return channels'
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
                    let folder (_, channelIn, _, peerDataVar, bundle', _, acked) channels
                            | bundle' == bundle && not acked = (channelIn, peerDataVar) : channels
                            | otherwise = channels

                    let channelsAndPeerDataVars = Map.foldr folder [] map
                    return (st, channelsAndPeerDataVars)

        logWarning $ sformat ("closing " % shown % " channels on bundle " % shown % " to " % shown) (length channelsAndPeerDataVars) bundle peer

        forM_ channelsAndPeerDataVars $ \(dumpBytes, peerDataVar) -> do
            _ <- tryPutSharedExclusive peerDataVar (error "no peer data because the connection was lost")
            dumpBytes Nothing

        return state'

-- | Spawn a thread and track it in shared state, taking care to remove it from
--   shared state when it's finished and updating statistics appropriately.
--   This is applicable to handlers spawned in response to inbound peer
--   connections, and also for actions which use outbound connections.
spawnHandler
    :: forall peerData m t .
       ( Mockable SharedAtomic m, MonadCatch m
       , Mockable LowLevelAsync m, Mockable Async m, Ord (ThreadId m)
       , Mockable Metrics.Metrics m, Mockable CurrentTime m
       , WithLogger m
       , MonadFix m )
    => SharedAtomicT m (NodeState peerData m)
    -> HandlerProvenance peerData m (Maybe BS.ByteString -> m ())
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
                Remote _ _ -> nodeState {
                      _nodeStateInbound = Set.insert someHandler (_nodeStateInbound nodeState)
                    }
                Local peer (nonce, peerDataVar, connBundle, timeoutPromise, dumpBytes) -> nodeState {
                      _nodeStateOutboundBidirectional = Map.alter alteration peer (_nodeStateOutboundBidirectional nodeState)
                    }
                    where
                    alteration Nothing = Just $ Map.singleton nonce (someHandler, dumpBytes, incrBytes, peerDataVar, connBundle, timeoutPromise, False)
                    alteration (Just map) = Just $ Map.insert nonce (someHandler, dumpBytes, incrBytes, peerDataVar, connBundle, timeoutPromise, False) map

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
        throwM e

    signalFinished :: SomeHandler m -> Microsecond -> SharedAtomicT m Int -> Maybe SomeException -> m ()
    signalFinished someHandler startTime totalBytesVar outcome = do
        endTime <- currentTime
        let elapsed = endTime - startTime
        totalBytes <- readSharedAtomic totalBytesVar
        modifySharedAtomic stateVar $ \nodeState -> do
            let nodeState' = case provenance of
                    Remote _ _ -> nodeState {
                          _nodeStateInbound = Set.delete someHandler (_nodeStateInbound nodeState)
                        }
                    -- Remove the nonce for this peer, and remove the whole map
                    -- if this was the only nonce for that peer.
                    Local peer (nonce, _, _, _, _) -> nodeState {
                          _nodeStateOutboundBidirectional = Map.update updater peer (_nodeStateOutboundBidirectional nodeState)
                        }
                        where
                        updater map =
                            let map' = Map.delete nonce map
                            in  if Map.null map' then Nothing else Just map'
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

controlHeaderBidirectionalSyn :: Nonce -> BS.ByteString
controlHeaderBidirectionalSyn (Nonce nonce) =
    fixedSizeBuilder' 9 $
        BS.word8 controlHeaderCodeBidirectionalSyn
     <> BS.word64BE nonce

controlHeaderBidirectionalAck :: Nonce -> BS.ByteString
controlHeaderBidirectionalAck (Nonce nonce) =
    fixedSizeBuilder' 9 $
        BS.word8 controlHeaderCodeBidirectionalAck
     <> BS.word64BE nonce

fixedSizeBuilder' :: Int -> BS.Builder -> BS.ByteString
fixedSizeBuilder' n = LBS.toStrict . fixedSizeBuilder n

fixedSizeBuilder :: Int -> BS.Builder -> LBS.ByteString
fixedSizeBuilder n =
    BS.toLazyByteStringWith (BS.untrimmedStrategy n n) LBS.empty

-- | Create, use, and tear down a conversation channel with a given peer
--   (NodeId).
--   This may be killed with a 'Timeout' exception in case the peer does not
--   give an ACK before the specified timeout ('nodeAckTimeout').
withInOutChannel
    :: forall packingType peerData m a .
       ( MonadMask m, Mockable Async m, Ord (ThreadId m)
       , Mockable LowLevelAsync m
       , Mockable SharedAtomic m
       , Mockable SharedExclusive m
       , Mockable Channel.Channel m
       , Mockable CurrentTime m, Mockable Metrics.Metrics m
       , Mockable Delay m
       , MonadFix m, WithLogger m
       , Serializable packingType peerData )
    => Node packingType peerData m
    -> NodeId
    -> (peerData -> ChannelIn m -> ChannelOut m -> m a)
    -> m a
withInOutChannel node@Node{nodeEnvironment, nodeState} nodeid@(NodeId peer) action = do
    nonce <- modifySharedAtomic nodeState $ \nodeState -> do
               let (nonce, !prng') = random (_nodeStateGen nodeState)
               pure (nodeState { _nodeStateGen = prng' }, nonce)
    channel <- fmap ChannelIn Channel.newChannel
    -- A mutable cell for the channel. We'll swap it to Nothing when we don't
    -- want to accept any more bytes (the handler has finished).
    channelVar <- newSharedAtomic (Just channel)
    let dumpBytes mbs = withSharedAtomic channelVar $ \mchannel -> case mchannel of
            Nothing                  -> pure ()
            Just (ChannelIn channel) -> Channel.writeChannel channel mbs
        closeChannel = modifySharedAtomic channelVar $ \_ -> pure (Nothing, ())
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
            rec { let provenance = Local peer (nonce, peerDataVar, NT.bundle conn, timeoutPromise, dumpBytes)
                ; (promise, _) <- spawnHandler nodeState provenance $ do
                      -- It's essential that we only send the handshake SYN inside
                      -- the handler, because at this point the nonce is guaranteed
                      -- to be known in the node state. If we sent the handhsake
                      -- before 'spawnHandler' we risk (although it's highly unlikely)
                      -- receiving the ACK before the nonce is put into the state.
                      -- This isn't so unlikely in the case of self-connections.
                      outcome <- NT.send conn [controlHeaderBidirectionalSyn nonce]
                      case outcome of
                          Left err -> throwM err
                          Right _ -> do
                              peerData <- readSharedExclusive peerDataVar
                              action peerData channel (ChannelOut conn)
                  -- Here we spawn the timeout thread... Killing the 'promise'
                  -- is enough to clean everything up.
                  -- This timeout promise is included in the provenance, so
                  -- that when an ACK is received, the timeout thread can
                  -- be killed.
                ; timeoutPromise <- async $ do
                      delay (nodeAckTimeout nodeEnvironment)
                      cancelWith promise Timeout
                }
            wait promise
    connectToPeer node nodeid action' `finally` closeChannel

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
       , MonadMask m
       )
    => Node packingType peerData m
    -> NodeId
    -> NT.Connection m
    -> m ()
disconnectFromPeer Node{nodeState} (NodeId peer) conn =
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

                _ -> throwM (InternalError "finishClosing : impossible")

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
                    , Just (ComingUp !_m excl) <- comingUp ->
                          return . Left $ excl

                    | otherwise -> throwM (InternalError "startClosing : impossible")

                Nothing -> throwM (InternalError "startClosing : impossible")
                Just (AllGoingDown _) -> throwM (InternalError "startClosing : impossible")
                Just (AllComingUp _) -> throwM (InternalError "startClosing : impossible")

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
connectToPeer
    :: forall packingType peerData m r .
       ( MonadMask m
       , Mockable SharedAtomic m
       , Mockable SharedExclusive m
       , Serializable packingType peerData
       , WithLogger m
       )
    => Node packingType peerData m
    -> NodeId
    -> (NT.Connection m -> m r)
    -> m r
connectToPeer node@Node{nodeEndPoint, nodeState, nodePacking, nodePeerData, nodeEnvironment} nid@(NodeId peer) act =
    -- 'establish' will update shared state indicating the nature of
    -- connections to this peer: how many are coming up, going down, or
    -- established. It's essential to bracket that against 'disconnectFromPeer'
    -- so that if there's an exception when sending the peer data or when
    -- doing the 'act' continuation, the state is always brought back to
    -- consistency.
    bracket establish (disconnectFromPeer node nid) $ \conn -> do
        sendPeerDataIfNecessary conn
        act conn

    where

    mtu = nodeMtu nodeEnvironment

    sendPeerDataIfNecessary conn =
        bracketWithException getPeerDataResponsibility
                             dischargePeerDataResponsibility
                             (maybeSendPeerData conn)

    maybeSendPeerData conn responsibility = case responsibility of
        -- Somebody else sent it, so we can proceed.
        False -> return ()
        -- We are responsible for sending it.
        True  -> sendPeerData conn

    sendPeerData conn = do
        serializedPeerData <- pack nodePacking nodePeerData
        writeMany mtu (ChannelOut conn) serializedPeerData

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

                    | otherwise -> throwM (InternalError "impossible")
                _ -> do
                    logError "getPeerDataResponsibility: unexpected peer state"
                    throwM $ InternalError "connectToPeer: getPeerDataResponsibility: impossible"

            let nodeState' = nodeState {
                      _nodeStateConnectedTo = Map.insert peer ocs map
                    }
            return (nodeState', responsibility)
        case responsibility of
            Just (Left excl) -> do
                _ <- readSharedExclusive excl
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
                _ -> do
                    logError "dischargePeerDataResponsibility: unexpected peer state"
                    throwM $ InternalError "connectToPeer: dischargePeerDataResponsibility: impossible"

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
            Left err   -> throwM err
            Right conn -> return conn

    -- Update the OutboundConnectionState at this peer to no longer show
    -- this connection as coming up, and fill the shared exclusive if it's
    -- the first to come up.
    finishConnecting _ (merr :: Maybe SomeException) = do
        modifySharedAtomic nodeState $ \nodeState -> do
            when (_nodeStateClosed nodeState) (throwM $ InternalError "connectToPeer : node closed while establishing connection!")
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
                                  Just _  -> established
                          return . Just $ Stable comingUp' established' goingDown transmission

                _ -> throwM (InternalError "finishConnecting : impossible")

            let nodeState' = nodeState {
                      _nodeStateConnectedTo = Map.update (const choice) peer map
                    }
            return (nodeState', ())


    -- Update the OutboundConnectionState at this peer to show this connection
    -- as going up.
    startConnecting = do
        canOpen <- modifySharedAtomic nodeState $ \nodeState -> do
            when (_nodeStateClosed nodeState) (throwM $ userError "connectToPeer : you're doing it wrong! Our node is closed!")
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

-- FIXME: Remove this once https://github.com/fpco/safe-exceptions/pull/28 is merged.
bracketWithException
    :: ( MonadMask m, Exception e )
    => m r
    -> (r -> Maybe e -> m b)
    -> (r -> m c)
    -> m c
bracketWithException before after thing = UnsafeExc.mask $ \restore -> do
    x <- before
    res1 <- UnsafeExc.try $ restore (thing x)
    case res1 of
        Left (e1 :: SomeException) -> do
            _ :: Either SomeException b <-
                UnsafeExc.try $ UnsafeExc.uninterruptibleMask_ $ after x (UnsafeExc.fromException e1)
            UnsafeExc.throwM e1
        Right y -> do
            _ <- UnsafeExc.uninterruptibleMask_ $ after x Nothing
            return y
