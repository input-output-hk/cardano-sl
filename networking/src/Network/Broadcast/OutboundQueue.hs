{-------------------------------------------------------------------------------
  Outbound message queue

  Intended for qualified import

  > import Network.Broadcast.OutboundQ (OutboundQ)
  > import qualified Network.Broadcast.OutboundQ as OutQ
  > import Network.Broadcast.OutboundQueue.Types

  References:
  * https://issues.serokell.io/issue/CSL-1272
  * IERs_V2.md
-------------------------------------------------------------------------------}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Network.Broadcast.OutboundQueue (
    OutboundQ -- opaque
    -- * Initialization
  , new
    -- ** Enqueueing policy
  , Precedence(..)
  , MaxAhead(..)
  , Enqueue(..)
  , EnqueuePolicy
  , UnknownNodeType(..)
    -- ** Dequeueing policy
  , RateLimit(..)
  , MaxInFlight(..)
  , Dequeue(..)
  , DequeuePolicy
    -- ** Failure policy
  , FailurePolicy
  , ReconsiderAfter(..)
    -- ** Subscription
  , MaxBucketSize(..)
  , SpareCapacity(..)
  , bucketSpareCapacity
    -- * Enqueueing
  , Origin(..)
  , EnqueueTo (..)
  , enqueue
  , enqueueSync'
  , enqueueSync
  , enqueueCherished
  , clearRecentFailures
  , clearFailureOf
    -- * Dequeuing
  , SendMsg
  , dequeueThread
    -- ** Controlling the dequeuer
  , flush
  , waitShutdown
    -- * Peers
  , Peers(..)
  , AllOf
  , Alts
  , simplePeers
  , peersFromList
  , updatePeersBucket
    -- * Debugging
  , registerQueueMetrics
  , dumpState
  , currentlyInFlight
  ) where

import           Control.Concurrent
import           Control.Exception.Safe (MonadMask, SomeException, displayException, finally, mask_,
                                         try)
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Either (rights)
import           Data.Foldable (fold)
import           Data.List (intercalate, sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, maybeToList)
import           Data.Monoid ((<>))
import           Data.Ord (comparing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Typeable (typeOf)
import           Formatting (Format, sformat, shown, string, (%))
import qualified System.Metrics as Monitoring
import           System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as Counter
import           System.Wlog.CanLog (WithLogger, logDebug)
import qualified System.Wlog.CanLog as Log
import           System.Wlog.Severity (Severity (..))

import qualified Mockable as M
import           Network.Broadcast.OutboundQueue.ConcurrentMultiQueue (MultiQueue)
import qualified Network.Broadcast.OutboundQueue.ConcurrentMultiQueue as MQ
import           Network.Broadcast.OutboundQueue.Types

{-------------------------------------------------------------------------------
  Precedence levels
-------------------------------------------------------------------------------}

-- | Precedence levels
--
-- These precedence levels are not given meaningful names because the same kind
-- of message might be given different precedence levels on different kinds of
-- nodes. Meaning is given to these levels in the enqueueing policy.
data Precedence = PLowest | PLow | PMedium | PHigh | PHighest
  deriving (Show, Eq, Ord, Enum, Bounded)

enumPrecLowestFirst :: [Precedence]
enumPrecLowestFirst = [minBound .. maxBound]

enumPrecHighestFirst :: [Precedence]
enumPrecHighestFirst = reverse enumPrecLowestFirst

{-------------------------------------------------------------------------------
  Enqueueing policy

  The enquing policy is intended to guarantee that at the point of enqueing
  we can be reasonably sure that the message will get to where it needs to be
  within the maximum time bounds.
-------------------------------------------------------------------------------}

-- | Maximum number of messages allowed "ahead" of the message to be enqueued
--
-- This is the total number of messages currently in-flight or in-queue, with a
-- precedence at or above the message to be enqueued.
--
-- We can think of this as "all messages that will be handled before the new
-- message", although that is not /quite/ right: messages currently already
-- in-flight with a precedence lower than the new message are not included even
-- though they are also handled before the new message. We make this exception
-- because the presence of low precedence in-flight messages should not affect
-- the enqueueing policy for higher precedence messages.
--
-- If we cannot find any alternative that doesn't match requirements we simply
-- give up on forwarding set.
newtype MaxAhead = MaxAhead Int
  deriving Show

-- | Enqueueing instruction
data Enqueue =
    -- | For /all/ forwarding sets of the specified node type, chose /one/
    -- alternative to send the message to
    EnqueueAll {
        enqNodeType   :: NodeType
      , enqMaxAhead   :: MaxAhead
      , enqPrecedence :: Precedence
      }

    -- | Choose /one/ alternative of /one/ forwarding set of any of the
    -- specified node types (listed in order of preference)
  | EnqueueOne {
        enqNodeTypes  :: [NodeType]
      , enqMaxAhead   :: MaxAhead
      , enqPrecedence :: Precedence
      }
  deriving (Show)

-- | The enqueuing policy
--
-- The enqueueing policy decides what kind of peer to send each message to,
-- how to pick alternatives, and which precedence level to assign to the
-- message. However, it does NOT decide _how many_ alternatives to pick; we
-- pick one from _each_ of the lists that we are given. It is the responsiblity
-- of the next layer up to configure these peers as desired.
--
-- TODO: Sanity check the number of forwarding sets and number of alternatives.
type EnqueuePolicy nid =
           MsgType nid  -- ^ Type of the message we want to send
        -> [Enqueue]

{-------------------------------------------------------------------------------
  Dequeue policy
-------------------------------------------------------------------------------}

data Dequeue = Dequeue {
      -- | Delay before sending the next message (to this node)
      deqRateLimit   :: RateLimit

      -- | Maximum number of in-flight messages (to this node node)
    , deqMaxInFlight :: MaxInFlight
    }
  deriving (Show)

-- | Rate limiting
data RateLimit = NoRateLimiting | MaxMsgPerSec Int
  deriving (Show)

-- | Maximum number of in-flight messages (for latency hiding)
newtype MaxInFlight = MaxInFlight Int
  deriving (Show)

-- | Dequeue policy
--
-- The dequeue policy depends only on the type of the node we're sending to,
-- not the type of the message we're sending.
type DequeuePolicy = NodeType -> Dequeue

{-------------------------------------------------------------------------------
  Failure policy
-------------------------------------------------------------------------------}

-- | The failure policy determines what happens when a failure occurs as we send
-- a message to a particular node: how long (in sec) should we wait until we
-- consider this node to be a viable alternative again?
type FailurePolicy nid = NodeType -> MsgType nid -> SomeException -> ReconsiderAfter

-- | How long after a failure should we reconsider this node again?
newtype ReconsiderAfter = ReconsiderAfter NominalDiffTime

{-------------------------------------------------------------------------------
  Thin wrapper around ConcurrentMultiQueue
-------------------------------------------------------------------------------}

-- | The values we store in the multiqueue
data Packet msg nid a = Packet {
    -- | The actual payload of the message
    packetPayload  :: msg a

    -- | Type of the message
  , packetMsgType  :: MsgType nid

    -- | Type of the node the packet needs to be sent to
  , packetDestType :: NodeType

    -- | Node to send it to
  , packetDestId   :: nid

    -- | Precedence of the message
  , packetPrec     :: Precedence

    -- | MVar filled with the result of the sent action
    --
    -- (empty when enqueued)
  , packetSent     :: MVar (Either SomeException a)
  }

-- | Hide the 'a' type parameter
data EnqPacket msg nid = forall a. EnqPacket (Packet msg nid a)

-- | Lift functions on 'Packet' to 'EnqPacket'
liftEnq :: (forall a. Packet msg nid a -> b) -> EnqPacket msg nid -> b
liftEnq f (EnqPacket p) = f p

-- | The keys we use to index the multiqueue
data Key nid =
    -- | All messages with a certain precedence
    --
    -- Used when dequeuing to determine the next message to send
    KeyByPrec Precedence

    -- | All messages to a certain destination
    --
    -- Used when dequeing to determine max in-flight to a particular destination
    -- (for latency hiding)
  | KeyByDest nid

    -- | All messages with a certain precedence to a particular destination
    --
    -- Used when enqueuing to determine routing (enqueuing policy)
  | KeyByDestPrec nid Precedence
  deriving (Show, Eq, Ord)

-- | MultiQueue instantiated at the types we need
type MQ msg nid = MultiQueue (Key nid) (EnqPacket msg nid)

mqEnqueue :: (MonadIO m, Ord nid)
          => MQ msg nid -> EnqPacket msg nid -> m ()
mqEnqueue qs p = liftIO $
  MQ.enqueue qs [ KeyByDest     (liftEnq packetDestId p)
                , KeyByDestPrec (liftEnq packetDestId p) (liftEnq packetPrec p)
                , KeyByPrec                              (liftEnq packetPrec p)
                ]
                p

-- | Check whether a node is not currently busy
--
-- (i.e., number of in-flight messages is less than the max)
type NotBusy nid = NodeType -> nid -> Bool

mqDequeue :: forall m msg nid. (MonadIO m, Ord nid)
          => MQ msg nid -> NotBusy nid -> m (Maybe (EnqPacket msg nid))
mqDequeue qs notBusy =
    orElseM [
        liftIO $ MQ.dequeue (KeyByPrec prec) notBusy' qs
      | prec <- enumPrecHighestFirst
      ]
  where
    notBusy' :: EnqPacket msg nid -> Bool
    notBusy' (EnqPacket Packet{..}) = notBusy packetDestType packetDestId

{-------------------------------------------------------------------------------
  State Initialization
-------------------------------------------------------------------------------}

-- | How many messages are in-flight to each destination?
type InFlight nid = Map nid (Map Precedence Int)

-- | For each node, its most recent failure and how long we should wait before
-- trying again
type Failures nid = Map nid (UTCTime, ReconsiderAfter)

inFlightTo :: Ord nid => nid -> Getter (InFlight nid) (Map Precedence Int)
inFlightTo nid = at nid . anon Map.empty Map.null

inFlightWithPrec :: Ord nid => nid -> Precedence -> Getter (InFlight nid) Int
inFlightWithPrec nid prec = inFlightTo nid . at prec . anon 0 (== 0)

-- | Given an update function and a `Packet`, set the `InFlight` to the new value
-- calculated by `f`. In case no match can be found, this function is effectively a noop.
setInFlightFor :: forall m msg nid a. (MonadIO m, Ord nid)
               => Packet msg nid a
               -> (Int -> Int)
               -> MVar (InFlight nid)
               -> m ()
setInFlightFor Packet{..} f var = liftIO $ modifyMVar_ var (return . update)
  where
    update :: InFlight nid -> InFlight nid
    update = Map.adjust updateInnerMap packetDestId

    updateInnerMap :: Map Precedence Int -> Map Precedence Int
    updateInnerMap = Map.adjust f packetPrec

-- | The outbound queue (opaque data structure)
--
-- NOTE: The 'Ord' instance on the type of the buckets @buck@ determines the
-- final 'Peers' value that the queue gets every time it reads all buckets.
data OutboundQ msg nid buck = ( FormatMsg msg
                              , Ord nid
                              , Show nid
                              , Ord buck
                              , Show buck
                              ) => OutQ {
      -- | Node ID of the current node (primarily for debugging purposes)
      qSelf            :: String

      -- | Enqueuing policy
    , qEnqueuePolicy   :: EnqueuePolicy nid

      -- | Dequeueing policy
    , qDequeuePolicy   :: DequeuePolicy

      -- | Failure policy
    , qFailurePolicy   :: FailurePolicy nid

      -- | Maximum size of the buckets
    , qMaxBucketSize   :: buck -> MaxBucketSize

      -- | Assumed type of unknown nodes
    , qUnknownNodeType :: nid -> NodeType

      -- | Messages sent but not yet acknowledged
    , qInFlight        :: MVar (InFlight nid)

      -- | Nodes that we should not send any messages to right now because
      -- of rate limiting
    , qRateLimited     :: MVar (Set nid)

      -- | Messages scheduled but not yet sent
    , qScheduled       :: MQ msg nid

      -- | Buckets with known peers
      --
      -- NOTE: When taking multiple MVars at the same time, qBuckets must be
      -- taken first (lock ordering).
    , qBuckets         :: MVar (Map buck (Peers nid))

      -- | Recent communication failures
    , qFailures        :: MVar (Failures nid)

      -- | Used to send control messages to the main thread
    , qCtrlMsg         :: MVar CtrlMsg

      -- | Signal we use to wake up blocked threads
    , qSignal          :: Signal CtrlMsg

      -- | Some metrics about the queue's health
    , qHealth          :: QHealth buck
    }

-- | Use a formatter to get a dump of the state.
-- Currently this just shows the known peers.
dumpState
    :: MonadIO m
    => OutboundQ msg nid buck
    -> (forall a . (Format r a) -> a)
    -> m r
dumpState outQ@OutQ{} formatter = do
    peers <- getAllPeers outQ
    let formatted = formatter format peers
    return formatted
  where
    format = "OutboundQ internal state '{"%shown%"}'"

-- | Debug function to return the `InFlight` map. Internal use only.
currentlyInFlight :: forall m msg nid buck. MonadIO m => OutboundQ msg nid buck -> m (InFlight nid)
currentlyInFlight = liftIO . readMVar . qInFlight

-- | Type assumed for unknown nodes
--
-- Occassionally the queue will receive requests to send messages to nodes it
-- does not know about. This may happen for instance when the network contains
-- P2P nodes and those nodes announce new blocks to us, but we have not
-- registered those nodes as our peers (P2P networks often are not
-- bidirectional). In order to be able to apply the various policies, the queue
-- needs to assume a node type for these nodes.
newtype UnknownNodeType nid = UnknownNodeType (nid -> NodeType)

-- | Initialize the outbound queue
--
-- NOTE: The dequeuing thread must be started separately. See 'dequeueThread'.
new :: forall m msg nid buck.
       ( MonadIO m
       , FormatMsg msg
       , Ord nid
       , Show nid
       , Enum buck
       , Bounded buck
       , Ord buck
       , Show buck
       )
    => String                  -- ^ Identifier of this node (for logging)
    -> EnqueuePolicy nid
    -> DequeuePolicy
    -> FailurePolicy nid
    -> (buck -> MaxBucketSize)
    -> UnknownNodeType nid
    -> m (OutboundQ msg nid buck)
new qSelf
    qEnqueuePolicy
    qDequeuePolicy
    qFailurePolicy
    qMaxBucketSize
    (UnknownNodeType qUnknownNodeType)
  = liftIO $ do
    qInFlight    <- newMVar Map.empty
    qScheduled   <- MQ.new
    qBuckets     <- newMVar Map.empty
    qCtrlMsg     <- newEmptyMVar
    qFailures    <- newMVar Map.empty
    qRateLimited <- newMVar Set.empty
    qHealth      <- newQHealth

    -- Only look for control messages when the queue is empty
    let checkCtrlMsg :: IO (Maybe CtrlMsg)
        checkCtrlMsg = do
          qSize <- MQ.size qScheduled
          if qSize == 0
            then tryTakeMVar qCtrlMsg
            else return Nothing

    qSignal <- newSignal checkCtrlMsg

    return OutQ{..}

{-------------------------------------------------------------------------------
  Monitoring the queue's health
-------------------------------------------------------------------------------}

-- | Queue health metrics
data QHealth buck = QHealth {
      -- | Total number of failed "enqueue all" instructions
      qFailedEnqueueAll  :: Counter

      -- | Total number of failed "enqueue one" instructions
    , qFailedEnqueueOne  :: Counter

      -- | Total number of times a message failed to send to _any_ of the
      -- peers it got enqueued to
    , qFailedAllSends    :: Counter

      -- | Total number of times cherish looped
    , qFailedCherishLoop :: Counter

      -- | Total number we could not choose a node from a list of alternatives
    , qFailedChooseAlt   :: Counter

      -- | Total number of failed sends
    , qFailedSend        :: Counter

      -- | Total number of times we refused to update a bucket because doing
      -- so would exceed the bucket's maximum size
    , qFailedBucketFull  :: buck -> Counter
    }

newQHealth :: forall buck. (Enum buck, Bounded buck, Ord buck)
           => IO (QHealth buck)
newQHealth = QHealth
    <$> Counter.new
    <*> Counter.new
    <*> Counter.new
    <*> Counter.new
    <*> Counter.new
    <*> Counter.new
    <*> indexedCounter
  where
    indexedCounter :: IO (buck -> Counter)
    indexedCounter = do
      bs <- forM [minBound .. maxBound] $ \b -> (b, ) <$> Counter.new
      return (Map.fromList bs Map.!)

-- | An enumeration of the various kinds of failures
--
-- The third type argument indicates the additional information we need to
-- format a human-readable error or warning message for the failure.
data Failure msg nid buck fmt where
    FailedEnqueueAll   :: Failure msg nid buck (Enqueue, Some msg, [Alts nid])
    FailedEnqueueOne   :: Failure msg nid buck (Enqueue, Some msg, [(NodeType, Alts nid)])
    FailedAllSends     :: Failure msg nid buck (Some msg, [nid])
    FailedCherishLoop  :: Failure msg nid buck ()
    FailedChooseAlt    :: Failure msg nid buck [nid]
    FailedSend         :: Failure msg nid buck (Some (Packet msg nid), SomeException)
    FailedBucketFull   :: buck -> Failure msg nid buck ()

deriving instance Show buck => Show (Failure msg nid buck fmt)

enumFailures :: (Enum buck, Bounded buck) => [Some (Failure msg nid buck)]
enumFailures = mconcat [
      [Some FailedEnqueueAll]
    , [Some FailedEnqueueOne]
    , [Some FailedAllSends]
    , [Some FailedCherishLoop]
    , [Some FailedChooseAlt]
    , [Some FailedSend]
    , [Some (FailedBucketFull b) | b <- [minBound .. maxBound]]
    ]

failureSeverity :: Failure msg nid buck fmt -> Severity
failureSeverity FailedEnqueueAll     = Error
failureSeverity FailedEnqueueOne     = Error
failureSeverity FailedAllSends       = Error
failureSeverity FailedCherishLoop    = Error
failureSeverity FailedChooseAlt      = Warning
failureSeverity FailedSend           = Warning
failureSeverity (FailedBucketFull _) = Warning

failureCounter :: Failure msg nid buck fmt -> QHealth buck -> Counter
failureCounter failure QHealth{..} =
    case failure of
      FailedEnqueueAll   -> qFailedEnqueueAll
      FailedEnqueueOne   -> qFailedEnqueueOne
      FailedAllSends     -> qFailedAllSends
      FailedCherishLoop  -> qFailedCherishLoop
      FailedChooseAlt    -> qFailedChooseAlt
      FailedSend         -> qFailedSend
      FailedBucketFull b -> qFailedBucketFull b

-- | Qualified name for the EKG counter for a failure
failureCounterName :: Show buck => Failure msg nid buck fmt -> [String]
failureCounterName FailedEnqueueAll     = ["FailedEnqueueAll"]
failureCounterName FailedEnqueueOne     = ["FailedEnqueueOne"]
failureCounterName FailedAllSends       = ["FailedAllSends"]
failureCounterName FailedCherishLoop    = ["FailedCherishLoop"]
failureCounterName FailedChooseAlt      = ["FailedChooseAlt"]
failureCounterName FailedSend           = ["FailedSend"]
failureCounterName (FailedBucketFull b) = ["FailedBucketFull", show b]

failureFormat :: (FormatMsg msg, Show nid, Show buck)
              => Failure msg nid buck fmt -> String -> fmt -> Text
failureFormat FailedEnqueueAll self (enq, Some msg, fwdSets) =
    sformat ( string
            % ": enqueue instruction " % shown
            % " failed to enqueue message " % formatMsg
            % " to forwarding sets " % shown
            )
            self enq msg fwdSets
failureFormat FailedEnqueueOne self (enq, Some msg, fwdSets) =
    sformat ( string
            % ": enqueue instruction " % shown
            % " failed to enqueue message " % formatMsg
            % " to forwarding sets " % shown
            )
            self enq msg fwdSets
failureFormat FailedAllSends self (Some msg, nids) =
    sformat ( string % ": message " % formatMsg
            % " got enqueued to " % shown
            % " but all sends failed"
            )
            self msg nids
failureFormat FailedCherishLoop self () =
    sformat ( string % ": enqueueCherished loop? This a policy failure." )
            self
failureFormat FailedChooseAlt self alts =
    sformat ( string % ": could not choose suitable alternative from " % shown )
            self alts
failureFormat FailedSend self (Some Packet{..}, err) =
    sformat ( string % ": sending " % formatMsg % " to " % shown
            % " failed with " % string % " :: " % shown)
            self
            packetPayload
            packetDestId
            (displayException err)
            (typeOf err)
failureFormat (FailedBucketFull b) self () =
    sformat ( string % ": maximum bucket size of bucket " % shown % " exceeded" )
            self b

logFailure :: (WithLogger m, MonadIO m)
           => OutboundQ msg nid buck
           -> Failure msg nid buck fmt
           -> fmt
           -> m ()
logFailure OutQ{..} failure fmt = do
    Log.logMessage (failureSeverity failure)
                   (failureFormat failure qSelf fmt)
    liftIO $ Counter.inc $ failureCounter failure qHealth

{-------------------------------------------------------------------------------
  EKG metrics
-------------------------------------------------------------------------------}

type Namespace = String

-- | Register queue-related metrics to EKG.
registerQueueMetrics :: forall msg nid buck. (Enum buck, Bounded buck, Show buck)
                     => Maybe Namespace
                     -- ^ An optional namespace the metrics should be prefixed with.
                     -- Passing nothing won't touch the names assigned to the metrics,
                     -- whereas `Just ns` will prefix each name with `ns`. Example:
                     --
                     -- ns.InFlight
                     -- ...
                     --
                     -> OutboundQ msg nid buck
                     -> Monitoring.Store
                     -> IO ()
registerQueueMetrics namespaceMb OutQ{..} store = do
    -- Gauges showing queue internal state
    regGauge ["InFlight"]  $ countInFlight <$> readMVar qInFlight
    regGauge ["Failures"]  $ readMVar qFailures >>= countRecentFailures
    regGauge ["Scheduled"] $ MQ.size qScheduled

    -- We create a gauge for every bucket
    --
    -- Since these buckets are added to the map when needed, but we register
    -- all gauges up-front, we need to enumerate the list of buckets
    --
    -- NOTE: By reporting this per bucket, we can easily distinguish the number
    -- of subscribers ('BucketSubscriptionListener') from, say, statically known
    -- peers ('BucketStatic').
    forM_ [minBound .. maxBound] $ \buck ->
      regGauge ["bucket", show buck] $
        countPeers . Map.findWithDefault mempty buck <$> readMVar qBuckets

    -- Counters for all failures
    forM_ (enumFailures :: [Some (Failure msg nid buck)]) $ \(Some failure) ->
      regCounter (failureCounterName failure)
                 (failureCounter     failure)
  where
    regGauge :: Integral a => [String] -> IO a -> IO ()
    regGauge qualName f = Monitoring.registerGauge
                            (metric qualName)
                            (fromIntegral <$> f)
                            store

    regCounter :: [String] -> (QHealth buck -> Counter) -> IO ()
    regCounter qualName ctr = Monitoring.registerCounter
                                (metric qualName)
                                (fromIntegral <$> Counter.read (ctr qHealth))
                                store

    metric :: [String] -> Text
    metric =
        -- If we have a namespace, add it to the beginning of the
        -- initial list, so that the final metric name will be
        -- correctly namespaced.
        -- E.g.
        -- let namespaceMb = Just "mynamespace"
        -- metric ["InFlight"]
        -- >>> mynamespace.test.InFlight
        let prefix = maybeToList namespaceMb ++ ["queue"]
        in T.pack . intercalate "." . (prefix ++)

countInFlight :: InFlight nid -> Int
countInFlight = sum . fmap sum

countPeers :: Ord nid => Peers nid -> Int
countPeers = Set.size . peersRouteSet

countRecentFailures :: MonadIO m => Failures nid -> m Int
countRecentFailures fs = liftIO $ aux <$> getCurrentTime
  where
    aux :: UTCTime -> Int
    aux now = Map.size (Map.filter (isRecentFailure now) fs)

{-------------------------------------------------------------------------------
  Interpreter for the enqueing policy
-------------------------------------------------------------------------------}

-- | Enqueue a message to the specified set of peers
--
-- If no suitable peers can be found, choose one from the fallback set (if any).
intEnqueue :: forall m msg nid buck a. (MonadIO m, WithLogger m)
           => OutboundQ msg nid buck
           -> MsgType nid
           -> msg a
           -> Peers nid
           -> m [Packet msg nid a]
intEnqueue outQ@OutQ{..} msgType msg peers = fmap concat $
    forM (qEnqueuePolicy msgType) $ \case

      enq@EnqueueAll{..} -> do
        let fwdSets :: AllOf (Alts nid)
            fwdSets = removeOrigin (msgOrigin msgType) $
                        peersRoutes peers ^. routesOfType enqNodeType

            sendAll :: [Packet msg nid a]
                    -> AllOf (Alts nid)
                    -> m [Packet msg nid a]
            sendAll acc []           = return acc
            sendAll acc (alts:altss) = do
              mPacket <- sendFwdSet True -- warn on failure
                                    (map packetDestId acc)
                                    enqMaxAhead
                                    enqPrecedence
                                    (enqNodeType, alts)
              case mPacket of
                Nothing -> sendAll    acc  altss
                Just p  -> sendAll (p:acc) altss

        enqueued <- sendAll [] fwdSets

        -- Log an error if we didn't manage to enqueue the message to any peer
        -- at all (provided that we were configured to send it to some)
        if | null fwdSets ->
               logDebug $ debugNotEnqueued enqNodeType -- This isn't an error
           | null enqueued ->
               logFailure outQ FailedEnqueueAll (enq, Some msg, fwdSets)
           | otherwise ->
               logDebug $ debugEnqueued enqueued

        return enqueued

      enq@EnqueueOne{..} -> do
        let fwdSets :: [(NodeType, Alts nid)]
            fwdSets = concatMap
                        (\t -> map (t,) $ removeOrigin (msgOrigin msgType) $
                                            peersRoutes peers ^. routesOfType t)
                        enqNodeTypes

            -- We don't warn when choosing an alternative here, as failure to
            -- choose an alternative implies failure to enqueue for 'EnqueueOne'
            sendOne :: [(NodeType, Alts nid)] -> m [Packet msg nid a]
            sendOne = fmap maybeToList
                    . orElseM
                    . map (sendFwdSet False [] enqMaxAhead enqPrecedence)

        enqueued <- sendOne fwdSets

        -- Log an error if we didn't manage to enqueue the message
        if null enqueued
          then logFailure outQ FailedEnqueueOne (enq, Some msg, fwdSets)
          else logDebug $ debugEnqueued enqueued

        return enqueued
  where
    -- Attempt to send the message to a single forwarding set
    sendFwdSet :: Bool                 -- ^ Warn on failure?
               -> [nid]                -- ^ Nodes we already sent something to
               -> MaxAhead             -- ^ Max allowed number of msgs ahead
               -> Precedence           -- ^ Precedence of the message
               -> (NodeType, Alts nid) -- ^ Alternatives to choose from
               -> m (Maybe (Packet msg nid a))
    sendFwdSet warnOnFailure alreadyPicked maxAhead prec (nodeType, alts) = do
      mAlt <- pickAlt outQ maxAhead prec $ filter (`notElem` alreadyPicked) alts
      case mAlt of
        Nothing -> do
          when warnOnFailure $ logFailure outQ FailedChooseAlt alts
          return Nothing
        Just alt -> liftIO $ do
          sentVar <- newEmptyMVar
          let packet = Packet {
                           packetPayload  = msg
                         , packetDestId   = alt
                         , packetMsgType  = msgType
                         , packetDestType = nodeType
                         , packetPrec     = prec
                         , packetSent     = sentVar
                         }
          mqEnqueue qScheduled (EnqPacket packet)
          poke qSignal
          return $ Just packet

    -- Don't forward a message back to the node that sent it originally
    -- (We assume that a node does not appear in its own list of peers)
    removeOrigin :: Origin nid -> AllOf (Alts nid) -> AllOf (Alts nid)
    removeOrigin origin =
      case origin of
        OriginSender    -> id
        OriginForward n -> filter (not . null) . map (filter (/= n))

    debugNotEnqueued :: NodeType -> Text
    debugNotEnqueued nodeType = sformat
      ( string
      % ": message "
      % formatMsg
      % " not enqueued to any nodes of type "
      % shown
      % " since no such (relevant) peers listed in "
      % shown
      )
      qSelf
      msg
      nodeType
      peers

    debugEnqueued :: [Packet msg nid a] -> Text
    debugEnqueued enqueued =
      sformat (string % ": message " % formatMsg % " enqueued to " % shown)
              qSelf msg (map packetDestId enqueued)

-- | Node ID with current stats needed to pick a node from a list of alts
data NodeWithStats nid = NodeWithStats {
      nstatsId       :: nid  -- ^ Node ID
    , nstatsFailure  :: Bool -- ^ Recent failure?
    , nstatsAhead    :: Int  -- ^ Number of messages ahead
    , nstatsInFlight :: Int  -- ^ Number of messages in flight
    }

-- | Compute current node statistics
nodeWithStats :: (MonadIO m, WithLogger m)
              => OutboundQ msg nid buck
              -> Precedence -- ^ For determining number of messages ahead
              -> nid
              -> m (NodeWithStats nid)
nodeWithStats outQ prec nstatsId = do
    (nstatsAhead, nstatsInFlight) <- countAhead outQ nstatsId prec
    nstatsFailure                 <- hasRecentFailure outQ nstatsId
    return NodeWithStats{..}

-- | Choose an appropriate node from a list of alternatives
--
-- All alternatives are assumed to be of the same type; we prefer to pick
-- nodes with a smaller number of messages ahead.
pickAlt :: forall m msg nid buck. (MonadIO m, WithLogger m)
        => OutboundQ msg nid buck
        -> MaxAhead
        -> Precedence
        -> [nid]
        -> m (Maybe nid)
pickAlt outQ@OutQ{} (MaxAhead maxAhead) prec alts = do
    alts' <- mapM (nodeWithStats outQ prec) alts
    orElseM [
        if | nstatsFailure -> do
               logDebug $ debugFailure nstatsId
               return Nothing
           | (nstatsAhead + nstatsInFlight) > maxAhead -> do
               logDebug $ debugAhead nstatsId nstatsAhead nstatsInFlight maxAhead
               return Nothing
           | otherwise -> do
               return $ Just nstatsId
      | NodeWithStats{..} <- sortBy (comparing ((+) <$> nstatsAhead <*> nstatsInFlight)) alts'
      ]
  where
    debugFailure :: nid -> Text
    debugFailure = sformat $
          "Rejected alternative " % shown
        % " as it has a recent failure"

    debugAhead :: nid -> Int -> Int -> Int -> Text
    debugAhead = sformat $
          "Rejected alternative " % shown
        % " as it has " % shown
        % " messages ahead and " %shown
        % " messages in flight, which total more than the maximum " % shown

-- | Check how many messages are currently ahead
--
-- First component is the total ahead in the queue.
-- Second component is the total in-flight.
--
-- NOTE: This is of course a highly dynamic value; by the time we get to
-- actually enqueue the message the value might be slightly different. Bounds
-- are thus somewhat fuzzy.
countAhead :: forall m msg nid buck. (MonadIO m, WithLogger m)
           => OutboundQ msg nid buck -> nid -> Precedence -> m (Int, Int)
countAhead OutQ{..} nid prec = do
    logDebug . debugInFlight =<< liftIO (readMVar qInFlight)
    (inFlight, inQueue) <- liftIO $ (,)
      <$> forM [prec .. maxBound] (\prec' ->
            view (inFlightWithPrec nid prec') <$> readMVar qInFlight)
      <*> forM [prec .. maxBound] (\prec' ->
            MQ.sizeBy (KeyByDestPrec nid prec') qScheduled)
    return $ (sum inQueue, sum inFlight)
  where
    debugInFlight :: InFlight nid -> Text
    debugInFlight = sformat (string % ": inFlight = " % shown) qSelf


{-------------------------------------------------------------------------------
  Interpreter for the dequeueing policy
-------------------------------------------------------------------------------}

checkMaxInFlight :: Ord nid => DequeuePolicy -> InFlight nid -> NotBusy nid
checkMaxInFlight dequeuePolicy inFlight nodeType nid =
    sum (Map.elems (inFlight ^. inFlightTo nid)) < n
  where
    MaxInFlight n = deqMaxInFlight (dequeuePolicy nodeType)

intDequeue :: forall m msg nid buck. WithLogger m
           => OutboundQ msg nid buck
           -> ThreadRegistry m
           -> SendMsg m msg nid
           -> m (Maybe CtrlMsg)
intDequeue outQ@OutQ{..} threadRegistry@TR{} sendMsg = do
    mPacket <- getPacket
    case mPacket of
      Left ctrlMsg -> return $ Just ctrlMsg
      Right packet -> sendPacket packet >> return Nothing
  where
    getPacket :: m (Either CtrlMsg (EnqPacket msg nid))
    getPacket = retryIfNothing qSignal $ do
      inFlight    <- liftIO $ readMVar qInFlight
      rateLimited <- liftIO $ readMVar qRateLimited

      let notBusy :: NotBusy nid
          notBusy nodeType nid = and [
              not $ nid `Set.member` rateLimited
            , checkMaxInFlight qDequeuePolicy inFlight nodeType nid
            ]

      mqDequeue qScheduled notBusy

    -- Send the packet we just dequeued
    --
    -- At this point we have dequeued the message but not yet recorded it as
    -- in-flight. That's okay though: the only function whose behaviour is
    -- affected by 'rsInFlight' is 'intDequeue', the main thread (this thread) is
    -- the only thread calling 'intDequeue', and we will update 'rsInFlight'
    -- before dequeueing the next message.
    --
    -- We start a new thread to handle the conversation. This is a bit of a
    -- subtle design decision. We could instead start the conversation here in
    -- the main thread, and fork a thread only to wait for the acknowledgement.
    -- The problem with doing that is that if that conversation gets blocked or
    -- delayed for any reason, it will block or delay the whole outbound queue.
    -- The downside of the /current/ solution is that it makes priorities
    -- somewhat less meaningful: although the priorities dictate in which order
    -- we fork threads to handle conversations, after that those threads all
    -- compete with each other (amongst other things, for use of the network
    -- device), with no real way to prioritize any one thread over the other. We
    -- will be able to solve this conumdrum properly once we move away from TCP
    -- and use the RINA network architecture instead.
    sendPacket :: EnqPacket msg nid -> m ()
    sendPacket (EnqPacket p) = do
      sendStartTime <- liftIO $ getCurrentTime

      -- Mark the message as in-flight, limiting both enqueues and dequeues
      setInFlightFor p (\n -> n + 1) qInFlight

      -- We mark the node as rate limited, making it unavailable for any
      -- additional dequeues until the timer expires and we mark it as
      -- available again. We start the timer /before/ the send so that the
      -- duration of the send does not affect when the next dequeue can take
      -- places (apart from max-in-flight, of course).
      case deqRateLimit $ qDequeuePolicy (packetDestType p) of
        NoRateLimiting -> return ()
        MaxMsgPerSec n -> do
          let delay = 1000000 `div` n
          applyMVar_ qRateLimited $ Set.insert (packetDestId p)
          forkThread threadRegistry $ \unmask -> unmask $
            (liftIO $ threadDelay delay) `finally` (liftIO $ do
              applyMVar_ qRateLimited $ Set.delete (packetDestId p)
              poke qSignal)

      forkThread threadRegistry $ \unmask -> do
        logDebug $ debugSending p

        ma <- try $ unmask $ sendMsg (packetPayload p) (packetDestId p)

        -- Reduce the in-flight count ..
        setInFlightFor p (\n -> n - 1) qInFlight
        liftIO $ poke qSignal

        -- .. /before/ notifying the sender that the send is complete.
        -- If we did this the other way around a subsequent enqueue might fail
        -- because of policy restrictions on the max in-flight.
        liftIO $ putMVar (packetSent p) ma

        case ma of
          Left err -> do
            logFailure outQ FailedSend (Some p, err)
            intFailure outQ p sendStartTime err
          Right _  ->
            return ()

        logDebug $ debugSent p

    debugSending :: Packet msg nid a -> Text
    debugSending Packet{..} =
      sformat (string % ": sending " % formatMsg % " to " % shown)
              qSelf packetPayload packetDestId

    debugSent :: Packet msg nid a -> Text
    debugSent Packet{..} =
      sformat (string % ": sent " % formatMsg % " to " % shown)
              qSelf packetPayload packetDestId

{-------------------------------------------------------------------------------
  Interpreter for failure policy
-------------------------------------------------------------------------------}

-- | What do we know when sending a message fails?
--
-- NOTE: Since we don't send messages to nodes listed in failures, we can
-- assume that there isn't an existing failure here.
intFailure :: forall m msg nid buck a. MonadIO m
           => OutboundQ msg nid buck
           -> Packet msg nid a  -- ^ Packet we failed to send
           -> UTCTime           -- ^ Time of the send
           -> SomeException     -- ^ The exception thrown by the send action
           -> m ()
intFailure OutQ{..} p sendStartTime err = do
    applyMVar_ qFailures $
      Map.insert (packetDestId p) (
          sendStartTime
        , qFailurePolicy (packetDestType p)
                         (packetMsgType  p)
                         err
        )

hasRecentFailure :: MonadIO m => OutboundQ msg nid buck -> nid -> m Bool
hasRecentFailure OutQ{..} nid = liftIO $ do
    mFailure <- Map.lookup nid <$> readMVar qFailures
    case mFailure of
      Nothing      -> return False
      Just failure -> (`isRecentFailure` failure) <$> getCurrentTime

isRecentFailure :: UTCTime                    -- ^ Now
                -> (UTCTime, ReconsiderAfter) -- ^ Failure
                -> Bool
isRecentFailure now (timeOfFailure, ReconsiderAfter n) =
    now < addUTCTime n timeOfFailure

-- | Reset internal statistics about failed nodes
--
-- This is useful when we know for external reasons that nodes may be reachable
-- again, allowing the outbound queue to enqueue messages to those nodes.
clearRecentFailures :: MonadIO m => OutboundQ msg nid buck -> m ()
clearRecentFailures OutQ{..} = applyMVar_ qFailures $ const Map.empty

-- | Clear recent failure status for a particular node
--
-- This is useful when we know for external reasons that this particular
-- node is reachable again (for instance, because it sent us a message),
-- allowing the outbound queue to enqueue messages to that node again.
clearFailureOf :: MonadIO m => OutboundQ msg nid buck -> nid -> m ()
clearFailureOf OutQ{..} nid = applyMVar_ qFailures $ Map.delete nid

{-------------------------------------------------------------------------------
  Public interface to enqueing
-------------------------------------------------------------------------------}

-- | Queue a message to be sent, but don't wait (asynchronous API)
enqueue :: (MonadIO m, WithLogger m)
        => OutboundQ msg nid buck
        -> MsgType nid -- ^ Type of the message being sent
        -> msg a       -- ^ Message to send
        -> m [(nid, m (Either SomeException a))]
enqueue outQ msgType msg = do
    waitAsync <$> intEnqueueTo outQ msgType msg (msgEnqueueTo msgType)

-- | Queue a message and wait for it to have been sent
--
-- Returns for each node that the message got enqueued the result of the
-- send action (or an exception if it failed).
enqueueSync' :: (MonadIO m, WithLogger m)
             => OutboundQ msg nid buck
             -> MsgType nid -- ^ Type of the message being sent
             -> msg a       -- ^ Message to send
             -> m [(nid, Either SomeException a)]
enqueueSync' outQ msgType msg = do
    promises <- enqueue outQ msgType msg
    traverse (\(nid, wait) -> (,) nid <$> wait) promises

-- | Queue a message and wait for it to have been sent
--
-- We wait for the message to have been sent (successfully or unsuccessfully)
-- to all the peers it got enqueued to. Like in the asynchronous API,
-- warnings will be logged when individual sends fail. Additionally, we will
-- log an error when /all/ sends failed (this doesn't currently happen in the
-- asynchronous API).
enqueueSync :: forall m msg nid buck a. (MonadIO m, WithLogger m)
            => OutboundQ msg nid buck
            -> MsgType nid -- ^ Type of the message being sent
            -> msg a       -- ^ Message to send
            -> m ()
enqueueSync outQ msgType msg =
    warnIfNotOneSuccess outQ msg $ enqueueSync' outQ msgType msg

-- | Enqueue a message which really should not get lost
--
-- Returns 'True' if the message was successfully sent.
enqueueCherished :: forall m msg nid buck a. (MonadIO m, WithLogger m)
                 => OutboundQ msg nid buck
                 -> MsgType nid -- ^ Type of the message being sent
                 -> msg a       -- ^ Message to send
                 -> m Bool
enqueueCherished outQ msgType msg =
    cherish outQ $ enqueueSync' outQ msgType msg

{-------------------------------------------------------------------------------
  Internal generalization of the enqueueing API
-------------------------------------------------------------------------------}

-- | Enqueue message to the specified set of peers
intEnqueueTo :: forall m msg nid buck a. (MonadIO m, WithLogger m)
             => OutboundQ msg nid buck
             -> MsgType nid
             -> msg a
             -> EnqueueTo nid
             -> m [Packet msg nid a]
intEnqueueTo outQ@OutQ{..} msgType msg enqTo = do
    peers <- restrict <$> getAllPeers outQ
    intEnqueue outQ msgType msg peers
  where
    -- Restrict the set of all peers to the requested subset (if one).
    --
    -- Any unknown peers will be used as a fallback in case no other peers
    -- can be found.
    restrict :: Peers nid -> Peers nid
    restrict allPeers =
      case enqTo of
        EnqueueToAll           -> allPeers
        EnqueueToSubset subset ->
          let restricted = restrictPeers subset allPeers
              -- Every peer in the target subset, not appearing in the routes,
              -- will be classified and included in a route via peersFromList.
              unknown    = peersFromList mempty
                         . map (\nid -> (classifyNodeDefault allPeers (qUnknownNodeType nid) nid, [nid]))
                         . Set.toList
                         $ subset Set.\\ peersRouteSet allPeers

          in  restricted <> unknown

waitAsync :: MonadIO m
          => [Packet msg nid a] -> [(nid, m (Either SomeException a))]
waitAsync = map $ \Packet{..} -> (packetDestId, liftIO $ readMVar packetSent)

-- | Make sure a synchronous send succeeds to at least one peer
warnIfNotOneSuccess :: forall m msg nid buck a. (MonadIO m, WithLogger m)
                    => OutboundQ msg nid buck
                    -> msg a
                    -> m [(nid, Either SomeException a)]
                    -> m ()
warnIfNotOneSuccess outQ msg act = do
    attempts <- act
    -- If the attempts is null, we would already have logged an error that
    -- we couldn't enqueue at all
    when (not (null attempts) && null (successes attempts)) $
      logFailure outQ FailedAllSends (Some msg, map fst attempts)

-- | Repeatedly run an action until at least one send succeeds, we run out of
-- options, or we reach a predetermined maximum number of iterations.
cherish :: forall m msg nid buck a. (MonadIO m, WithLogger m)
        => OutboundQ msg nid buck
        -> m [(nid, Either SomeException a)]
        -> m Bool
cherish outQ act =
    go maxNumIterations
  where
    go :: Int -> m Bool
    go 0 = do
      logFailure outQ FailedCherishLoop ()
      return False
    go n = do
      attempts <- act
      if | not (null (successes attempts)) ->
             -- We managed to successfully send it to at least one peer
             -- Consider it a job well done
             return True
         | null attempts ->
             -- We couldn't find anyone to send to. Give up in despair.
             return False
         | otherwise -> -- not (null attemts) && null succs
             -- We tried to send it to some nodes but they all failed
             -- In this case, we simply try again, hoping that we'll manage to
             -- pick some different alternative nodes to send to (since the
             -- failures will have been recorded in qFailures)
             go (n - 1)

    -- If we didn't have an upper bound on the number of iterations, we could
    -- in principle loop indefinitely, if the timeouts on sends are close to
    -- the time-to-reset-error-state defined by the failure policy.
    -- (Thus, the latter should be significantly larger than send timeouts.)
    maxNumIterations :: Int
    maxNumIterations = 4

successes :: [(nid, Either SomeException a)] -> [a]
successes = rights . map snd

{-------------------------------------------------------------------------------
  Dequeue thread
-------------------------------------------------------------------------------}

-- | Action to send a message
--
-- The action should block until the message has been acknowledged by the peer.
--
-- NOTE:
--
-- * The IO action will be run in a separate thread.
-- * No additional timeout is applied to the 'SendMsg', so if one is
--   needed it must be provided externally.
type SendMsg m msg nid = forall a. msg a -> nid -> m a

-- | The dequeue thread
--
-- It is the responsibility of the next layer up to fork this thread; this
-- function does not return unless told to terminate using 'waitShutdown'.
dequeueThread :: forall m msg nid buck. (
                   MonadIO              m
                 , MonadMask            m
                 , M.Mockable M.Async   m
                 , M.Mockable M.Fork    m
                 , Ord (M.ThreadId      m)
                 , WithLogger           m
                 )
              => OutboundQ msg nid buck -> SendMsg m msg nid -> m ()
dequeueThread outQ@OutQ{..} sendMsg = withThreadRegistry $ \threadRegistry ->
    let loop :: m ()
        loop = do
          mCtrlMsg <- intDequeue outQ threadRegistry sendMsg
          case mCtrlMsg of
            Nothing      -> loop
            Just ctrlMsg -> do
              waitAllThreads threadRegistry
              case ctrlMsg of
                Shutdown ack -> do liftIO $ putMVar ack ()
                Flush    ack -> do liftIO $ putMVar ack ()
                                   loop

    in loop

{-------------------------------------------------------------------------------
  Controlling the dequeue thread
-------------------------------------------------------------------------------}

-- | Control messages sent to the main thread
--
-- NOTE: These are given lower precedence than non-control messages.
data CtrlMsg =
    Shutdown (MVar ())
  | Flush    (MVar ())

-- | Gracefully shutdown the relayer
waitShutdown :: MonadIO m => OutboundQ msg nid buck -> m ()
waitShutdown OutQ{..} = liftIO $ do
    ack <- newEmptyMVar
    putMVar qCtrlMsg $ Shutdown ack
    poke qSignal
    takeMVar ack

-- | Wait for all messages currently enqueued to have been sent
flush :: MonadIO m => OutboundQ msg nid buck -> m ()
flush OutQ{..} = liftIO $ do
    ack <- newEmptyMVar
    putMVar qCtrlMsg $ Flush ack
    poke qSignal
    takeMVar ack

{-------------------------------------------------------------------------------
  Buckets

  NOTE: Behind NAT nodes: Edge nodes behind NAT can contact a relay node to ask
  to be notified of messages. The listener on the relay node should call
  'addKnownPeers' on its outbound queue to effectively subscribe the edge node
  that contacted it. Then the conversation should remain open, so that the
  (heavy-weight) TCP connection between the edge node and the relay node is
  kept open. When the edge node disappears the listener thread on the relay
  node should call 'removeKnownPeer' to remove the edge node from its outbound
  queue again.
-------------------------------------------------------------------------------}

-- | Maximum size for a bucket (if limited)
data MaxBucketSize = BucketSizeUnlimited | BucketSizeMax Int
  deriving (Show, Eq)

exceedsBucketSize :: Int -> MaxBucketSize -> Bool
exceedsBucketSize _ BucketSizeUnlimited = False
exceedsBucketSize n (BucketSizeMax m)   = n > m

-- | Spare capacity for a bucket
data SpareCapacity = UnlimitedCapacity | SpareCapacity Int
  deriving (Show, Eq)

-- | Returns how many "free slots" the bucket `buck` still has for peers.
-- Returns `UnlimitedCapacity` if there is no bucket size limit.
bucketSpareCapacity :: MonadIO m => OutboundQ msg nid buck -> buck -> m SpareCapacity
bucketSpareCapacity OutQ{..} buck = case qMaxBucketSize buck of
  BucketSizeUnlimited        -> return UnlimitedCapacity
  BucketSizeMax maxCapacity  -> do
    currentCapacity <- countPeers . fromMaybe mempty . Map.lookup buck <$> liftIO (readMVar qBuckets)
    return . SpareCapacity $ max 0 (maxCapacity - currentCapacity)

-- | Internal method: read all buckets of peers
getAllPeers :: MonadIO m => OutboundQ msg nid buck -> m (Peers nid)
getAllPeers OutQ{..} = liftIO $ fold <$> readMVar qBuckets

-- | Update a bucket of peers
--
-- Any messages to peers that no longer exist in _any_ bucket will be
-- removed from the queue.
--
-- It is assumed that every bucket is modified by exactly one thread.
-- Provided that assumption is true, then we guarantee the invariant that if
-- thread @T@ adds node @n@ to its (private) bucket and then enqueues a message,
-- that message will not be deleted because another thread happened to remove
-- node @n@ from _their_ bucket.
--
-- Returns 'False' if the update could not complete
-- (maximum bucket size exceeded).
updatePeersBucket :: forall m msg nid buck. (MonadIO m, WithLogger m)
                  => OutboundQ msg nid buck
                  -> buck
                  -> (Peers nid -> Peers nid)
                  -> m Bool
updatePeersBucket outQ@OutQ{..} buck f = do
    success <- liftIO $ modifyMVar qBuckets $ \buckets -> do

      let before   = fold buckets
          buckets' = Map.alter f' buck buckets
          after    = fold buckets'
          removed  = peersSet before Set.\\ peersSet after

      -- Use the peersRouteSet here because the bucket size is in terms of
      -- routed peers.
      if    Set.size (peersRouteSet (buckets' Map.! buck))
         `exceedsBucketSize`
            qMaxBucketSize buck
        then return (buckets, False)
        else do
          forM_ removed $ \nid -> do
            applyMVar_ qInFlight    $ at nid .~ Nothing
            applyMVar_ qFailures    $ Map.delete nid
            applyMVar_ qRateLimited $ Set.delete nid
            MQ.removeAllIn (KeyByDest nid) qScheduled
          return (buckets', True)

    unless success $
      logFailure outQ (FailedBucketFull buck) ()
    return success
  where
    f' :: Maybe (Peers nid) -> Maybe (Peers nid)
    f' Nothing      = Just $ f mempty
    f' (Just peers) = Just $ f peers

{-------------------------------------------------------------------------------
  Auxiliary: starting and registering threads
-------------------------------------------------------------------------------}

data ThreadRegistry m =
       ( MonadIO              m
       , M.Mockable M.Async   m
       , M.Mockable M.Fork    m
       , MonadMask            m
       , Ord (M.ThreadId      m)
       )
    => TR (MVar (Map (M.ThreadId m) (M.Promise m ())))

-- | Create a new thread registry, killing all threads when the action
-- terminates.
withThreadRegistry :: ( MonadIO              m
                      , M.Mockable M.Async   m
                      , M.Mockable M.Fork    m
                      , MonadMask            m
                      , Ord (M.ThreadId      m)
                      )
                   => (ThreadRegistry m -> m ()) -> m ()
withThreadRegistry k = do
    threadRegistry <- liftIO $ TR <$> newMVar Map.empty
    k threadRegistry `finally` killAllThreads threadRegistry

killAllThreads :: ThreadRegistry m -> m ()
killAllThreads (TR reg) = do
    threads <- applyMVar reg $ \threads -> (Map.empty, Map.elems threads)
    mapM_ M.cancel threads

waitAllThreads :: ThreadRegistry m -> m ()
waitAllThreads (TR reg) = do
    threads <- applyMVar reg $ \threads -> (Map.empty, Map.elems threads)
    mapM_ M.wait threads

type Unmask m = forall a. m a -> m a

-- | Fork a new thread, taking care of registration and unregistration
forkThread :: ThreadRegistry m -> (Unmask m -> m ()) -> m ()
forkThread (TR reg) threadBody = mask_ $ do
    barrier <- liftIO $ newEmptyMVar
    thread  <- M.asyncWithUnmask $ \unmask -> do
                 tid <- M.myThreadId
                 liftIO $ takeMVar barrier
                 threadBody unmask `finally`
                   applyMVar_ reg (at tid .~ Nothing)
    tid     <- M.asyncThreadId thread
    applyMVar_ reg (at tid .~ Just thread)
    liftIO $ putMVar barrier ()

{-------------------------------------------------------------------------------
  Auxiliary: Signalling

  A signal is used to detect whether " something " changed between two points in
  time, and block a thread otherwise. Only a single thread should be calling
  'retryIfNothing'; other threads should call 'poke' to indicate when
  something changed and the blocked action can be retried. A signal is _not_ a
  counter: we don't keep track of how often 'poke' is called.
-------------------------------------------------------------------------------}

data Signal b = Signal {
    -- | Used to wake up the blocked thread
    signalPokeVar :: MVar ()

    -- | Check to see if there is an out-of-bound control message available
  , signalCtrlMsg :: IO (Maybe b)
  }

newSignal :: IO (Maybe b) -> IO (Signal b)
newSignal signalCtrlMsg = do
    signalPokeVar <- newEmptyMVar
    return Signal{..}

poke :: Signal b -> IO ()
poke Signal{..} = void $ tryPutMVar signalPokeVar ()

-- | Keep retrying an action until it succeeds, blocking between attempts.
retryIfNothing :: forall m a b. MonadIO m
               => Signal b -> m (Maybe a) -> m (Either b a)
retryIfNothing Signal{..} act = go
  where
    go :: m (Either b a)
    go = do
      ma <- act
      case ma of
        Just a  -> return (Right a)
        Nothing -> do
          -- If the action did not return a value, wait for a concurrent thread
          -- to signal that something has changed (may already have happened as
          -- the action was running, of course, in which case we try again
          -- immediately).
          --
          -- If there were multiple changes, then the signal will only remember
          -- that there /was/ a change, not how many of them. This is ok,
          -- however: we run the action again in this new state, no matter how
          -- many changes took place. If in that new state the action still
          -- fails, then we will wait for further changes on the next iteration.
          mCtrlMsg <- liftIO $ signalCtrlMsg
          case mCtrlMsg of
            Just ctrlMsg ->
              return (Left ctrlMsg)
            Nothing -> do
              liftIO $ takeMVar signalPokeVar
              go

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

orElseM :: forall m a. Monad m => [m (Maybe a)] -> m (Maybe a)
orElseM = foldr aux (return Nothing)
  where
    aux :: m (Maybe a) -> m (Maybe a) -> m (Maybe a)
    aux f g = f >>= maybe g (return . Just)

applyMVar :: MonadIO m => MVar a -> (a -> (a, b)) -> m b
applyMVar mv f = liftIO $ modifyMVar mv $ \a -> return $! f a

applyMVar_ :: MonadIO m => MVar a -> (a -> a) -> m ()
applyMVar_ mv f = liftIO $ modifyMVar_ mv $ \a -> return $! f a

-- | Existential
data Some (f :: k -> *) where
  Some :: f a -> Some f
