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
{-# LANGUAGE RecursiveDo               #-}
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
  , PacketStatus (..)
  , Aborted (..)
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
  , getAllPeers
    -- * Debugging
  , registerQueueMetrics
  , dumpState
  , currentlyInFlight
  ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception (Exception, SomeException, catch, throwIO, displayException,
                                    finally, mask_)
import           Control.Lens
import           Control.Monad
import           Data.Either (rights)
import           Data.Foldable (fold)
import           Data.List (intercalate, sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, maybeToList, mapMaybe)
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

import           Pos.Util.Trace (Trace, traceWith, Severity (..))

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

    -- | TVar with the status of the packet.
    -- Useful if the enqueuer wished to abort an enqueued packet, or kill a
    -- running conversation that was dequeued.
    --
    -- Note: we want a TVar because then someone can wait until it changes.
    -- However, if we use a TVar, the dequeue thread can't safely transition
    -- from Enqueued to Dequeued while spawning the Async inside.
    --
    --   do atomically $ do
    --        writeTVar thread
    --      thread <- async ...
  , packetStatus   :: TVar (PacketStatus a)
  }

-- | Status of a packet.
-- In the queue and not aborted.
-- In the queue and aborted (reaching into the queue and removing it seems
-- infeasible).
-- Out of the queue and in-flight.
data PacketStatus a = PacketEnqueued | PacketAborted | PacketDequeued (Async a)

-- | An exception that may be useful for dealing with 'PacketStatus', for
-- instance when waiting on an enqueued thing: if you retry a transaction
-- until it becomes 'PacketDequeued', you may want to throw 'Aborted' if
-- it goes to 'PacketAborted' instead.
data Aborted = Aborted

deriving instance Show Aborted
instance Exception Aborted

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

mqEnqueue :: (Ord nid)
          => MQ msg nid -> EnqPacket msg nid -> IO ()
mqEnqueue qs p =
  MQ.enqueue qs [ KeyByDest     (liftEnq packetDestId p)
                , KeyByDestPrec (liftEnq packetDestId p) (liftEnq packetPrec p)
                , KeyByPrec                              (liftEnq packetPrec p)
                ]
                p

-- | Check whether a node is not currently busy
--
-- (i.e., number of in-flight messages is less than the max)
type NotBusy nid = NodeType -> nid -> Bool

mqDequeue :: forall msg nid. (Ord nid)
          => MQ msg nid -> NotBusy nid -> IO (Maybe (EnqPacket msg nid))
mqDequeue qs notBusy =
    orElseM [
        MQ.dequeue (KeyByPrec prec) notBusy' qs
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
setInFlightFor :: forall msg nid a. (Ord nid)
               => Packet msg nid a
               -> (Int -> Int)
               -> MVar (InFlight nid)
               -> IO ()
setInFlightFor Packet{..} f var = modifyMVar_ var (return . update)
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
      qTrace           :: Trace IO (Severity, Text)

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
    :: OutboundQ msg nid buck
    -> (forall a . (Format r a) -> a)
    -> IO r
dumpState outQ@OutQ{} formatter = do
    peers <- getAllPeers outQ
    let formatted = formatter format peers
    return formatted
  where
    format = "OutboundQ internal state '{"%shown%"}'"

-- | Debug function to return the `InFlight` map. Internal use only.
currentlyInFlight :: forall msg nid buck. OutboundQ msg nid buck -> IO (InFlight nid)
currentlyInFlight = readMVar . qInFlight

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
new :: forall msg nid buck.
       ( FormatMsg msg
       , Ord nid
       , Show nid
       , Enum buck
       , Bounded buck
       , Ord buck
       , Show buck
       )
    => Trace IO (Severity, Text)
    -> EnqueuePolicy nid
    -> DequeuePolicy
    -> FailurePolicy nid
    -> (buck -> MaxBucketSize)
    -> UnknownNodeType nid
    -> IO (OutboundQ msg nid buck)
new qTrace
    qEnqueuePolicy
    qDequeuePolicy
    qFailurePolicy
    qMaxBucketSize
    (UnknownNodeType qUnknownNodeType)
  = do
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
              => Failure msg nid buck fmt -> fmt -> Text
failureFormat FailedEnqueueAll (enq, Some msg, fwdSets) =
    sformat ( "enqueue instruction " % shown
            % " failed to enqueue message " % formatMsg
            % " to forwarding sets " % shown
            )
            enq msg fwdSets
failureFormat FailedEnqueueOne (enq, Some msg, fwdSets) =
    sformat ( "enqueue instruction " % shown
            % " failed to enqueue message " % formatMsg
            % " to forwarding sets " % shown
            )
            enq msg fwdSets
failureFormat FailedAllSends (Some msg, nids) =
    sformat ( "message " % formatMsg
            % " got enqueued to " % shown
            % " but all sends failed"
            )
            msg nids
failureFormat FailedCherishLoop () =
    sformat ( "enqueueCherished loop? This a policy failure." )
failureFormat FailedChooseAlt alts =
    sformat ( "could not choose suitable alternative from " % shown )
            alts
failureFormat FailedSend (Some Packet{..}, err) =
    sformat ( "sending " % formatMsg % " to " % shown
            % " failed with " % string % " :: " % shown)
            packetPayload
            packetDestId
            (displayException err)
            (typeOf err)
failureFormat (FailedBucketFull b) () =
    sformat ( "maximum bucket size of bucket " % shown % " exceeded" )
            b

logFailure :: OutboundQ msg nid buck
           -> Failure msg nid buck fmt
           -> fmt
           -> IO ()
logFailure OutQ{..} failure fmt = do
    traceWith qTrace (failureSeverity failure, failureFormat failure fmt)
    Counter.inc $ failureCounter failure qHealth

logDebugOQ :: OutboundQ msg nid buck -> Text -> IO ()
logDebugOQ OutQ{..} txt = traceWith qTrace (Debug, txt)

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

countRecentFailures :: Failures nid -> IO Int
countRecentFailures fs = aux <$> getCurrentTime
  where
    aux :: UTCTime -> Int
    aux now = Map.size (Map.filter (isRecentFailure now) fs)

{-------------------------------------------------------------------------------
  Interpreter for the enqueing policy
-------------------------------------------------------------------------------}

-- | Enqueue a message to the specified set of peers
--
-- If no suitable peers can be found, choose one from the fallback set (if any).
intEnqueue :: forall msg nid buck a.
              OutboundQ msg nid buck
           -> MsgType nid
           -> msg a
           -> Peers nid
           -> IO [Packet msg nid a]
intEnqueue outQ@OutQ{..} msgType msg peers = fmap concat $
    forM (qEnqueuePolicy msgType) $ \case

      enq@EnqueueAll{..} -> do
        let fwdSets :: AllOf (Alts nid)
            fwdSets = removeOrigin (msgOrigin msgType) $
                        peersRoutes peers ^. routesOfType enqNodeType

            sendAll :: [Packet msg nid a]
                    -> AllOf (Alts nid)
                    -> IO [Packet msg nid a]
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
               logDebugOQ outQ $ debugNotEnqueued enqNodeType -- This isn't an error
           | null enqueued ->
               logFailure outQ FailedEnqueueAll (enq, Some msg, fwdSets)
           | otherwise ->
               logDebugOQ outQ $ debugEnqueued enqueued

        return enqueued

      enq@EnqueueOne{..} -> do
        let fwdSets :: [(NodeType, Alts nid)]
            fwdSets = concatMap
                        (\t -> map (t,) $ removeOrigin (msgOrigin msgType) $
                                            peersRoutes peers ^. routesOfType t)
                        enqNodeTypes

            -- We don't warn when choosing an alternative here, as failure to
            -- choose an alternative implies failure to enqueue for 'EnqueueOne'
            sendOne :: [(NodeType, Alts nid)] -> IO [Packet msg nid a]
            sendOne = fmap maybeToList
                    . orElseM
                    . map (sendFwdSet False [] enqMaxAhead enqPrecedence)

        enqueued <- sendOne fwdSets

        -- Log an error if we didn't manage to enqueue the message
        if null enqueued
          then logFailure outQ FailedEnqueueOne (enq, Some msg, fwdSets)
          else logDebugOQ outQ $ debugEnqueued enqueued

        return enqueued
  where
    -- Attempt to send the message to a single forwarding set
    sendFwdSet :: Bool                 -- ^ Warn on failure?
               -> [nid]                -- ^ Nodes we already sent something to
               -> MaxAhead             -- ^ Max allowed number of msgs ahead
               -> Precedence           -- ^ Precedence of the message
               -> (NodeType, Alts nid) -- ^ Alternatives to choose from
               -> IO (Maybe (Packet msg nid a))
    sendFwdSet warnOnFailure alreadyPicked maxAhead prec (nodeType, alts) = do
      mAlt <- pickAlt outQ maxAhead prec $ filter (`notElem` alreadyPicked) alts
      case mAlt of
        Nothing -> do
          when warnOnFailure $ logFailure outQ FailedChooseAlt alts
          return Nothing
        Just alt -> do
          sentVar <- newTVarIO PacketEnqueued
          let packet = Packet {
                           packetPayload  = msg
                         , packetDestId   = alt
                         , packetMsgType  = msgType
                         , packetDestType = nodeType
                         , packetPrec     = prec
                         , packetStatus   = sentVar
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
      ( "message "
      % formatMsg
      % " not enqueued to any nodes of type "
      % shown
      % " since no such (relevant) peers listed in "
      % shown
      )
      msg
      nodeType
      peers

    debugEnqueued :: [Packet msg nid a] -> Text
    debugEnqueued enqueued =
      sformat ("message " % formatMsg % " enqueued to " % shown)
              msg (map packetDestId enqueued)

-- | Node ID with current stats needed to pick a node from a list of alts
data NodeWithStats nid = NodeWithStats {
      nstatsId       :: nid  -- ^ Node ID
    , nstatsFailure  :: Bool -- ^ Recent failure?
    , nstatsAhead    :: Int  -- ^ Number of messages ahead
    , nstatsInFlight :: Int  -- ^ Number of messages in flight
    }

-- | Compute current node statistics
nodeWithStats :: OutboundQ msg nid buck
              -> Precedence -- ^ For determining number of messages ahead
              -> nid
              -> IO (NodeWithStats nid)
nodeWithStats outQ prec nstatsId = do
    (nstatsAhead, nstatsInFlight) <- countAhead outQ nstatsId prec
    nstatsFailure                 <- hasRecentFailure outQ nstatsId
    return NodeWithStats{..}

-- | Choose an appropriate node from a list of alternatives
--
-- All alternatives are assumed to be of the same type; we prefer to pick
-- nodes with a smaller number of messages ahead.
pickAlt :: forall msg nid buck.
           OutboundQ msg nid buck
        -> MaxAhead
        -> Precedence
        -> [nid]
        -> IO (Maybe nid)
pickAlt outQ@OutQ{} (MaxAhead maxAhead) prec alts = do
    alts' <- mapM (nodeWithStats outQ prec) alts
    orElseM [
        if | nstatsFailure -> do
               logDebugOQ outQ $ debugFailure nstatsId
               return Nothing
           | (nstatsAhead + nstatsInFlight) > maxAhead -> do
               logDebugOQ outQ $ debugAhead nstatsId nstatsAhead nstatsInFlight maxAhead
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
countAhead :: forall msg nid buck.
              OutboundQ msg nid buck -> nid -> Precedence -> IO (Int, Int)
countAhead outQ@OutQ{..} nid prec = do
    logDebugOQ outQ . debugInFlight =<< (readMVar qInFlight)
    (inFlight, inQueue) <- (,)
      <$> forM [prec .. maxBound] (\prec' ->
            view (inFlightWithPrec nid prec') <$> readMVar qInFlight)
      <*> forM [prec .. maxBound] (\prec' ->
            MQ.sizeBy (KeyByDestPrec nid prec') qScheduled)
    return $ (sum inQueue, sum inFlight)
  where
    debugInFlight :: InFlight nid -> Text
    debugInFlight = sformat ("inFlight = " % shown)


{-------------------------------------------------------------------------------
  Interpreter for the dequeueing policy
-------------------------------------------------------------------------------}

checkMaxInFlight :: Ord nid => DequeuePolicy -> InFlight nid -> NotBusy nid
checkMaxInFlight dequeuePolicy inFlight nodeType nid =
    sum (Map.elems (inFlight ^. inFlightTo nid)) < n
  where
    MaxInFlight n = deqMaxInFlight (dequeuePolicy nodeType)

intDequeue :: forall msg nid buck.
              OutboundQ msg nid buck
           -> ThreadRegistry
           -> SendMsg msg nid
           -> IO (Maybe CtrlMsg)
intDequeue outQ@OutQ{..} threadRegistry@TR{} sendMsg = do
    mPacket <- getPacket
    case mPacket of
      Left ctrlMsg -> return $ Just ctrlMsg
      Right packet -> sendPacket packet >> return Nothing
  where
    getPacket :: IO (Either CtrlMsg (EnqPacket msg nid))
    getPacket = retryIfNothing qSignal $ do
      inFlight    <- readMVar qInFlight
      rateLimited <- readMVar qRateLimited

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
    sendPacket :: EnqPacket msg nid -> IO ()
    sendPacket (EnqPacket p) = mdo
      -- 😱  Does this even work?
      -- We can't spawn the thread inside the STM transaction of course. So
      -- instead we lazily fill it will the new status, using the old status
      -- to determine what the new status shall be. In case the old status
      -- is PacketEnqueued, computing the new status requires forking a
      -- thread.
      -- Should be fine I think.
      oldStatus <- atomically $ do
        status <- readTVar (packetStatus p)
        writeTVar (packetStatus p) newStatus
        return status
      newStatus <- case oldStatus of
        PacketDequeued it -> do
          logDebugOQ outQ $ debugImpossible p
          return (PacketDequeued it)
        PacketAborted  -> do
          logDebugOQ outQ $ debugAborted p
          return PacketAborted
        PacketEnqueued -> do
          sendStartTime <- getCurrentTime

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
              void $ forkThread threadRegistry $ \unmask -> unmask $
                (threadDelay delay) `finally` (do
                  applyMVar_ qRateLimited $ Set.delete (packetDestId p)
                  poke qSignal)

          theThread <- forkThread threadRegistry $ \unmask -> do
            logDebugOQ outQ $ debugSending p

            finallyWithException (unmask $ sendMsg (packetPayload p) (packetDestId p)) $ \ma -> do

              -- Reduce the in-flight count ..
              setInFlightFor p (\n -> n - 1) qInFlight
              poke qSignal

              -- .. /before/ notifying the sender that the send is complete.
              -- If we did this the other way around a subsequent enqueue might fail
              -- because of policy restrictions on the max in-flight.

              case ma of
                Just err -> do
                  logFailure outQ FailedSend (Some p, err)
                  intFailure outQ p sendStartTime err
                Nothing ->
                  return ()

              logDebugOQ outQ $ debugSent p

          return (PacketDequeued theThread)
      return ()

    debugImpossible :: Packet msg nid a -> Text
    debugImpossible Packet{..} =
      sformat ("packet " % formatMsg % " to " % shown % " sent twice. This is a bug!")
              packetPayload packetDestId

    debugAborted :: Packet msg nid a -> Text
    debugAborted Packet{..} =
      sformat ("aborted " % formatMsg % " to " % shown)
              packetPayload packetDestId

    debugSending :: Packet msg nid a -> Text
    debugSending Packet{..} =
      sformat ("sending " % formatMsg % " to " % shown)
              packetPayload packetDestId

    debugSent :: Packet msg nid a -> Text
    debugSent Packet{..} =
      sformat ("sent " % formatMsg % " to " % shown)
              packetPayload packetDestId

finallyWithException :: IO a -> (Maybe SomeException -> IO x) -> IO a
finallyWithException it handler = do
  x <- it `catch` (\e -> handler (Just e) >> throwIO e)
  _ <- handler Nothing
  pure x

{-------------------------------------------------------------------------------
  Interpreter for failure policy
-------------------------------------------------------------------------------}

-- | What do we know when sending a message fails?
--
-- NOTE: Since we don't send messages to nodes listed in failures, we can
-- assume that there isn't an existing failure here.
intFailure :: forall msg nid buck a.
              OutboundQ msg nid buck
           -> Packet msg nid a  -- ^ Packet we failed to send
           -> UTCTime           -- ^ Time of the send
           -> SomeException     -- ^ The exception thrown by the send action
           -> IO ()
intFailure OutQ{..} p sendStartTime err = do
    applyMVar_ qFailures $
      Map.insert (packetDestId p) (
          sendStartTime
        , qFailurePolicy (packetDestType p)
                         (packetMsgType  p)
                         err
        )

hasRecentFailure :: OutboundQ msg nid buck -> nid -> IO Bool
hasRecentFailure OutQ{..} nid = do
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
clearRecentFailures :: OutboundQ msg nid buck -> IO ()
clearRecentFailures OutQ{..} = applyMVar_ qFailures $ const Map.empty

-- | Clear recent failure status for a particular node
--
-- This is useful when we know for external reasons that this particular
-- node is reachable again (for instance, because it sent us a message),
-- allowing the outbound queue to enqueue messages to that node again.
clearFailureOf :: OutboundQ msg nid buck -> nid -> IO ()
clearFailureOf OutQ{..} nid = applyMVar_ qFailures $ Map.delete nid

{-------------------------------------------------------------------------------
  Public interface to enqueing
-------------------------------------------------------------------------------}

-- | Queue a message to be sent, but don't wait (asynchronous API)
enqueue :: OutboundQ msg nid buck
        -> MsgType nid -- ^ Type of the message being sent
        -> msg a       -- ^ Message to send
        -> IO [(nid, TVar (PacketStatus a))]
enqueue outQ msgType msg = do
    takePacketStatus <$> intEnqueueTo outQ msgType msg (msgEnqueueTo msgType)

-- | Queue a message and wait for it to have been sent.
--
-- A 'Nothing' means the conversation was aborted before it was dequeued.
-- Otherwise, you get the 'Aync' of the thread that runs the conversation.
enqueueSync' :: OutboundQ msg nid buck
             -> MsgType nid -- ^ Type of the message being sent
             -> msg a       -- ^ Message to send
             -> IO [(nid, Maybe (Async a))]
enqueueSync' outQ msgType msg = do
    promises <- enqueue outQ msgType msg
    atomically $ traverse waitForDequeue promises
  where
    waitForDequeue :: (nid, TVar (PacketStatus a)) -> STM (nid, Maybe (Async a))
    waitForDequeue (nid, tvar) = do
      it <- readTVar tvar
      case it of
        PacketEnqueued -> retry
        PacketAborted -> pure (nid, Nothing)
        PacketDequeued thread -> pure (nid, Just thread)

-- | Queue a message and wait for it to have been sent
--
-- We wait for the message to have been sent (successfully or unsuccessfully)
-- to all the peers it got enqueued to. Like in the asynchronous API,
-- warnings will be logged when individual sends fail. Additionally, we will
-- log an error when /all/ sends failed (this doesn't currently happen in the
-- asynchronous API).
enqueueSync :: forall msg nid buck a.
               OutboundQ msg nid buck
            -> MsgType nid -- ^ Type of the message being sent
            -> msg a       -- ^ Message to send
            -> IO ()
enqueueSync outQ msgType msg =
    warnIfNotOneSuccess outQ msg $ enqueueSync' outQ msgType msg >>= waitForAll

-- | Enqueue a message which really should not get lost
--
-- Returns 'True' if the message was successfully sent.
enqueueCherished :: forall msg nid buck a.
                    OutboundQ msg nid buck
                 -> MsgType nid -- ^ Type of the message being sent
                 -> msg a       -- ^ Message to send
                 -> IO Bool
enqueueCherished outQ msgType msg =
    cherish outQ $ enqueueSync' outQ msgType msg >>= waitForAll

{-------------------------------------------------------------------------------
  Internal generalization of the enqueueing API
-------------------------------------------------------------------------------}

-- | Enqueue message to the specified set of peers
intEnqueueTo :: forall msg nid buck a.
                OutboundQ msg nid buck
             -> MsgType nid
             -> msg a
             -> EnqueueTo nid
             -> IO [Packet msg nid a]
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

takePacketStatus :: [Packet msg nid a] -> [(nid, TVar (PacketStatus a))]
takePacketStatus = map $ \Packet{..} -> (packetDestId, packetStatus)

waitForAll :: [(nid, Maybe (Async a))] -> IO [(nid, Maybe (Either SomeException a))]
waitForAll = undefined

-- | Make sure a synchronous send succeeds to at least one peer
warnIfNotOneSuccess :: forall msg nid buck a.
                       OutboundQ msg nid buck
                    -> msg a
                    -> IO [(nid, Maybe (Either SomeException a))]
                    -> IO ()
warnIfNotOneSuccess outQ msg act = do
    attempts <- act
    -- If the attempts is null, we would already have logged an error that
    -- we couldn't enqueue at all
    when (not (null attempts) && null (successes attempts)) $
      logFailure outQ FailedAllSends (Some msg, map fst attempts)

-- | Repeatedly run an action until at least one send succeeds, we run out of
-- options, or we reach a predetermined maximum number of iterations.
cherish :: forall msg nid buck a.
           OutboundQ msg nid buck
        -> IO [(nid, Maybe (Either SomeException a))]
        -> IO Bool
cherish outQ act =
    go maxNumIterations
  where
    go :: Int -> IO Bool
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

successes :: [(nid, Maybe (Either SomeException a))] -> [a]
successes = rights . mapMaybe id . map snd

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
type SendMsg msg nid = forall a. msg a -> nid -> IO a

-- | The dequeue thread
--
-- It is the responsibility of the next layer up to fork this thread; this
-- function does not return unless told to terminate using 'waitShutdown'.
dequeueThread :: forall msg nid buck.
                 OutboundQ msg nid buck -> SendMsg msg nid -> IO ()
dequeueThread outQ@OutQ{..} sendMsg = withThreadRegistry $ \threadRegistry ->
    let loop :: IO ()
        loop = do
          mCtrlMsg <- intDequeue outQ threadRegistry sendMsg
          case mCtrlMsg of
            Nothing      -> loop
            Just ctrlMsg -> do
              waitAllThreads threadRegistry
              case ctrlMsg of
                Shutdown ack -> do putMVar ack ()
                Flush    ack -> do putMVar ack ()
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
waitShutdown :: OutboundQ msg nid buck -> IO ()
waitShutdown OutQ{..} = do
    ack <- newEmptyMVar
    putMVar qCtrlMsg $ Shutdown ack
    poke qSignal
    takeMVar ack

-- | Wait for all messages currently enqueued to have been sent
flush :: OutboundQ msg nid buck -> IO ()
flush OutQ{..} = do
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
bucketSpareCapacity :: OutboundQ msg nid buck -> buck -> IO SpareCapacity
bucketSpareCapacity OutQ{..} buck = case qMaxBucketSize buck of
  BucketSizeUnlimited        -> return UnlimitedCapacity
  BucketSizeMax maxCapacity  -> do
    currentCapacity <- countPeers . fromMaybe mempty . Map.lookup buck <$> (readMVar qBuckets)
    return . SpareCapacity $ max 0 (maxCapacity - currentCapacity)

-- | Internal method: read all buckets of peers
getAllPeers :: OutboundQ msg nid buck -> IO (Peers nid)
getAllPeers OutQ{..} = fold <$> readMVar qBuckets

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
updatePeersBucket :: forall msg nid buck.
                     OutboundQ msg nid buck
                  -> buck
                  -> (Peers nid -> Peers nid)
                  -> IO Bool
updatePeersBucket outQ@OutQ{..} buck f = do
    success <- modifyMVar qBuckets $ \buckets -> do

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

data ThreadRegistry = TR (MVar (Map ThreadId (Some Async)))

-- | Create a new thread registry, killing all threads when the action
-- terminates.
withThreadRegistry :: (ThreadRegistry -> IO ()) -> IO ()
withThreadRegistry k = do
    threadRegistry <- TR <$> newMVar Map.empty
    k threadRegistry `finally` killAllThreads threadRegistry

killAllThreads :: ThreadRegistry -> IO ()
killAllThreads (TR reg) = do
    threads <- applyMVar reg $ \threads -> (Map.empty, Map.elems threads)
    mapM_ cancelSome threads
  where
    cancelSome :: Some Async -> IO ()
    cancelSome (Some it) = cancel it

waitAllThreads :: ThreadRegistry -> IO ()
waitAllThreads (TR reg) = do
    threads <- applyMVar reg $ \threads -> (Map.empty, Map.elems threads)
    mapM_ waitSome threads
  where
    waitSome :: Some Async -> IO ()
    waitSome (Some it) = void (wait it)

type Unmask = forall a. IO a -> IO a

-- | Fork a new thread, taking care of registration and unregistration
forkThread :: ThreadRegistry -> (Unmask -> IO a) -> IO (Async a)
forkThread (TR reg) threadBody = mask_ $ do
    barrier <- newEmptyMVar
    thread  <- asyncWithUnmask $ \unmask -> do
                 tid <- myThreadId
                 takeMVar barrier
                 threadBody unmask `finally`
                   applyMVar_ reg (at tid .~ Nothing)
    let tid = asyncThreadId thread
    applyMVar_ reg (at tid .~ Just (Some thread))
    putMVar barrier ()
    return thread

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
retryIfNothing :: forall a b.
                  Signal b -> IO (Maybe a) -> IO (Either b a)
retryIfNothing Signal{..} act = go
  where
    go :: IO (Either b a)
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
          mCtrlMsg <- signalCtrlMsg
          case mCtrlMsg of
            Just ctrlMsg ->
              return (Left ctrlMsg)
            Nothing -> do
              takeMVar signalPokeVar
              go

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

orElseM :: forall m a. Monad m => [m (Maybe a)] -> m (Maybe a)
orElseM = foldr aux (return Nothing)
  where
    aux :: m (Maybe a) -> m (Maybe a) -> m (Maybe a)
    aux f g = f >>= maybe g (return . Just)

applyMVar :: MVar a -> (a -> (a, b)) -> IO b
applyMVar mv f = modifyMVar mv $ \a -> return $! f a

applyMVar_ :: MVar a -> (a -> a) -> IO ()
applyMVar_ mv f = modifyMVar_ mv $ \a -> return $! f a

-- | Existential
data Some (f :: k -> *) where
  Some :: f a -> Some f
