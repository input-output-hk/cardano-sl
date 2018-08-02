{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeFamilies,
             FlexibleContexts, ScopedTypeVariables, GADTs, RankNTypes,
             GeneralizedNewtypeDeriving #-}
module ConsumerProtocol where

import Data.Word
import Data.List (tails, foldl')
--import Data.Maybe
import Data.Hashable
--import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQueue

import Control.Applicative
import Control.Monad
import Control.Concurrent.STM (STM, retry)
import Control.Exception (assert)
import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import System.Random (StdGen, mkStdGen, randomR)

import Test.QuickCheck

import ChainExperiment2


--
-- IPC based protocol
--


class Monad m => MonadSay m where
  say :: String -> m ()

class Monad m => MonadProbe m where
  type Probe m :: * -> *
  probeOutput :: Probe m a -> a -> m ()

class (Monad m, TimeMeasure (Time m)) => MonadTimer m where
  type Time m :: *
  timer :: Duration (Time m) -> m () -> m ()

class (Ord t, Ord (Duration t), Num (Duration t)) => TimeMeasure t where
  type Duration t :: *

  diffTime :: t -> t -> Duration t
  addTime  :: Duration t -> t -> t

class Monad m => MonadConc m where
  type MVar m :: * -> *

  fork         :: m () -> m ()
  newEmptyMVar :: m (MVar m a)
  newMVar      :: a -> m (MVar m a)
  takeMVar     :: MVar m a -> m a
  tryTakeMVar  :: MVar m a -> m (Maybe a)
  putMVar      :: MVar m a -> a -> m ()
  tryPutMVar   :: MVar m a -> a -> m Bool

  readMVar     :: MVar m a -> m a
  readMVar v = do x <- takeMVar v; putMVar v x; return x


class Monad m => MonadSendRecv m where
  type BiChan m :: * -> * -> *

  newChan :: m (BiChan m s r)
  sendMsg :: BiChan m s r -> s -> m ()
  recvMsg :: BiChan m s r -> m r

-- | In this protocol the consumer always initiates things and the producer
-- replies. This is the type of messages that the consumer sends.
data MsgConsumer = MsgRequestNext
--                 | MsgInformChain

data MsgProducer = MsgRollForward  Block
                 | MsgRollBackward Point
                 | MsgAwaitReply

{-
data ProtcolM s r a where
  ReturnP :: a -> ProtcolM s r a
  FailP   :: String -> ProtcolM s r a
  SendMsg :: s -> ProtcolM s r ()
  RecvMsg :: ProtcolM s r r
-}

consumerSideProtocol1 :: forall m.
                         (MonadSendRecv m, MonadSay m)
                      => (Block -> m ())
                      -> (Point -> m ())
                      -> BiChan m MsgConsumer MsgProducer
                      -> m ()
consumerSideProtocol1 addBlock rollbackTo chan =
    --TODO: do the protocol initiation phase
    requestNext
  where
    requestNext :: m ()
    requestNext = do
      sendMsg chan MsgRequestNext
      reply <- recvMsg chan
      handleChainUpdate reply
      requestNext

    handleChainUpdate :: MsgProducer -> m ()
    handleChainUpdate MsgAwaitReply = do
      say ("awaiting real reply")

    handleChainUpdate (MsgRollForward  b) = do
      addBlock b
      say ("ap blocks from point X to point Y")
      return ()

    handleChainUpdate (MsgRollBackward p) = do
      say ("rolling back N blocks from point X to point Y")
      return ()

producerSideProtocol1 :: forall m.
                         (MonadSendRecv m, MonadSay m)
                      => (ReaderId -> m (Maybe (ConsumeChain Block)))
                      -> (ReaderId -> m (ConsumeChain Block))
                      -> ReaderId
                      -> BiChan m MsgProducer MsgConsumer
                      -> m ()
producerSideProtocol1 tryReadChainUpdate readChainUpdate rid chan =
    --TODO: do the protocol initiation phase
    awaitRequest
  where
    awaitRequest = do
      msg <- recvMsg chan
      case msg of
        MsgRequestNext -> handleNext

    handleNext = do
      mupdate <- tryReadChainUpdate rid
      case mupdate of
        -- Reader is at the head, have to wait for producer state changes.
        Nothing -> do
          sendMsg chan MsgAwaitReply
          update <- readChainUpdate rid
          handleChainUpdate update

        Just update ->
          handleChainUpdate update

    handleChainUpdate update = do
      sendReply update
      awaitRequest

    sendReply :: ConsumeChain Block -> m ()
    sendReply (RollForward  b) = sendMsg chan (MsgRollForward b)
    sendReply (RollBackward p) = sendMsg chan (MsgRollBackward p)

--
-- Simulation of composition of producer and consumer
--


-- | Given two sides of a protocol, ...
--
simulateWire
  :: (SimChan s p c -> SimM s ())
  -> (SimChan s c p -> SimM s ())
  -> SimM s ()
simulateWire protocolSideA protocolSideB = do
    chan <- newChan
    fork $ protocolSideA chan
    fork $ protocolSideB (flipSimChan chan)
    return ()

--
-- Simulation monad for protocol testing
--

data SimM (s :: *) a where
  Return   :: a -> SimM s a
  Fail     :: String -> SimM s a

  Say      :: [String] -> SimM s b -> SimM s b
  Probe    :: SimProbe s o -> o -> SimM s b -> SimM s b

  Timer    :: VTimeDuration -> SimM s () -> SimM s b  -> SimM s b

  Fork     :: SimM s () -> SimM s b  -> SimM s b
  NewMVar  :: (SimMVar s a -> SimM s b) -> SimM s b
  TakeMVar :: SimMVar s a -> (a -> SimM s b) -> SimM s b
  PutMVar  :: SimMVar s a ->  a -> SimM s b  -> SimM s b
  TryTakeMVar :: SimMVar s a -> (Maybe a -> SimM s b) -> SimM s b
  TryPutMVar  :: SimMVar s a -> a -> (Bool -> SimM s b) -> SimM s b

data SimMVar s a = MVar (STRef s (MVarContent s a)) MVarTag
type MVarTag = Tag

newtype SimProbe s a = SimProbe (STRef s (ProbeTrace a))
type ProbeTrace a = [(VTime, a)]

newtype VTime         = VTime Int          deriving (Eq, Ord, Show)
newtype VTimeDuration = VTimeDuration Int  deriving (Eq, Ord, Show, Num, Enum, Real)

data MVarContent s a = MVarEmpty  [a -> Thread s] -- threads blocked in take
                     | MVarFull a [(a, Thread s)] -- threads blocked in put

type Action s = SimM s ()
data Thread s = Thread ThreadId (Action s)
type ThreadId = ThreadTag
type ThreadTag = Tag
type Tag = String -- for annotating the trace

instance TimeMeasure VTime where
  type Duration VTime = VTimeDuration

  diffTime (VTime t) (VTime t') = VTimeDuration (t-t')
  addTime  (VTimeDuration d) (VTime t) = VTime (t+d)


instance Functor (SimM s) where
  fmap = liftM

instance Applicative (SimM s) where
  (<*>) = ap
  pure  = return

instance Monad (SimM s) where
  return = Return

  Return       x   >>= f = f x
  Fail       msg   >>= _ = Fail msg
  Say        msg k >>= f = Say  msg (k >>= f)
  Probe      v o k >>= f = Probe v o (k >>= f)
  Timer      d a k >>= f = Timer d a (k >>= f)
  Fork         a k >>= f = Fork   a (k >>= f)
  NewMVar        k >>= f = NewMVar         (\v -> k v >>= f)
  TakeMVar    v  k >>= f = TakeMVar    v   (\x -> k x >>= f)
  TryTakeMVar v  k >>= f = TryTakeMVar v   (\x -> k x >>= f)
  PutMVar    v x k >>= f = PutMVar     v x (k >>= f)
  TryPutMVar v x k >>= f = TryPutMVar  v x (\y -> k y >>= f)

  fail msg = Fail msg

instance MonadSay (SimM s) where
  say    msg  = Say [msg] (Return ())

instance MonadProbe (SimM s) where
  type Probe (SimM s) = SimProbe s
  probeOutput p o = Probe p o (Return ())

instance MonadTimer (SimM s) where
  type Time (SimM s) = VTime
  timer t action = Timer t action (Return ())

instance MonadConc (SimM s) where
  type MVar (SimM s) = SimMVar s

  fork          task = Fork task (Return ())
  newEmptyMVar       = NewMVar Return
  newMVar          x = do mvar <- newEmptyMVar; putMVar mvar x; return mvar
  tryPutMVar  mvar x = TryPutMVar mvar x Return
  tryTakeMVar mvar   = TryTakeMVar mvar Return
  takeMVar    mvar   = TakeMVar mvar Return
  putMVar     mvar x = PutMVar  mvar x (Return ())

instance MonadSendRecv (SimM s) where
  type BiChan (SimM s) = SimChan s

  newChan = SimChan <$> newEmptyMVar <*> newEmptyMVar
  sendMsg (SimChan  s _r) = putMVar  s
  recvMsg (SimChan _s  r) = takeMVar r

data SimChan s send recv = SimChan (SimMVar s send) (SimMVar s recv)

flipSimChan (SimChan unichanAB unichanBA) = SimChan unichanBA unichanAB

data SimState s = SimState {
       runqueue :: ![Thread s],
       curTime  :: !VTime,
       timers   :: !(PQueue VTime (Action s)),
       prng     :: !StdGen
     }

--
-- Simulator interpreter
--

type Trace = [(VTime, ThreadId, TraceEvent)]

data TraceEvent = EventFail String
                | EventSay [String]
                | EventTimerCreated VTime
                | EventTimerExpired
                | EventThreadForked ThreadId
                | EventThreadStopped
                | EventCreatedMVar         MVarTag
                | EventNonBlockingTookMVar MVarTag
                | EventFailedTryTakeMVar   MVarTag
                | EventBlockedOnTakeMVar   MVarTag
                | EventUnblockedTakeMVar   MVarTag
                | EventNonBlockingPutMVar  MVarTag
                | EventFailedTryPutMVar    MVarTag
                | EventBlockedOnPutMVar    MVarTag
                | EventUnblockedPutMVar    MVarTag
  deriving Show

type PrngSeed = Int

newProbe :: ST s (SimProbe s a)
newProbe = SimProbe <$> newSTRef []

readProbe :: SimProbe s a -> ST s (ProbeTrace a)
readProbe (SimProbe p) = reverse <$> readSTRef p

runSimM :: PrngSeed -> (forall s. SimM s ()) -> Trace
runSimM prngseed initialThread = runST (runSimMST prngseed initialThread)

runSimMST :: forall s. PrngSeed -> SimM s () -> ST s Trace
runSimMST prngseed initialThread = schedule initialState
  where
    initialState :: SimState s
    initialState = SimState [Thread "main" initialThread]
                            (VTime 0)
                            PQueue.empty
                            (mkStdGen prngseed)

schedule :: SimState s -> ST s Trace

-- at least one runnable thread, run it one step
schedule simstate@SimState {
           runqueue = Thread tid action:remaining,
           curTime  = time, timers, prng
         } =
  case action of

    Return () -> do
      -- this thread is done
      trace <- schedule simstate { runqueue = remaining }
      return ((time,tid,EventThreadStopped):trace)

    Fail msg -> do
      -- stop the whole sim on failure
      return ((time,tid,EventFail msg):[])

    Say msg k -> do
      let thread' = Thread tid k
      trace <- schedule simstate { runqueue = thread':remaining }
      return ((time,tid,EventSay msg):trace)

    Probe (SimProbe p) o k -> do
      modifySTRef p ((time, o):)
      let thread' = Thread tid k
      schedule simstate { runqueue = thread':remaining }

    Timer t a k -> do
      let expiry  = t `addTime` time
          timers' = PQueue.insert expiry a timers
          thread' = Thread tid k
      trace <- schedule simstate { runqueue = thread':remaining
                                 , timers   = timers' }
      return ((time,tid,EventTimerCreated expiry):trace)

    Fork a k -> do
      let thread'  = Thread tid  k
          tid'     = "TODO"
          thread'' = Thread tid' a
      trace <- schedule simstate { runqueue = thread':thread'':remaining }
      return ((time,tid,EventThreadForked tid'):trace)

    NewMVar k -> do
      v <- newSTRef (MVarEmpty [])
      let vtag    = "TODO"
          thread' = Thread tid (k (MVar v vtag))
      trace <- schedule simstate { runqueue = thread':remaining }
      return ((time,tid,EventCreatedMVar vtag):trace)

    PutMVar (MVar v vtag) x k -> do
      ms <- readSTRef v
      case ms of
        MVarEmpty (t:ts) -> do
          -- there's another thread waiting to take the value, so we pass it
          -- to that thread, wake that thread up, and the MVar remains empty
          writeSTRef v (MVarEmpty ts)
          let thread'  = Thread tid k
              thread''@(Thread tid' _) = t x
          trace <- schedule simstate { runqueue = thread':thread'':remaining }
          return ((time,tid,EventNonBlockingPutMVar vtag)
                 :(time,tid',EventUnblockedTakeMVar vtag):trace)

        MVarEmpty [] -> do
          -- no threads waiting to take the value, so we fill it and move on
          writeSTRef v (MVarFull x [])
          let thread' = Thread tid k
          trace <- schedule simstate { runqueue = thread':remaining }
          return ((time,tid,EventNonBlockingPutMVar vtag):trace)

        MVarFull x0 ts -> do
          -- already full, so we add ourself to the end of the blocked list
          writeSTRef v (MVarFull x0 (ts ++ [(x, Thread tid k)]))
          trace <- schedule simstate { runqueue = remaining }
          return ((time,tid,EventBlockedOnPutMVar vtag):trace)

    TryPutMVar (MVar v vtag) x k -> do
      ms <- readSTRef v
      case ms of
        MVarEmpty (t:ts) -> do
          -- there's another thread waiting to take the value, so we pass it
          -- to that thread, wake that thread up, and the MVar remains empty
          writeSTRef v (MVarEmpty ts)
          let thread'  = Thread tid (k True)
              thread''@(Thread tid' _) = t x
          trace <- schedule simstate { runqueue = thread':thread'':remaining }
          return ((time,tid,EventNonBlockingPutMVar vtag)
                 :(time,tid',EventUnblockedTakeMVar vtag):trace)

        MVarEmpty [] -> do
          -- no threads waiting to take the value, so we fill it and move on
          writeSTRef v (MVarFull x [])
          let thread' = Thread tid (k True)
          trace <- schedule simstate { runqueue = thread':remaining }
          return ((time,tid,EventNonBlockingPutMVar vtag):trace)

        MVarFull _x0 _ts -> do
          -- already full, but this is non-blocking tryPut so we return
          let thread' = Thread tid (k False)
          trace <- schedule simstate { runqueue = thread':remaining }
          return ((time,tid,EventFailedTryPutMVar vtag):trace)

    TakeMVar (MVar v vtag) k -> do
      ms <- readSTRef v
      case ms of
        MVarFull x ((x', t):ts) -> do
          -- there's another thread waiting to put a value, so we re-fill the
          -- MVar with that value, and let the other thread go on its way
          writeSTRef v (MVarFull x' ts)
          let thread'  = Thread tid (k x)
              thread''@(Thread tid' _) = t
          trace <- schedule simstate { runqueue = thread':thread'':remaining }
          return ((time,tid,EventNonBlockingTookMVar vtag)
                 :(time,tid',EventUnblockedPutMVar vtag):trace)

        MVarFull x [] -> do
          -- no threads waiting to take the value, so we grab the value and
          -- leave the MVar empty
          writeSTRef v (MVarEmpty [])
          let thread' = Thread tid (k x)
          trace <- schedule simstate { runqueue = thread':remaining }
          return ((time,tid,EventNonBlockingTookMVar vtag):trace)

        MVarEmpty ts -> do
          -- already empty, so we add ourself to the end of the blocked list
          writeSTRef v (MVarEmpty (ts ++ [\x -> Thread tid (k x)]))
          trace <- schedule simstate { runqueue = remaining }
          return ((time,tid,EventBlockedOnTakeMVar vtag):trace)

    TryTakeMVar (MVar v vtag) k -> do
      ms <- readSTRef v
      case ms of
        MVarFull x ((x', t):ts) -> do
          -- there's another thread waiting to put a value, so we re-fill the
          -- MVar with that value, and let the other thread go on its way
          writeSTRef v (MVarFull x' ts)
          let thread'  = Thread tid (k (Just x))
              thread''@(Thread tid' _) = t
          trace <- schedule simstate { runqueue = thread':thread'':remaining }
          return ((time,tid,EventNonBlockingTookMVar vtag)
                 :(time,tid',EventUnblockedPutMVar vtag):trace)

        MVarFull x [] -> do
          -- no threads waiting to take the value, so we grab the value and
          -- leave the MVar empty
          writeSTRef v (MVarEmpty [])
          let thread' = Thread tid (k (Just x))
          trace <- schedule simstate { runqueue = thread':remaining }
          return ((time,tid,EventNonBlockingTookMVar vtag):trace)

        MVarEmpty _ts -> do
          -- already empty, but this is non-blocking tryTake so we return
          let thread' = Thread tid (k Nothing)
          trace <- schedule simstate { runqueue = thread':remaining }
          return ((time,tid,EventFailedTryTakeMVar vtag):trace)

-- no runnable threads, advance the time to the next timer event, or stop.
schedule simstate@(SimState [] time timers _) =
    -- important to get all events that expire at this time
    case removeMinimums timers of
      Nothing -> return []

      Just (time', events, timers') -> assert (time' > time) $ do
        trace <- schedule simstate { runqueue = map (Thread "timer") events
                                   , curTime  = time'
                                   , timers   = timers' }
        return ((time', "timer", EventTimerExpired):trace)

removeMinimums :: Ord k => PQueue k a -> Maybe (k, [a], PQueue k a)
removeMinimums = \pqueue ->
    case PQueue.minViewWithKey pqueue of
      Nothing                -> Nothing
      Just ((k, x), pqueue') -> Just (collectAll k [x] pqueue')
  where
    collectAll k xs pqueue =
      case PQueue.minViewWithKey pqueue of
        Just ((k', x'), pqueue')
          | k == k' -> collectAll k (x':xs) pqueue'
        _           -> (k, reverse xs, pqueue)


example0 :: (MonadSay m, MonadTimer m, MonadConc m) => m ()
example0 = do
  say "starting"
  v <- newEmptyMVar
  timer 2 $ do
    say "timer fired!"
    putMVar v ()
  say "waiting on mvar"
  takeMVar v
  say "main done"

example1 :: [a] -> ST s (Trace, ProbeTrace a)
example1 xs = do
    p <- newProbe
    trace <- runSimMST prngseed $ do
      v <- newEmptyMVar
      fork (producer v)
      fork (consumer v p)
    ptrace <- readProbe p
    return (trace, ptrace)
  where
    prngseed   = 0
    producer v =
      sequence_ [ timer delay $ putMVar v x
                | (delay, x) <- zip [1..] xs ]

    consumer v p = forever $ do
      x <- takeMVar v
      probeOutput p x

example2 :: TestChainAndUpdates -> SimM s ()
example2 = \(TestChainAndUpdates _c us) -> do
    chainvar <- newEmptyMVar
    fork $ generator chainvar us
    simulateWire (producer chainvar) consumer
  where
    generator chainvar us =
      sequence_
        [ timer d $ do
            case u of
              AddBlock b -> putMVar chainvar (RollForward b)
              SwitchFork _n bs -> do
                putMVar chainvar (RollBackward (0, 0)) --TODO dummy
                mapM_ (putMVar chainvar . RollForward) bs
        | (d, u) <- zip [1..] us ]

    producer :: SimMVar s (ConsumeChain Block)
             -> SimChan s MsgProducer MsgConsumer
             -> SimM s ()
    producer chainvar =
      producerSideProtocol1
        (\_rid -> tryTakeMVar chainvar)
        (\_rid -> takeMVar chainvar)
        0 --TODO: dummy rid

    consumer :: SimChan s MsgConsumer MsgProducer
             -> SimM s ()
    consumer =
      consumerSideProtocol1
        (\b -> say ("addBlock " ++ show b))
        (\p -> say ("rollBackTo " ++ show p))

--
-- STM based protocol
--

-- | An STM-based interface provided by a chain producer to chain consumers.
--
data ChainProducer = ChainProducer {
       establishChainConsumer :: [Point]
                              -> STM (ChainConsumer, [Point])
     }

data ChainConsumer = ChainConsumer {
       currentReadPoint :: STM Point,
       improveReadPoint :: [Point] -> STM (),
       tryPeekChain     :: STM (Maybe (ConsumeChain Block)),
       tryReadChain     :: STM (Maybe (ConsumeChain Block))
     }

type MaxReadBlocks = Int

readRollForwardOnly :: ChainConsumer -> MaxReadBlocks -> STM [Block]
readRollForwardOnly ChainConsumer{tryPeekChain, tryReadChain} maxBlocks =
    go maxBlocks
  where 
    go 0 = return []
    go n = do
      res <- tryPeekChain
      case res of
        Just (RollForward b) -> do
          _ <- tryReadChain
          bs <- go (n-1)
          return (b:bs)
        _ -> return []

-- | Like 'tryReadChain' but reads multiple blocks in one go.
--
tryReadChainN :: ChainConsumer
              -> MaxReadBlocks -- ^ The maximum number of blocks to read
              -> STM (Maybe (ConsumeChain [Block]))
tryReadChainN cs@ChainConsumer{..} maxBlocks = do
    res <- tryReadChain
    case res of
      -- If we're at the chain head or it's a rollback we just return that.
      Nothing               -> return Nothing
      Just (RollBackward p) -> return (Just (RollBackward p))
      -- If we get one block we peek at what's ahead and consume any
      -- more blocks, up to our limit.
      Just (RollForward b) -> do
        bs <- readRollForwardOnly cs (maxBlocks-1)
        return (Just (RollForward (b:bs)))

-- | Like 'tryReadChainN' but blocks at the chain head.
--
readChainN :: ChainConsumer
           -> MaxReadBlocks -- ^ The maximum number of blocks to read
           -> STM (ConsumeChain [Block])
readChainN cs@ChainConsumer{..} maxBlocks = do
    res <- tryReadChain
    case res of
      -- If it's the chain head we block by retrying.
      Nothing                 -> retry
      -- If it's a rollback we just return that.
      Just (RollBackward p) -> return (RollBackward p)
      -- If we get one block we peek at what's ahead and consume any
      -- more blocks, up to our limit.
      Just (RollForward b) -> do
        bs <- readRollForwardOnly cs (maxBlocks-1)
        return (RollForward (b:bs))

