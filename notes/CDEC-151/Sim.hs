{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Sim (
  SimF,
  SimM,
  SimProbe,
  SimChan (..),
  SimMVar,
  flipSimChan,
  newProbe,
  runSimM,
  runSimMST,

  example0,
  example1
  ) where

import           Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQueue

-- import Control.Applicative
import           Control.Exception (assert)
import           Control.Monad
import           Control.Monad.Free (Free)
import           Control.Monad.Free as Free
import           Control.Monad.ST.Lazy
import           Data.STRef.Lazy
import           System.Random (StdGen, mkStdGen)


import           MonadClass hiding (MVar)
import qualified MonadClass

--
-- Simulation monad for protocol testing
--
data SimF (s :: *) a where
  Fail         :: String -> SimF s a

  Say          :: [String] -> b -> SimF s b
  Probe        :: SimProbe s o -> o -> b -> SimF s b

  Timer        :: VTimeDuration -> Free (SimF s) () -> b -> SimF s b

  Fork         :: Free (SimF s) () -> b -> SimF s b
  NewEmptyMVar :: (SimMVar s a -> b) -> SimF s b
  TakeMVar     :: SimMVar s a -> (a -> b) -> SimF s b
  PutMVar      :: SimMVar s a ->  a -> b  -> SimF s b
  TryTakeMVar  :: SimMVar s a -> (Maybe a -> b) -> SimF s b
  TryPutMVar   :: SimMVar s a -> a -> (Bool -> b) -> SimF s b

instance Functor (SimF s) where
    fmap _ (Fail f)           = Fail f
    fmap f (Say ss b)         = Say ss $ f b
    fmap f (Probe p o b)      = Probe p o $ f b
    fmap f (Timer d s b)      = Timer d s $ f b
    fmap f (Fork s b)         = Fork s $ f b
    fmap f (NewEmptyMVar k)   = NewEmptyMVar (f . k)
    fmap f (TakeMVar v k)     = TakeMVar v (f . k)
    fmap f (PutMVar v a b)    = PutMVar v a $ f b
    fmap f (TryTakeMVar v k)  = TryTakeMVar v (f . k)
    fmap f (TryPutMVar v a k) = TryPutMVar v a (f . k)

type SimM s a = Free (SimF s) a

data SimMVar s a = SimMVar (STRef s (MVarContent s a)) MVarTag
type MVarTag = Tag

newtype SimProbe s a = SimProbe (STRef s (ProbeTrace a))
type ProbeTrace a = [(VTime, a)]

newtype VTime         = VTime Int          deriving (Eq, Ord, Show)
newtype VTimeDuration = VTimeDuration Int  deriving (Eq, Ord, Show, Num, Enum, Real)

data MVarContent s a
    = MVarEmpty  [a -> Thread s] -- threads blocked in take
    | MVarFull a [(a, Thread s)] -- threads blocked in put

type Action s = Free (SimF s) ()
data Thread s = Thread ThreadId (Action s)
type ThreadId = ThreadTag
type ThreadTag = Tag
type Tag = String -- for annotating the trace

instance TimeMeasure VTime where
  type Duration VTime = VTimeDuration

  diffTime (VTime t) (VTime t') = VTimeDuration (t-t')
  addTime  (VTimeDuration d) (VTime t) = VTime (t+d)

instance MonadSay (Free (SimF s)) where
  say msg = Free.liftF $ Say [msg] ()

instance MonadProbe (Free (SimF s)) where
  type Probe (Free (SimF s)) = SimProbe s
  probeOutput p o = Free.liftF $ Probe p o ()

instance MonadTimer (Free (SimF s)) where
  type Time (Free (SimF s)) = VTime
  timer t action = Free.liftF $ Timer t action ()

instance MonadConc (Free (SimF s)) where
  type MVar (Free (SimF s)) = SimMVar s

  fork          task = Free.liftF $ Fork task ()
  newEmptyMVar       = Free.liftF $ NewEmptyMVar id
  newMVar          x = do mvar <- newEmptyMVar; putMVar mvar x; return mvar
  tryPutMVar  mvar x = Free.liftF $ TryPutMVar mvar x id
  tryTakeMVar mvar   = Free.liftF $ TryTakeMVar mvar id
  takeMVar    mvar   = Free.liftF $ TakeMVar mvar id
  putMVar     mvar x = Free.liftF $ PutMVar  mvar x ()

instance MonadSendRecv (Free (SimF s)) where
  type BiChan (Free (SimF s)) = SimChan s

  newChan = SimChan <$> newEmptyMVar <*> newEmptyMVar
  sendMsg (SimChan  s _r) = putMVar  s
  recvMsg (SimChan _s  r) = takeMVar r

data SimChan s send recv = SimChan (SimMVar s send) (SimMVar s recv)

flipSimChan :: SimChan s recv send -> SimChan s send recv
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

newProbe :: ST s (SimProbe s a)
newProbe = SimProbe <$> newSTRef []

readProbe :: SimProbe s a -> ST s (ProbeTrace a)
readProbe (SimProbe p) = reverse <$> readSTRef p

runSimM :: StdGen -> (forall s. SimM s ()) -> Trace
runSimM prng initialThread = runST (runSimMST prng initialThread)

runSimMST :: forall s. StdGen -> SimM s () -> ST s Trace
runSimMST prng initialThread = schedule initialState
  where
    initialState :: SimState s
    initialState = SimState [Thread "main" initialThread]
                            (VTime 0)
                            PQueue.empty
                            prng

schedule :: SimState s -> ST s Trace

-- at least one runnable thread, run it one step
schedule simstate@SimState {
           runqueue = Thread tid action:remaining,
           curTime  = time, timers
         } =
  case action of

    Pure () -> do
      -- this thread is done
      trace <- schedule simstate { runqueue = remaining }
      return ((time,tid,EventThreadStopped):trace)

    Free (Fail msg) -> do
      -- stop the whole sim on failure
      return [(time,tid,EventFail msg)]

    Free (Say msg k) -> do
      let thread' = Thread tid k
      trace <- schedule simstate { runqueue = thread':remaining }
      return ((time,tid,EventSay msg):trace)

    Free (Probe (SimProbe p) o k) -> do
      modifySTRef p ((time, o):)
      let thread' = Thread tid k
      schedule simstate { runqueue = thread':remaining }

    Free (Timer t a k) -> do
      let expiry  = t `addTime` time
          timers' = PQueue.insert expiry a timers
          thread' = Thread tid k
      trace <- schedule simstate { runqueue = thread':remaining
                                 , timers   = timers' }
      return ((time,tid,EventTimerCreated expiry):trace)

    Free (Fork a k) -> do
      let thread'  = Thread tid  k
          tid'     = "TODO"
          thread'' = Thread tid' a
      trace <- schedule simstate { runqueue = thread':thread'':remaining }
      return ((time,tid,EventThreadForked tid'):trace)

    Free (NewEmptyMVar k) -> do
      v <- newSTRef (MVarEmpty [])
      let vtag    = "TODO"
          thread' = Thread tid (k (SimMVar v vtag))
      trace <- schedule simstate { runqueue = thread':remaining }
      return ((time,tid,EventCreatedMVar vtag):trace)

    Free (PutMVar (SimMVar v vtag) x k) -> do
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

    Free (TryPutMVar (SimMVar v vtag) x k) -> do
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

    Free (TakeMVar (SimMVar v vtag) k) -> do
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

    Free (TryTakeMVar (SimMVar v vtag) k) -> do
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

example1 :: forall a s. [a] -> ST s (Trace, ProbeTrace a)
example1 xs = do
    p <- newProbe
    trace <- runSimMST prngseed $ do
      v <- newEmptyMVar
      fork (producer v)
      fork (consumer v p)
    ptrace <- readProbe p
    return (trace, ptrace)
  where
    prngseed   = mkStdGen 0

    producer :: SimMVar s a
             -> Free (SimF s) ()
    producer v =
      sequence_ [ timer delay $ putMVar v x
                | (delay, x) <- zip [1..] xs ]

    consumer
        :: SimMVar s a
        -> Probe (Free (SimF s)) a
        -> Free (SimF s) ()
    consumer v p = forever $ do
      x <- takeMVar v
      probeOutput p x

