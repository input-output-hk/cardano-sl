{-# LANGUAGE TypeFamilies, ScopedTypeVariables, RankNTypes,
             GADTSyntax, ExistentialQuantification, 
             NamedFieldPuns, GeneralizedNewtypeDeriving #-}
module Sim (
  SimM,
  SimProbe,
  Chan (..),
  flipChan,
  newProbe,
  runSimM,
  runSimMST,
  ) where

import           Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQueue

import Control.Applicative
import Control.Monad
import Control.Exception (assert)
import Control.Monad.ST.Lazy
import Data.STRef.Lazy
import System.Random (StdGen, mkStdGen)


import           MonadClass hiding (MVar)
import qualified MonadClass

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
  NewMVar  :: (MVar s a -> SimM s b) -> SimM s b
  TakeMVar :: MVar s a -> (a -> SimM s b) -> SimM s b
  PutMVar  :: MVar s a ->  a -> SimM s b  -> SimM s b
  TryTakeMVar :: MVar s a -> (Maybe a -> SimM s b) -> SimM s b
  TryPutMVar  :: MVar s a -> a -> (Bool -> SimM s b) -> SimM s b

data MVar s a = MVar (STRef s (MVarContent s a)) MVarTag
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
  type MVar (SimM s) = MVar s

  fork          task = Fork task (Return ())
  newEmptyMVar       = NewMVar Return
  newMVar          x = do mvar <- newEmptyMVar; putMVar mvar x; return mvar
  tryPutMVar  mvar x = TryPutMVar mvar x Return
  tryTakeMVar mvar   = TryTakeMVar mvar Return
  takeMVar    mvar   = TakeMVar mvar Return
  putMVar     mvar x = PutMVar  mvar x (Return ())

instance MonadSendRecv (SimM s) where
  type BiChan (SimM s) = Chan s

  newChan = Chan <$> newEmptyMVar <*> newEmptyMVar
  sendMsg (Chan  s _r) = putMVar  s
  recvMsg (Chan _s  r) = takeMVar r

data Chan s send recv = Chan (MVar s send) (MVar s recv)

flipChan :: Chan s recv send -> Chan s send recv
flipChan (Chan unichanAB unichanBA) = Chan unichanBA unichanAB

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
    prngseed   = mkStdGen 0
    producer v =
      sequence_ [ timer delay $ putMVar v x
                | (delay, x) <- zip [1..] xs ]

    consumer v p = forever $ do
      x <- takeMVar v
      probeOutput p x

