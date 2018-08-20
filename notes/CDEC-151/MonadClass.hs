{-# LANGUAGE MultiParamTypeClasses, TypeFamilies,
             FlexibleContexts #-}
module MonadClass (
  MonadSay(..),
  MonadProbe(..),
  TimeMeasure(..),
  MonadTimer(..),
  MonadConc(..),
  MonadSendRecv(..),
  ) where


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

  modifyMVar   :: MVar m a -> (a -> m (a, b)) -> m b
  modifyMVar v a = do
    x <- takeMVar v
    (x', y) <- a x
    putMVar v x'
    return y

  modifyMVar_   :: MVar m a -> (a -> m a) -> m ()
  modifyMVar_ v a = do
    x  <- takeMVar v
    x' <- a x
    putMVar v x'

class Monad m => MonadSendRecv m where
  type BiChan m :: * -> * -> *

  newChan :: m (BiChan m s r)
  sendMsg :: BiChan m s r -> s -> m ()
  recvMsg :: BiChan m s r -> m r

