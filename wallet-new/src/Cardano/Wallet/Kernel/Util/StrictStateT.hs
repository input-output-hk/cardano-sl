{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Cardano.Wallet.Kernel.Util.StrictStateT (
    StrictStateT -- opaque
  , runStrictStateT
  , evalStrictStateT
  , strictStateT
    -- * Conduit support
  , strictStateC
  , execStrictStateC
  ) where

import           Universum

import           Conduit
import           Control.Monad.Except (MonadError)
import           Crypto.Random (MonadRandom (..))
import           Data.Conduit.Internal (ConduitT (..), Pipe (..))

-- | Drop-in replacement for 'StateT' that updates the state strictly.
newtype StrictStateT s m a = StrictStateT {
    unStrictStateT :: StateT s m a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadTrans
           , MonadError e
           , MonadCatch
           , MonadThrow
           , MonadIO
           , MonadResource
           )

instance MonadRandom m => MonadRandom (StrictStateT s m) where
  getRandomBytes = lift . getRandomBytes

instance Monad m => MonadState s (StrictStateT s m) where
  get    = StrictStateT $ get
  put !s = StrictStateT $ put s

runStrictStateT :: StrictStateT s m a -> s -> m (a, s)
runStrictStateT = runStateT . unStrictStateT

evalStrictStateT :: Monad m => StrictStateT s m a -> s -> m a
evalStrictStateT = evalStateT . unStrictStateT

strictStateT :: forall s m a. Monad m => (s -> m (a, s)) -> StrictStateT s m a
strictStateT f = StrictStateT $ StateT f'
  where
    f' :: s -> m (a, s)
    f' s = do (a, !s') <- f s ; return (a, s')

{-------------------------------------------------------------------------------
  Conduit support

  These are straight-forward adaptations of the code in Data.Conduit.Lift.
-------------------------------------------------------------------------------}

strictStateC :: Monad m
             => (s -> ConduitT i o m (a, s))
             -> ConduitT i o (StrictStateT s m) a
strictStateC k = do
    s <- lift get
    (r, s') <- transPipe lift (k s)
    lift $ put s'
    return r
{-# INLINABLE strictStateC #-}

runStrictStateC :: Monad m
                => s
                -> ConduitT i o (StrictStateT s m) r
                -> ConduitT i o m (r, s)
runStrictStateC = thread (,) runStrictStateT
{-# INLINABLE runStrictStateC #-}

execStrictStateC :: Monad m
                 => s
                 -> ConduitT i o (StrictStateT s m) r
                 -> ConduitT i o m s
execStrictStateC s p = fmap snd $ runStrictStateC s p
{-# INLINABLE execStrictStateC #-}

{-------------------------------------------------------------------------------
  Copied straight from Data.Conduit.Lift (where it's not exported)
-------------------------------------------------------------------------------}

thread :: Monad m
       => (r -> s -> res)
       -> (forall a. t m a -> s -> m (a, s))
       -> s
       -> ConduitT i o (t m) r
       -> ConduitT i o m res
thread toRes runM s0 (ConduitT c0) =
    ConduitT $ \rest ->
        let go s (Done r) = rest (toRes r s)
            go s (PipeM mp) = PipeM $ do
                (p, s') <- runM mp s
                return $ go s' p
            go s (Leftover p i) = Leftover (go s p) i
            go s (NeedInput x y) = NeedInput (go s . x) (go s . y)
            go s (HaveOutput p o) = HaveOutput (go s p) o
         in go s0 (c0 Done)
{-# INLINABLE thread #-}
