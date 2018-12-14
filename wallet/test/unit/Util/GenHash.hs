{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util.GenHash (
    GenHash(..)
  , GenHashT -- opaque
  , runGenHashT
  , evalGenHashT
  , withHash
  ) where

import           Universum

import           Conduit (MonadResource)
import           Crypto.Random (MonadRandom (..))

import           Cardano.Wallet.Kernel.Util.StrictStateT

{-------------------------------------------------------------------------------
  Generalize over monads in which we can generate hashes
-------------------------------------------------------------------------------}

-- | Monad in which we can generate fresh hashes
class Monad m => GenHash m where
  genHash :: m Int

{-------------------------------------------------------------------------------
  Add 'GenHash' to a monad stack
-------------------------------------------------------------------------------}

newtype GenHashState = GenHashState { fromGenHashState ::Int }

-- | Add 'GenHash' capability to a monad stack
--
-- NOTE: 'GenHashT' cannot implement 'MonadUnliftIO' as it uses state under
-- the hood to keep track of fresh hashes.
newtype GenHashT m a = GenHashT {
      unGenHashT :: StrictStateT GenHashState m a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadState GenHashState
           , MonadCatch
           , MonadThrow
           , MonadRandom
           , MonadResource
           )

instance MonadTrans GenHashT where
  lift = GenHashT . lift

runGenHashT :: Monad m => GenHashT m a -> Int -> m (a, Int)
runGenHashT ma initHash =
    second fromGenHashState <$>
      runStrictStateT (unGenHashT ma) (GenHashState initHash)

evalGenHashT :: Monad m => GenHashT m a -> Int -> m a
evalGenHashT ma = fmap fst . runGenHashT ma

withHash :: Monad m => Int -> GenHashT m a -> m a
withHash = flip evalGenHashT

instance Monad m => GenHash (GenHashT m) where
  genHash = state $ \(GenHashState h) -> (h, GenHashState (h + 1))
