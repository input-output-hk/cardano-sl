{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Mockable.Class
  ( Mockable (..)
  , MFunctor' (..)
  ) where

class MFunctor' f m n where
    hoist' :: (forall t . m t -> n t) -> f m t -> f n t

-- | Instances of this class identify how a given thing can be mocked within a
--   given monad.
--
--   The meaning of "mock" is very loose, as it includes what we would by
--   convention consider to be "real". For instance, "mocking" a fork in
--   IO by using 'forkIO' is a valid and reasonable instance of Mockable.
--   The point of the class is that some other monad, perhaps one which cannot
--   do any IO, could also do forking, and so by switching the monad parameter
--   we can get a different interpretation of a monadic program without changing
--   its text.
class ( Monad m ) => Mockable (d :: (* -> *) -> * -> *) (m :: * -> *) where
    liftMockable :: d m t -> m t
