{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Mockable.Class
  ( Mockable (..)
  , Mockables
  , MFunctor' (..)
  ) where

import           Data.Kind (Constraint)

class MFunctor' f m n where
    hoist' :: (forall a . m a -> n a) -> f m t -> f n t

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

type family Mockables (m :: * -> *) (ds :: [(* -> *) -> * -> *]) where
    Mockables m '[]    = (() :: Constraint)
    Mockables m (d:ds) = (Mockable d m, Mockables m ds)
