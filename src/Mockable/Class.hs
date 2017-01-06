{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Mockable.Class ( Mockable(..) ) where

import           Control.Monad.Morph        (MFunctor (hoist))
import           Control.Monad.Trans.Reader (ReaderT (..))

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

instance (Mockable d m, MFunctor d) => Mockable d (ReaderT r m) where
    liftMockable dmt = ReaderT $ \r -> liftMockable $ hoist (flip runReaderT r) dmt
