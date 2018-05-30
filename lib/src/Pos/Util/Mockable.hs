{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for mockable stuff.

module Pos.Util.Mockable
       (
       ) where

import           Universum

import           Control.Monad.Morph (MFunctor (..))
import           Control.Monad.Trans.Identity (IdentityT (..))
import qualified Ether
import           Mockable (ChannelT, Counter, Distribution, Gauge, MFunctor' (..), Mockable (..),
                           Promise, SharedAtomicT, SharedExclusiveT, ThreadId)

instance {-# OVERLAPPABLE #-}
    (Monad m, MFunctor t) => MFunctor' t m n
  where
    hoist' = hoist

instance
    (Mockable d m, MFunctor' d (IdentityT m) m) =>
        Mockable d (IdentityT m)
  where
    liftMockable dmt = IdentityT $ liftMockable $ hoist' runIdentityT dmt

unTaggedTrans :: Ether.TaggedTrans tag t m a -> t m a
unTaggedTrans (Ether.TaggedTrans tma) = tma

instance
      (Mockable d (t m), Monad (t m),
       MFunctor' d (Ether.TaggedTrans tag t m) (t m)) =>
          Mockable d (Ether.TaggedTrans tag t m)
  where
    liftMockable dmt =
      Ether.TaggedTrans $ liftMockable $ hoist' unTaggedTrans dmt

type instance ThreadId (IdentityT m) = ThreadId m
type instance Promise (IdentityT m) = Promise m
type instance SharedAtomicT (IdentityT m) = SharedAtomicT m
type instance Counter (IdentityT m) = Counter m
type instance Distribution (IdentityT m) = Distribution m
type instance SharedExclusiveT (IdentityT m) = SharedExclusiveT m
type instance Gauge (IdentityT m) = Gauge m
type instance ChannelT (IdentityT m) = ChannelT m

type instance ThreadId (Ether.TaggedTrans tag t m) = ThreadId m
type instance Promise (Ether.TaggedTrans tag t m) = Promise m
type instance SharedAtomicT (Ether.TaggedTrans tag t m) = SharedAtomicT m
type instance Counter (Ether.TaggedTrans tag t m) = Counter m
type instance Distribution (Ether.TaggedTrans tag t m) = Distribution m
type instance SharedExclusiveT (Ether.TaggedTrans tag t m) = SharedExclusiveT m
type instance Gauge (Ether.TaggedTrans tag t m) = Gauge m
type instance ChannelT (Ether.TaggedTrans tag t m) = ChannelT m
