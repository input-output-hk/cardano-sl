-- | Communication-specific utility functions.

{-# LANGUAGE RankNTypes #-}

module Pos.Communication.Util
       ( Action
       , ActionSpec (..)
       , mapActionSpec
       , toAction
       , localSpecs
       ) where

import           Universum

import           Pos.Communication.Protocol (OutSpecs)
import           Pos.Diffusion.Types (Diffusion)

type Action m a = Diffusion m -> m a

newtype ActionSpec m a = ActionSpec (Action m a)

mapActionSpec
    :: (Diffusion m -> Diffusion m)
    -> (forall t. m t -> m t) -> ActionSpec m a -> ActionSpec m a
mapActionSpec saMapper aMapper (ActionSpec f) =
    ActionSpec $ \sA -> aMapper $ f (saMapper sA)

toAction
    :: (Diffusion m -> m a) -> ActionSpec m a
toAction h = ActionSpec $ h

localSpecs :: m a -> (ActionSpec m a, OutSpecs)
localSpecs h = (ActionSpec $ \__sA -> h, mempty)
