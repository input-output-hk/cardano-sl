{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Generator.Block.Orphans
       (
       ) where

import           Universum

import qualified Control.Monad.Catch as UnsafeExc
import           Control.Monad.Random.Strict (RandT)

instance MonadThrow m => MonadThrow (RandT g m) where
    throwM = lift . UnsafeExc.throwM
