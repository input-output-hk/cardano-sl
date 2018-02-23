-- | Utilities for dealing with 'Buildable'
module Util.Buildable (
    ShowThroughBuild(..)
  ) where

import           Formatting (build, sformat)
import           Prelude (Show (..))
import           Universum

newtype ShowThroughBuild a = STB { unSTB :: a }
  deriving (Eq)

instance Buildable a => Show (ShowThroughBuild a) where
  show = toString . sformat build . unSTB
