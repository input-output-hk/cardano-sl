-- | Utilities for dealing with 'Buildable'
module Util.Buildable (
    ShowThroughBuild(..)
  ) where

import           Formatting (bprint, sformat)
import qualified Formatting as F
import           Formatting.Buildable (Buildable (..))
import           Prelude (Show (..))
import           Test.QuickCheck (Arbitrary (..))
import           Universum

newtype ShowThroughBuild a = STB { unSTB :: a }
  deriving (Eq, Ord)

instance Buildable a => Buildable (ShowThroughBuild a) where
  build = bprint F.build . unSTB

instance Buildable a => Show (ShowThroughBuild a) where
  show = toString . sformat F.build . unSTB

instance Arbitrary a => Arbitrary (ShowThroughBuild a) where
  arbitrary = STB <$> arbitrary
