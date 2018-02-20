
module Pos.Communication.Limits.Given
    (
    ) where

import           Data.Reflection (Given, given)
import           Pos.Communication.Limits (HasAdoptedBlockVersionData (..))
import           Pos.Core (BlockVersionData)

-- Orphan instance to get the adopted block version data through reflection.
instance (Given (m BlockVersionData)) => HasAdoptedBlockVersionData m where
  adoptedBVData = given
