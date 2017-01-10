-- | Types describing runtime errors related to Block processing.

module Pos.Block.Error
       ( BlkError (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting          (bprint, int, (%))
import           Universum

import           Pos.Types           (EpochIndex)

data BlkError =
    -- | Can't apply blocks to state of transactions processing.
    BlkNoLeaders !EpochIndex
    deriving (Show)

instance Exception BlkError

instance Buildable BlkError where
    build (BlkNoLeaders epoch) =
        bprint
            ("there are no leaders for epoch " %int % " when they must be")
            epoch
