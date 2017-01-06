-- | Types describing runtime errors related to Txp.

module Pos.Update.Error
       ( USError (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting          (bprint, stext, (%))
import           Universum

data USError =
    -- | Can't apply blocks to GState.
    USCantApplyBlocks !Text
    deriving (Show)

instance Exception USError

instance Buildable USError where
    build (USCantApplyBlocks msg) = bprint ("US can't apply blocks: "%stext) msg
