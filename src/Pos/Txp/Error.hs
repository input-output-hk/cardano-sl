-- | Types describing runtime errors related to Txp.

module Pos.Txp.Error
       ( TxpError (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting          (bprint, stext, (%))
import           Universum

data TxpError =
    -- | Can't apply blocks to state of transactions processing.
    TxpCantApplyBlocks Text
    deriving (Show)

instance Exception TxpError

instance Buildable TxpError where
    build (TxpCantApplyBlocks msg) = bprint ("txp can't apply blocks: "%stext) msg
