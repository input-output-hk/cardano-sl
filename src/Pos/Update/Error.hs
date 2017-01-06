-- | Types describing runtime errors related to Txp.

module Pos.Update.Error
       ( USError (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting          (bprint, stext, (%))
import           Universum

import           Pos.Crypto          (shortHashF)
import           Pos.Types           (StakeholderId)

data USError
    = USCantApplyBlocks !Text
     -- ^ Can't apply blocks to GState.
    | USNotRichmen StakeholderId
     -- ^ Voter from applied block is not richman.
    deriving (Show)

instance Exception USError

instance Buildable USError where
    build (USCantApplyBlocks msg) = bprint ("US can't apply blocks: "%stext) msg
    build (USNotRichmen id) =
        bprint ("attempt to apply block with vote from not richman: "%shortHashF)
        id
