module Pos.Lrc.Error
       (
         LrcError (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting          (bprint, build, int, stext, (%))
import           Universum

import           Pos.Types           (EpochIndex)

data LrcError
    = LrcDataUnknown !EpochIndex !Text
    | UnknownBlocksForLrc
    | LrcAfterGenesis
    | NoRichmen !EpochIndex
    deriving (Show)

instance Exception LrcError

instance Buildable LrcError where
    build (LrcDataUnknown epoch reason) =
        bprint
            ("LRC data isn't presented for epoch #"%int%" so raise the exception with reason: "%stext)
            epoch reason
    build UnknownBlocksForLrc =
        bprint "there are no blocks for LRC computation"
    build LrcAfterGenesis =
        bprint "LRC was attempted after adoption of genesis block"
    build (NoRichmen epoch) =
        bprint ("there aren't richmen for epoch "%build) epoch
