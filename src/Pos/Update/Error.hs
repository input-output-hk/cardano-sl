-- | Types describing runtime errors related to Txp.

module Pos.Update.Error
       ( USError (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting          (bprint, build, stext, (%))
import           Universum

import           Pos.Crypto          (shortHashF)
import           Pos.Types           (SoftwareVersion, StakeholderId)
import           Pos.Update.Core     (UpId)

data USError
    = USCantApplyBlocks !Text
     -- ^ Can't apply blocks to GState.
    | USNotRichmen !StakeholderId
     -- ^ Voter from applied block is not richman.
    | USUnknownSoftware !SoftwareVersion
     -- ^ Unknown SoftwareVersion encountered.
    | USUnknownProposal !UpId
     -- ^ Unknown proposal ID encountered.
    deriving (Show)

instance Exception USError

instance Buildable USError where
    build (USCantApplyBlocks msg) = bprint ("US can't apply blocks: "%stext) msg
    build (USNotRichmen id) =
        bprint ("attempt to apply block with vote from not richman: "%shortHashF)
        id
    build (USUnknownSoftware sv) =
        bprint ("US encountered unknown SoftwareVersion: "%build) sv
    build (USUnknownProposal upId) =
        bprint ("US encountered unknown Update Proposal ID: "%build) upId
