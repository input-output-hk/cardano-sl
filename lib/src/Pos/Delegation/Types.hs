-- | Delegation-related local types.

module Pos.Delegation.Types
       ( DlgPayload (..)
       , mkDlgPayload
       , DlgUndo (..)
       , DlgMemPool
       , ProxySKBlockInfo
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, (%))
import           Serokell.Util (listJson)

import           Pos.Binary.Core ()
import           Pos.Core (ProxySKHeavy, StakeholderId)
import           Pos.Core.Delegation (DlgPayload (..), mkDlgPayload)
import           Pos.Crypto (PublicKey)


-- | Undo for the delegation component.
data DlgUndo = DlgUndo
    { duPsks            :: ![ProxySKHeavy]
      -- ^ PSKs we've modified when applying the block (by deleting or
      -- overwriting).
    , duPrevEpochPosted :: !(HashSet StakeholderId)
      -- ^ Set of stakeholders that posted in epoch i. This field
      -- should be present only for genesis block of epoch i+1.
    } deriving (Generic)

instance NFData DlgUndo

instance Buildable DlgUndo where
    build DlgUndo{..} =
        bprint ("DlgUndo:"%
                "\n  duPsks: "%listJson%
                "\n  duPrevEpochPosted: "%listJson)
               duPsks duPrevEpochPosted

-- | Map from issuer public keys to related heavy certs.
type DlgMemPool = HashMap PublicKey ProxySKHeavy

-- | Heavyweight PSK with real leader public key (because heavyweight
-- psks have redelegation feature, so pskIssuerPk hPsk /= leader in
-- general case). This is used to create a block header only.
type ProxySKBlockInfo = Maybe (ProxySKHeavy, PublicKey)
