-- | Delegation-related local types.

module Pos.Delegation.Types
       ( DlgPayload (..)
       , DlgUndo (..)
       , DlgMemPool
       , ProxySKBlockInfo
       , module Pos.Core.Delegation
       , isRevokePsk

       , DlgBlock
       , DlgBlund
       ) where

import           Universum

import qualified Data.Text.Buildable as Buildable
import           Formatting (bprint, (%))
import           Serokell.Util.Text (listJson)

import           Pos.Core (ComponentBlock (..), ProxySKHeavy, StakeholderId)
import           Pos.Core.Delegation (DlgPayload (..), checkDlgPayload)
import           Pos.Crypto (ProxySecretKey, PublicKey, isSelfSignedPsk)

-- | Undo for the delegation component.
data DlgUndo = DlgUndo
    { duPsks            :: ![ProxySKHeavy]
      -- ^ PSKs we've modified when applying the block (by deleting or
      -- overwriting). There should be no duplicates, every psk must
      -- have a unique issuer.
    , duPrevEpochPosted :: !(HashSet StakeholderId)
      -- ^ Set of stakeholders that posted in epoch i. This field
      -- should be present only for genesis block of epoch i+1.
    } deriving (Eq, Show, Generic)

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

-- | Checks if given PSK revokes delegation (issuer == delegate).
isRevokePsk :: ProxySecretKey w -> Bool
isRevokePsk = isSelfSignedPsk

----------------------------------------------------------------------------
-- DlgBlock
----------------------------------------------------------------------------

type DlgBlock = ComponentBlock DlgPayload

type DlgBlund = (DlgBlock, DlgUndo)
