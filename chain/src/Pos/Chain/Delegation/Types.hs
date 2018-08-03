-- | Delegation-related local types.

module Pos.Chain.Delegation.Types
       ( DlgPayload (..)
       , DlgUndo (..)
       , DlgMemPool
       , module Pos.Core.Delegation
       , isRevokePsk

       , DlgBlock
       , DlgBlund
       ) where

import           Universum

import           Formatting (bprint, (%))
import qualified Formatting.Buildable as Buildable
import           Serokell.Util.Text (listJson)

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Chain.Block.Union (ComponentBlock (..))
import           Pos.Core (StakeholderId)
import           Pos.Core.Delegation (DlgPayload (..), ProxySKBlockInfo,
                     ProxySKHeavy, checkDlgPayload)
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

deriveSimpleBi ''DlgUndo [
    Cons 'DlgUndo [
        Field [| duPsks            :: [ProxySKHeavy]        |],
        Field [| duPrevEpochPosted :: HashSet StakeholderId |]
    ]]

instance NFData DlgUndo

instance Buildable DlgUndo where
    build DlgUndo{..} =
        bprint ("DlgUndo:"%
                "\n  duPsks: "%listJson%
                "\n  duPrevEpochPosted: "%listJson)
               duPsks duPrevEpochPosted

-- | Map from issuer public keys to related heavy certs.
type DlgMemPool = HashMap PublicKey ProxySKHeavy

-- | Checks if given PSK revokes delegation (issuer == delegate).
isRevokePsk :: ProxySecretKey w -> Bool
isRevokePsk = isSelfSignedPsk

----------------------------------------------------------------------------
-- DlgBlock
----------------------------------------------------------------------------

type DlgBlock = ComponentBlock DlgPayload

type DlgBlund = (DlgBlock, DlgUndo)
