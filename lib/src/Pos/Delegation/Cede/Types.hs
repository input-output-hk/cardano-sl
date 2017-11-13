-- | Types related to PSK resolving via "Cede".

module Pos.Delegation.Cede.Types
       (
         DlgEdgeAction (..)
       , pskToDlgEdgeAction
       , dlgEdgeActionIssuer

       , CedeModifier (..)
       , cmPskMods
       , cmHasPostedThisEpoch
       ) where

import           Control.Lens (makeLenses)
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import           Universum

import           Pos.Core (ProxySKHeavy, StakeholderId, addressHash)
import           Pos.Crypto (pskIssuerPk)
import           Pos.Delegation.Helpers (isRevokePsk)

-- | Action on delegation database, used commonly. Generalizes
-- applications and rollbacks.
data DlgEdgeAction
    = DlgEdgeAdd !ProxySKHeavy
    | DlgEdgeDel !StakeholderId
    deriving (Show, Eq, Generic)

instance Hashable DlgEdgeAction

instance Buildable DlgEdgeAction where
    build (DlgEdgeAdd psk) = bprint ("DlgEdgeAdd: "%build) psk
    build (DlgEdgeDel iPk) = bprint ("DlgEdgeDel: "%build) iPk

-- | Converts heavy psk to the psk mapping action.
pskToDlgEdgeAction :: ProxySKHeavy -> DlgEdgeAction
pskToDlgEdgeAction psk
    | isRevokePsk psk = DlgEdgeDel (addressHash $ pskIssuerPk psk)
    | otherwise = DlgEdgeAdd psk

-- | Gets issuer of edge action (u from the edge uv).
dlgEdgeActionIssuer :: DlgEdgeAction -> StakeholderId
dlgEdgeActionIssuer = \case
    (DlgEdgeDel sId) -> sId
    (DlgEdgeAdd psk) -> addressHash (pskIssuerPk psk)

-- TODO should it use Pos.Util.Modifier ?
-- | A set of modifications for dlg-related component to implement
-- Cede-related typeclasses.
data CedeModifier = CedeModifier
    { _cmPskMods            :: !(HashMap StakeholderId DlgEdgeAction)
      -- ^ PSK modifications (added/overwritten or deleted)
    , _cmHasPostedThisEpoch :: !(HashMap StakeholderId Bool)
      -- ^ 'True' means user has posted psk, 'False' means he didn't.
    }

makeLenses ''CedeModifier

instance Monoid CedeModifier where
    mempty = CedeModifier mempty mempty
    mappend (CedeModifier a b) (CedeModifier c e) =
        CedeModifier (a <> c) (b <> e)
