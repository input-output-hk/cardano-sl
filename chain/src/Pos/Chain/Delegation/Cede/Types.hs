-- | Types related to PSK resolving via "Cede".

module Pos.Chain.Delegation.Cede.Types
       (
         DlgEdgeAction (..)
       , pskToDlgEdgeAction
       , dlgEdgeActionIssuer

       , CedeModifier (..)
       , cmPskMods
       , cmHasPostedThisEpoch
       , cmLookupCache
       , emptyCedeModifier
       ) where

import           Control.Lens (makeLenses)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable
import           Universum

import           Pos.Chain.Delegation.HeavyDlgIndex (ProxySKHeavy)
import           Pos.Chain.Delegation.Types (isRevokePsk)
import           Pos.Core (StakeholderId, addressHash)
import           Pos.Crypto (pskIssuerPk)

-- | Action on delegation database, used commonly. Generalizes
-- applications and rollbacks.
data DlgEdgeAction
    = DlgEdgeAdd !ProxySKHeavy
    | DlgEdgeDel !StakeholderId
    deriving (Show, Eq, Ord, Generic)

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

-- Pos.Util.Modifier is something similar.
-- | A set of modifications for dlg-related component to implement
-- Cede-related typeclasses.
data CedeModifier = CedeModifier
    { _cmPskMods            :: !(HashMap StakeholderId DlgEdgeAction)
      -- ^ PSK modifications (added/overwritten or deleted)
    , _cmHasPostedThisEpoch :: !(HashMap StakeholderId Bool)
      -- ^ 'True' means user has posted psk, 'False' means he didn't.
    , _cmLookupCache        :: !(HashMap StakeholderId (Maybe ProxySKHeavy))
      -- ^ This is a hacky cache to make things faster (it actually
      -- does). It can also make things slower in the future. Cache
      -- invalidation is hard, so we will erase the value of @k@ every
      -- time something is written to @k@. So writes will be a little
      -- bit slower, but it optimizes reads significantly. In other
      -- words, cache only holds db query result unless something
      -- comes into cmPskMods, then we drop the value from cache.
    }

makeLenses ''CedeModifier

-- | Create a completely empty 'CedeModifier'.
emptyCedeModifier :: CedeModifier
emptyCedeModifier = CedeModifier mempty mempty mempty
