-- | Types related to PSK resolving via "Cede".

module Pos.Delegation.Cede.Types
       (
         DlgEdgeAction (..)
       , pskToDlgEdgeAction
       , dlgEdgeActionIssuer

       , CedeModifier
       ) where

import           Universum

import           Pos.Core               (ProxySKHeavy, StakeholderId, addressHash)
import           Pos.Crypto             (pskIssuerPk)
import           Pos.Delegation.Helpers (isRevokePsk)

-- | Action on delegation database, used commonly. Generalizes
-- applications and rollbacks.
data DlgEdgeAction
    = DlgEdgeAdd !ProxySKHeavy
    | DlgEdgeDel !StakeholderId
    deriving (Show, Eq, Generic)

instance Hashable DlgEdgeAction

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

-- | A set of modifications for PSK storage.
type CedeModifier = HashMap StakeholderId DlgEdgeAction
