-- | Types related to PSK resolving via "Cede".

module Pos.Delegation.Cede.Types
       (
         DlgEdgeAction (..)
       , pskToDlgEdgeAction
       , dlgEdgeActionIssuer

       , CedeModifier
       ) where

import           Universum

import           Pos.Crypto             (PublicKey, pskIssuerPk)
import           Pos.Delegation.Helpers (isRevokePsk)
import           Pos.Types              (ProxySKHeavy)

-- | Action on delegation database, used commonly. Generalizes
-- applications and rollbacks.
data DlgEdgeAction
    = DlgEdgeAdd !ProxySKHeavy
    | DlgEdgeDel !PublicKey
    deriving (Show, Eq, Generic)

instance Hashable DlgEdgeAction

-- | Converts heavy psk to the psk mapping action.
pskToDlgEdgeAction :: ProxySKHeavy -> DlgEdgeAction
pskToDlgEdgeAction psk
    | isRevokePsk psk = DlgEdgeDel (pskIssuerPk psk)
    | otherwise = DlgEdgeAdd psk

-- | Gets issuer of edge action (u from the edge uv).
dlgEdgeActionIssuer :: DlgEdgeAction -> PublicKey
dlgEdgeActionIssuer = \case
    (DlgEdgeDel iPk) -> iPk
    (DlgEdgeAdd psk) -> pskIssuerPk psk

-- | A set of modifications for PSK storage.
type CedeModifier = HashMap PublicKey DlgEdgeAction
