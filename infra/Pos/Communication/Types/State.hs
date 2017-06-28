-- TODO: is this module needed?

-- | Socket state of server.

module Pos.Communication.Types.State
       ( PeerState
       ) where

import           Data.Default (Default (def))
-- import           Universum

-- | PeerState type aggregates socket states needed for different
-- parts of system.
data PeerState = PeerState
    {
    }

-- -- | Classy lenses for PeerState.
-- makeClassy ''PeerState

instance Default PeerState where
    def = PeerState
