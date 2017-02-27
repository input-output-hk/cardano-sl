{-# LANGUAGE TemplateHaskell #-}

-- | Socket state of server.

module Pos.Communication.Types.State
       ( PeerState
       , peerState
       ) where

import           Control.Lens (makeClassy)
import           Data.Default (Default (def))

-- | PeerState type aggregates socket states needed for different
-- parts of system.
data PeerState = PeerState
    {
    }

-- | Classy lenses for PeerState.
makeClassy ''PeerState

instance Default PeerState where
    def = PeerState
