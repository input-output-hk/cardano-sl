{-# LANGUAGE TemplateHaskell #-}

-- | Socket state of server.

module Pos.Communication.Types.State
       ( PeerState
       , peerVerInfo
       ) where

import           Control.Lens                     (makeClassy)
import           Data.Default                     (Default (def))
import           Pos.Communication.Types.Protocol (VerInfo)
import           Universum

-- | PeerState type aggregates socket states needed for different
-- parts of system.
data PeerState = PeerState
    { _peerVerInfo :: !(Maybe VerInfo)
    }

-- | Classy lenses for PeerState.
makeClassy ''PeerState

instance Default PeerState where
    def = PeerState Nothing
