{-# LANGUAGE TemplateHaskell #-}

-- | Socket state of server.

module Pos.Communication.Types.State
       ( PeerState
       , peerVersion
       ) where

import           Control.Lens (makeClassy)
import           Data.Default (Default (def))
import           Universum

import           Pos.Types    (ProtocolVersion)

-- | PeerState type aggregates socket states needed for different
-- parts of system.
data PeerState ssc = PeerState
    { _peerVersion     :: !(Maybe ProtocolVersion)
      -- ^ Version of the protocol peer uses
    }

-- | Classy lenses for PeerState.
makeClassy ''PeerState

instance Default (PeerState ssc) where
    def =
        PeerState
        { _peerVersion = Nothing
        }
