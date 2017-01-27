{-# LANGUAGE TemplateHaskell #-}

-- | Socket state of server.

module Pos.Communication.Types.State
       ( PeerState
       ) where

-- import           Control.Lens (makeClassy)
import           Data.Default (Default (def))
-- import           Universum

-- [CSL-666] TODO this insfrastructure is to be used later with difference we use IP, not NodeId to track peers

-- | PeerState type aggregates socket states needed for different
-- parts of system.
data PeerState ssc = PeerState

-- -- | Classy lenses for PeerState.
-- makeClassy ''PeerState

instance Default (PeerState ssc) where
    def = PeerState
