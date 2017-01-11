{-# LANGUAGE TemplateHaskell #-}

-- | Socket state of server.

module Pos.Communication.Types.State
       ( PeerState
       , MutPeerState
       , newMutPeerState
       , peerVersion
       ) where

import           Control.Concurrent.STM         (TVar, newTVarIO)
import           Control.Lens                   (makeClassy)
import           Data.Default                   (Default (def))
import           Universum

import           Pos.Block.Network.Server.State (BlockPeerState,
                                                 HasBlockPeerState (blockPeerState))
import           Pos.Types                      (ProtocolVersion)

--

-- | PeerState type aggregates socket states needed for different
-- parts of system.
data PeerState ssc = PeerState
    { __blockPeerState :: !(BlockPeerState ssc)
      -- ^ State of block/header logic
    , _peerVersion     :: !(Maybe ProtocolVersion)
      -- ^ Version of the protocol peer uses
    }

-- | Classy lenses for PeerState.
makeClassy ''PeerState

instance Default (PeerState ssc) where
    def =
        PeerState
        { __blockPeerState = def
        , _peerVersion = Nothing
        }

instance HasBlockPeerState (PeerState ssc) ssc where
    blockPeerState = _blockPeerState

-- | Mutable PeerState.
type MutPeerState ssc = TVar (PeerState ssc)

-- | Create a new mutable socket state
newMutPeerState :: IO (MutPeerState ssc)
newMutPeerState = newTVarIO def
