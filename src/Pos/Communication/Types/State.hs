{-# LANGUAGE TemplateHaskell #-}

-- | Socket state of server.

module Pos.Communication.Types.State
       ( SocketState
       , MutSocketState
       , newMutSocketState
       , peerVersion
       ) where

import           Control.Concurrent.STM         (TVar, newTVarIO)
import           Control.Lens                   (makeClassy)
import           Data.Default                   (Default (def))
import           Universum

import           Pos.Block.Network.Server.State (BlockSocketState,
                                                 HasBlockSocketState (blockSocketState))
import           Pos.Types                      (ProtocolVersion)

-- | SocketState type aggregates socket states needed for different
-- parts of system.
data SocketState ssc = SocketState
    { __blockSocketState :: !(BlockSocketState ssc)
      -- ^ State of block/header logic
    , _peerVersion       :: !(Maybe ProtocolVersion)
      -- ^ Version of the protocol peer uses
    }

-- | Classy lenses for SocketState.
makeClassy ''SocketState

instance Default (SocketState ssc) where
    def =
        SocketState
        { __blockSocketState = def
        , _peerVersion = Nothing
        }

instance HasBlockSocketState (SocketState ssc) ssc where
    blockSocketState = _blockSocketState

-- | Mutable SocketState.
type MutSocketState ssc = TVar (SocketState ssc)

-- | Create a new mutable socket state
newMutSocketState :: IO (MutSocketState ssc)
newMutSocketState = newTVarIO def
