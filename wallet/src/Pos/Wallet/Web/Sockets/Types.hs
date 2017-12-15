-- | Types for websockets

module Pos.Wallet.Web.Sockets.Types
       ( WSConnection
       , NotifyEvent (..)
       ) where

import           Universum

import qualified Network.WebSockets as WS

import           Pos.Core (ChainDifficulty)

-- | Shortcut for websocket connection
type WSConnection = WS.Connection

-- | Possible notifications
data NotifyEvent
    = ConnectionOpened
    -- _ | NewWalletTransaction CId
    -- _ | NewTransaction
    | NetworkDifficultyChanged ChainDifficulty -- ie new block or fork (rollback)
    | LocalDifficultyChanged ChainDifficulty -- ie new block or fork (rollback)
    | ConnectedPeersChanged Word
    | UpdateAvailable
    | ConnectionClosed
    deriving (Show, Generic)
