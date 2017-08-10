-- | Types for websockets

module Pos.Wallet.Web.Sockets.Types
       ( WSConnection
       ) where

import qualified Network.WebSockets as WS

type WSConnection = WS.Connection
