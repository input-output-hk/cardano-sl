module Explorer.Api.Socket where

import Prelude
import Control.Monad.Eff (Eff)
import Control.SocketIO.Client (Event, Host)
import Data.Argonaut.Core (Json)
import Data.Foreign (Foreign)
import Explorer.Api.Helper (decodeResult)
import Explorer.Types.Actions (Action(..), ActionChannel)
import Signal.Channel (CHANNEL, send)


-- host

socketHost :: Host
socketHost = "http://localhost:8110"

-- events

connectEvent :: Event
connectEvent = "connect"

closeEvent :: Event
closeEvent = "close"

lastestBlocksEvent :: Event
lastestBlocksEvent = "latestBlocks"

lastestTransactionsEvent :: Event
lastestTransactionsEvent = "latestTransactions"

-- event handler

connectHandler :: forall eff. ActionChannel -> Foreign
    -> Eff (channel :: CHANNEL | eff) Unit
connectHandler channel _ =
    send channel $ SocketConnected true

closeHandler :: forall eff. ActionChannel -> Foreign
    -> Eff (channel :: CHANNEL | eff) Unit
closeHandler channel _ =
    send channel $ SocketConnected false

latestBlocksHandler :: forall eff. ActionChannel -> Json
    -> Eff (channel :: CHANNEL | eff) Unit
latestBlocksHandler channel json =
    let result = decodeResult json in
    send channel $ SocketLatestBlocks result

latestTransactionsHandler :: forall eff. ActionChannel -> Json
    -> Eff (channel :: CHANNEL | eff) Unit
latestTransactionsHandler channel json =
    let result = decodeResult json in
    send channel $ SocketLatestTransactions result
