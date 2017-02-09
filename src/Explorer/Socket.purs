module Explorer.Socket where

import Prelude
import Control.Monad.Eff (Eff)
import Control.SocketIO.Client (Event, Host)
import Data.Argonaut.Core (Json)
import Explorer.Util.Api(decodeJson)
import Data.Foreign (Foreign)
import Explorer.Types.Actions (Action(..), ActionChannel)
import Prelude (($), Unit)
import Signal.Channel (CHANNEL, send)
import Debug.Trace


-- host

socketHost :: Host
socketHost = "http://localhost:9999"

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
latestBlocksHandler channel json = do
    traceAnyM "latestBlocksHandler ..........."
    traceAnyM json
    let result = decodeJson json
    traceAnyM result
    send channel $ SocketLatestBlocks result

latestTransactionsHandler :: forall eff. ActionChannel -> Json
    -> Eff (channel :: CHANNEL | eff) Unit
latestTransactionsHandler channel json =
    let result = decodeJson json in
    send channel $ SocketLatestTransactions result
