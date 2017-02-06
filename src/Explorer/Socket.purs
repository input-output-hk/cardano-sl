module Explorer.Socket where

import Control.Monad.Eff (Eff)
import Control.SocketIO.Client (Event, Host)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Generic.Argonaut(decodeJson)
import Data.Foreign (Foreign)
import Explorer.Types.Actions (Action(..), ActionChannel)
import Prelude (($), Unit)
import Signal.Channel (CHANNEL, send)


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
    let result = decodeJson json
    send channel $ SocketLatestBlocks result
