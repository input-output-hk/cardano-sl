module Explorer.Socket where

import Prelude (($), Unit)
import Control.Monad.Eff (Eff)
import Control.SocketIO.Client (Event)
import Data.Foreign (Foreign)
import Explorer.Types.Actions (Action(..), ActionChannel)
import Signal.Channel (CHANNEL, send)

-- events

connectEvent :: Event
connectEvent = "connect"

closeEvent :: Event
closeEvent = "close"


-- event handler

connectHandler :: forall eff. ActionChannel -> Foreign
    -> Eff (channel :: CHANNEL | eff) Unit
connectHandler channel _ =
    send channel $ SocketConnected true

closeHandler :: forall eff. ActionChannel -> Foreign
    -> Eff (channel :: CHANNEL | eff) Unit
closeHandler channel _ =
    send channel $ SocketConnected false
