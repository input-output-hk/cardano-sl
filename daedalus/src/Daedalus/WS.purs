module Daedalus.WS where

import Prelude
import WebSocket as WSSimple
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (writeRef, REF, readRef, Ref)
import Control.Monad.Eff.Var (($=))
import DOM.Event.Types (Event)
import DOM.Websocket.Event.Types (MessageEvent)
import Daedalus.Constants (wsUri)
import WebSocket (runMessage, runMessageEvent)

type NotifyCb eff = String -> Eff eff Unit
type ErrorCb eff = Event -> Eff eff Unit

type WSState eff = {
    url :: WSSimple.URL
    , connection :: Ref WSConnection
    , notifyCb :: NotifyCb eff
    , errorCb :: ErrorCb eff
}

mkWSState :: forall  eff. Ref WSConnection -> NotifyCb eff -> ErrorCb eff -> WSState eff
mkWSState conn notifyCb errorCb = {
    url: (WSSimple.URL wsUri)
    , notifyCb
    , errorCb
    , connection: conn
  }

data WSConnection = WSNotConnected
    | WSConnectionRequested
    | WSConnected WSSimple.Connection

closeConn :: forall eff. WSState eff -> Eff (ref :: REF, ws :: WSSimple.WEBSOCKET
    , err :: EXCEPTION | eff) Unit
closeConn state = do
    connection <- readRef state.connection
    case connection of
        WSConnected (WSSimple.Connection socket) -> socket.close
        _ -> pure unit

openConn :: forall eff. WSState eff -> Eff (ref :: REF, ws :: WSSimple.WEBSOCKET
    , err :: EXCEPTION | eff) Unit
openConn state = do
    connection <- readRef state.connection
    case connection of
        WSNotConnected -> mkConn state
        _ -> pure unit

mkConn :: forall eff. WSState eff -> Eff (ref :: REF, ws :: WSSimple.WEBSOCKET
    , err :: EXCEPTION | eff) Unit
mkConn state = do
    WSSimple.Connection socket <- WSSimple.newWebSocket state.url []
    writeRef state.connection WSConnectionRequested
    socket.onclose $= \_ -> onClose state
    socket.onmessage $= \e -> onMessage state e
    socket.onerror $= state.errorCb
    socket.onopen $= \_ -> onOpened state $ WSSimple.Connection socket

onClose :: forall eff. WSState eff -> Eff (ref :: REF | eff) Unit
onClose state =
    writeRef state.connection WSNotConnected

onMessage :: forall eff. WSState eff -> MessageEvent -> Eff eff Unit
onMessage state event = do
    state.notifyCb <<< runMessage $ runMessageEvent event
    pure unit

onOpened :: forall eff b. WSState eff -> WSSimple.Connection ->
    Eff (ref :: REF, ws :: WSSimple.WEBSOCKET, err :: EXCEPTION | b) Unit
onOpened state (WSSimple.Connection socket) = do
    writeRef state.connection <<< WSConnected $ WSSimple.Connection socket
    socket.send $ WSSimple.Message "Opened 4 Daedalus"
