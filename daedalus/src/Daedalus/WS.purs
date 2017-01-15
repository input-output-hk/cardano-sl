module Daedalus.WS where

import Prelude
import WebSocket as WS
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (writeRef, REF, readRef, Ref)
import Control.Monad.Eff.Var (($=))
import DOM.Event.Types (Event)
import DOM.Websocket.Event.Types (MessageEvent)
import Daedalus.Constants (wsUri)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe(Just, Nothing))
import WebSocket (runMessage, runMessageEvent)

type NotifyCb = Fn1 String (forall eff. Eff (| eff) Unit)
type ErrorCb = Fn1 Event (forall eff. Eff (| eff) Unit)

newtype WSState = WSState
    { url :: WS.URL
    , connection :: Ref WSConnection
    , notifyCb :: Maybe NotifyCb
    , errorCb :: Maybe ErrorCb
    }

mkWSState :: Ref WSConnection -> NotifyCb -> ErrorCb -> WSState
mkWSState conn notifyCb errorCb = WSState
    { url: (WS.URL wsUri)
    , notifyCb: Just notifyCb
    , errorCb: Just errorCb
    , connection: conn
    }

data WSConnection
    = WSNotConnected
    | WSConnectionRequested
    | WSConnected WS.Connection

closeConn :: forall eff. WSState -> Eff (ref :: REF, ws :: WS.WEBSOCKET
    , err :: EXCEPTION | eff) Unit
closeConn (WSState state) = do
    connection <- readRef state.connection
    case connection of
        WSConnected (WS.Connection socket) -> socket.close
        _ -> pure unit

openConn :: forall eff. WSState -> Eff (ref :: REF, ws :: WS.WEBSOCKET
    , err :: EXCEPTION | eff) Unit
openConn (WSState state) = do
    connection <- readRef state.connection
    case connection of
        WSNotConnected -> mkConn (WSState state)
        _ -> pure unit

mkConn :: forall eff. WSState -> Eff (ref :: REF, ws :: WS.WEBSOCKET
    , err :: EXCEPTION | eff) Unit
mkConn (WSState state) = do
    WS.Connection socket <- WS.newWebSocket state.url []
    writeRef state.connection WSConnectionRequested
    socket.onclose $= \_ -> onClose (WSState state)
    socket.onmessage $= \event -> onMessage (WSState state) event
    socket.onerror $= \event ->
        case state.errorCb of
            Just cb -> runFn1 cb event
            Nothing -> pure unit
    socket.onopen $= \_ -> onOpened (WSState state) $ WS.Connection socket

onClose :: forall eff. WSState -> Eff (ref :: REF | eff) Unit
onClose (WSState state) =
    writeRef state.connection WSNotConnected

onMessage :: forall eff. WSState -> MessageEvent -> Eff eff Unit
onMessage (WSState state) event = do
    let msg = runMessage $ runMessageEvent event
    case state.notifyCb of
        Just cb -> runFn1 cb msg
        Nothing -> pure unit

onOpened :: forall eff. WSState -> WS.Connection -> Eff (ref :: REF
    , ws :: WS.WEBSOCKET, err :: EXCEPTION | eff) Unit
onOpened (WSState state) (WS.Connection socket) = do
    writeRef state.connection <<< WSConnected $ WS.Connection socket
    -- TODO (jk) "socket.send" can be removed later on, just a check to get a message
    socket.send $ WS.Message "Opened 4 Daedalus"
