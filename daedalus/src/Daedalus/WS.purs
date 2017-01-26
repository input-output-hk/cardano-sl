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
import Data.Maybe (Maybe(Just, Nothing))
import WebSocket (runMessage, runMessageEvent)
import Data.Function.Eff (EffFn1, runEffFn1)
import Debug.Trace (traceAnyM)

type NotifyCb eff = EffFn1 eff String Unit
type ErrorCb eff = EffFn1 eff Event Unit

newtype WSState eff = WSState
    { url :: WS.URL
    , connection :: Ref WSConnection
    , notifyCb :: Maybe (NotifyCb eff)
    , errorCb :: Maybe (ErrorCb eff)
    }

mkWSState :: forall eff. Ref WSConnection -> NotifyCb eff -> ErrorCb eff -> WSState eff
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

closeConn :: forall eff. WSState eff -> Eff (ref :: REF, ws :: WS.WEBSOCKET
    , err :: EXCEPTION | eff) Unit
closeConn (WSState state) = do
    connection <- readRef state.connection
    case connection of
        WSConnected (WS.Connection socket) -> socket.close
        _ -> pure unit

openConn :: forall eff. WSState eff -> Eff (ref :: REF, ws :: WS.WEBSOCKET
    , err :: EXCEPTION | eff) Unit
openConn (WSState state) = do
    connection <- readRef state.connection
    case connection of
        WSNotConnected -> mkConn (WSState state)
        _ -> pure unit

mkConn :: forall eff. WSState eff -> Eff (ref :: REF, ws :: WS.WEBSOCKET
    , err :: EXCEPTION | eff) Unit
mkConn (WSState state) = do
    WS.Connection socket <- WS.newWebSocket state.url []
    writeRef state.connection WSConnectionRequested
    socket.onclose $= \_ -> onClose (WSState state)
    socket.onmessage $= \event -> onMessage (WSState state) event
    socket.onerror $= \event ->
        case state.errorCb of
            Just cb -> runEffFn1 cb event
            Nothing -> pure unit
    socket.onopen $= \_ -> onOpened (WSState state) $ WS.Connection socket

onClose :: forall eff. WSState eff -> Eff (ref :: REF | eff) Unit
onClose (WSState state) = do
    traceAnyM "wooooo closing"
    writeRef state.connection WSNotConnected

onMessage :: forall eff. WSState eff -> MessageEvent -> Eff eff Unit
onMessage (WSState state) event = do
    let msg = runMessage $ runMessageEvent event
    case state.notifyCb of
        Just cb -> runEffFn1 cb msg
        Nothing -> pure unit

onOpened :: forall eff. WSState eff -> WS.Connection -> Eff (ref :: REF
    , ws :: WS.WEBSOCKET, err :: EXCEPTION | eff) Unit
onOpened (WSState state) (WS.Connection socket) = do
    writeRef state.connection <<< WSConnected $ WS.Connection socket
