module Daedalus.WS where

-- This module implements WS channel with wallet. Note that this is still
-- work in progress and might be changed significantly.

import Prelude
import WebSocket as WS
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (writeRef, REF, readRef, Ref)
import Control.Monad.Eff.Var (($=))
import Control.Monad.Aff (later', launchAff, liftEff')
import DOM.Event.Types (Event)
import DOM.Websocket.Event.Types (MessageEvent)
import Data.Maybe (Maybe(Just, Nothing), maybe')
import WebSocket (runMessage, runMessageEvent)
import Data.Function.Eff (EffFn1, runEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Daedalus.TLS (WSSOptions, getWSUrl)
import Unsafe.Coerce (unsafeCoerce)
import Data.Options (options)
import Partial.Unsafe (unsafeCrashWith)

type NotifyCb eff = EffFn1 eff String Unit
type ErrorCb eff = EffFn1 eff Event Unit

newtype WSState eff = WSState
    { url :: WS.URL
    , connection :: Ref WSConnection
    , notifyCb :: Maybe (NotifyCb eff)
    , errorCb :: Maybe (ErrorCb eff)
    , wssOptions :: WSSOptions
    }

mkWSState :: forall eff. Ref WSConnection -> NotifyCb eff -> ErrorCb eff -> WSSOptions -> WSState eff
mkWSState conn notifyCb errorCb wssOptions = WSState
    { url: maybe' (\_ -> unsafeCrashWith "Incorrect TLS options for ws") WS.URL $ getWSUrl wssOptions
    , notifyCb: Just notifyCb
    , errorCb: Just errorCb
    , connection: conn
    , wssOptions: wssOptions
    }

data WSConnection
    = WSNotConnected
    | WSConnectionRequested
    | WSConnected WS.Connection

isConnected :: forall eff. WSState eff -> Eff (ref :: REF, ws :: WS.WEBSOCKET, err :: EXCEPTION | eff) Boolean
isConnected (WSState state) = do
    connection <- readRef state.connection
    pure $ case connection of
        WSConnected (WS.Connection _) -> true
        _ -> false

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

-- Initializes connection. Don't call this more then once!
mkConn :: forall eff. WSState eff -> Eff (ref :: REF, ws :: WS.WEBSOCKET
    , err :: EXCEPTION | eff) Unit
mkConn (WSState state) = do
    -- FIXME: don't use unsafeCoerce here. It was needed as websockets-simple is not designed properly
    WS.Connection socket <- WS.newWebSocket state.url $ unsafeCoerce $ options state.wssOptions
    writeRef state.connection WSConnectionRequested
    socket.onclose $= \_ -> onClose (WSState state)
    socket.onmessage $= \event -> onMessage (WSState state) event
    socket.onerror $= \event ->
        case state.errorCb of
            Just cb -> runEffFn1 cb event
            Nothing -> pure unit
    socket.onopen $= \_ -> onOpened (WSState state) $ WS.Connection socket

onClose :: forall eff. WSState eff -> Eff (ref :: REF, ws :: WS.WEBSOCKET, err :: EXCEPTION | eff) Unit
onClose st@(WSState state) = do
    -- FIXME: temp solution. Try reconnecting after close
    writeRef state.connection WSNotConnected
    case state.notifyCb of
        Just cb -> do
            -- NOTE: this logic was introduced in order to resolve someone calling mkConn multiple times.
            -- as REF is not working as I expected this logic of checking is it connected doesn't add
            -- anything to the table but I hope there will be time to fix this. Currecntly if user by accident calls
            -- mkConn more then once, there will be two mkConn trying to reconnect and they will interfer with each
            -- other.
            notConnected <- map not $ isConnected st
            when notConnected $
                void $ launchAff $ later' 5000 $ liftEff' $ mkConn st

            -- FIXME: don't hardcode the message. Create new event!
            unsafeCoerceEff $ runEffFn1 cb "{\"tag\":\"ConnectionClosedReconnecting\",\"contents\":[]}"
        Nothing -> pure unit

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
