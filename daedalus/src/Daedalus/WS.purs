module Daedalus.WS where

import Daedalus.Constants (wsUri)
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=), get)
import Debug.Trace (traceAnyM)
import WebSocket (WEBSOCKET, runMessageEvent, Message(Message), runURL, URL(URL), runMessage, newWebSocket, Connection(Connection))


openConn :: forall eff. String -> Eff (ws :: WEBSOCKET, err :: EXCEPTION | eff) Connection
openConn u = newWebSocket (URL u) []

-- Simple example based on https://github.com/zudov/purescript-websocket-simple/blob/master/example/src/Main.purs
-- It should be removed later on
example :: forall eff . Eff (ws :: WEBSOCKET, err :: EXCEPTION | eff) Unit
example = do
  Connection socket <- openConn wsUri

  socket.onopen $= \event -> do
    traceAnyM event
    log "onopen: Connection opened"

    log <<< runURL =<< get socket.url

    log "onopen: Sending 'hello'"
    socket.send (Message "hello")

    log "onopen: Sending 'goodbye'"
    socket.send (Message "goodbye")

  socket.onmessage $= \event -> do
    traceAnyM event
    let received = runMessage (runMessageEvent event)

    log $ "onmessage: Received '" <> received <> "'"

    when (received == "goodbye") do
      log "onmessage: closing connection"
      socket.close

  socket.onclose $= \event -> do
    traceAnyM event
    log "onclose: Connection closed"
