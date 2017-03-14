module Explorer.Api.Socket where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.SocketIO.Client (Event, Host)
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Debug.Trace (traceAnyM, traceShowM)
import Explorer.Api.Helper (decodeResult', encodeJson)
import Explorer.Types.Actions (Action(..), ActionChannel)
import Pos.Explorer.Web.ClientTypes (CTxId)
import Pos.Explorer.Web.Sockets.Methods (ClientEvent, ServerEvent)
import Signal.Channel (CHANNEL, send)


-- host

socketHost :: Host
socketHost = "http://localhost:8110"

-- events

class SocketEvent a where
    toEvent :: a -> Event

instance socketEventServerEvent :: SocketEvent ClientEvent where
    toEvent = cleanEventName <<< show <<< encodeJson

instance socketEventClientEvent :: SocketEvent ServerEvent where
    toEvent = cleanEventName <<< show <<< encodeJson

-- | Helper function to remove """ from event names 
cleanEventName :: Event -> Event
cleanEventName = replaceAll (Pattern "\"") (Replacement "")

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
    let result = decodeResult' json in
    send channel $ SocketLatestBlocks result

latestTransactionsHandler :: forall eff. ActionChannel -> Json
    -> Eff (channel :: CHANNEL | eff) Unit
latestTransactionsHandler channel json =
    let result = decodeResult' json in
    send channel $ SocketLatestTransactions result

-- all following event handler are for debugging only

callYouEventHandler :: forall eff. ActionChannel -> Foreign
    -> Eff (channel :: CHANNEL | eff) Unit
callYouEventHandler channel _ = do
    traceShowM "callYouEventHandler"
    send channel NoOp

callYouStringEventHandler :: forall eff. ActionChannel -> String
    -> Eff (channel :: CHANNEL | eff) Unit
callYouStringEventHandler channel str =
    send channel NoOp

callYouCTxIdEventHandler :: forall eff. ActionChannel -> Json
    -> Eff (channel :: CHANNEL | eff) Unit
callYouCTxIdEventHandler channel json = do
    traceAnyM "callYouCTxIdEventHandler"
    traceAnyM json
    let result = decodeResult' json
    traceAnyM (result :: Either Error CTxId)
    send channel NoOp
