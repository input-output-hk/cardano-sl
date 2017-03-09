module Explorer.Api.Socket where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.SocketIO.Client (Event, Host)
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Foreign (Foreign)
import Debug.Trace (traceAnyM, traceShowM)
import Explorer.Api.Helper (decodeResult)
import Explorer.Types.Actions (Action(..), ActionChannel)
import Pos.Explorer.Web.ClientTypes (CTxId)
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

-- all following events are for debugging only

callMeEvent :: Event
callMeEvent = "callme"

callYouEvent :: Event
callYouEvent = "callyou"

callMeStringEvent :: Event
callMeStringEvent = "callme-string"

callYouStringEvent :: Event
callYouStringEvent = "callyou-string"

callMeCTxIdEvent :: Event
callMeCTxIdEvent = "callme-txid"

callYouCTxIdEvent :: Event
callYouCTxIdEvent = "callyou-txid"



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
    let result = decodeResult json
    traceAnyM (result :: Either Error CTxId)
    send channel NoOp
