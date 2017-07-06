module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.SocketIO.Client (connect, on)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (click)
import DOM.HTML.Location (pathname)
import DOM.HTML.Types (htmlDocumentToEventTarget)
import DOM.HTML.Window (document, location)
import Data.Lens ((^.), set)
import Data.Maybe (Maybe(..), fromMaybe)
import Explorer.Api.Socket (addressTxsUpdatedEventHandler, blocksPageUpdatedEventHandler, callYouEventHandler, mkSocketHost, connectEvent, closeEvent, connectHandler, closeHandler, toEvent, txsUpdatedHandler) as Ex
import Explorer.I18n.Lang (Language(..), detectLocale)
import Explorer.Lenses.State (connection, lang, socket, syncAction)
import Explorer.Routes (match)
import Explorer.Types.Actions (Action(..), ActionChannel) as Ex
import Explorer.Types.App (AppEffects)
import Explorer.Types.State (State) as Ex
import Explorer.Update (update) as Ex
import Explorer.Util.Config (SyncAction(..), hostname, isProduction, secureProtocol)
import Explorer.View.Layout (view)
import Pos.Explorer.Socket.Methods (ServerEvent(..))
import Pux (App, Config, CoreEffects, start)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM)
import Signal (Signal, (~>))
import Signal.Channel (channel, send, subscribe)
import Signal.Time (every, second)

type AppConfig eff = (Config (DOMEvent -> Ex.Action) Ex.Action Ex.State (AppEffects eff))

-- | Config to synchronize data by socket-io
socketConfig :: forall eff . (AppConfig eff) -> Ex.ActionChannel -> Eff (CoreEffects (AppEffects eff)) (AppConfig eff)
socketConfig appConfig actionChannel = do
    -- socket
    let pingSignal = every (10.0 * second) ~> const Ex.SocketPing
    socketHost <- Ex.mkSocketHost (secureProtocol isProduction) <$> hostname
    socket' <- connect socketHost
    _ <- on socket' Ex.connectEvent $ Ex.connectHandler actionChannel
    _ <- on socket' Ex.closeEvent $ Ex.closeHandler actionChannel
    _ <- on socket' (Ex.toEvent TxsUpdated) $ Ex.txsUpdatedHandler actionChannel
    _ <- on socket' (Ex.toEvent BlocksLastPageUpdated) $ Ex.blocksPageUpdatedEventHandler actionChannel
    _ <- on socket' (Ex.toEvent AddrUpdated) $ Ex.addressTxsUpdatedEventHandler actionChannel
    -- Note:
    -- `CallYou` is the answer of `CallMe`.
    -- Handling both events are needed a to be connected with socket.io manually
    _ <- on socket' (Ex.toEvent CallYou) $ Ex.callYouEventHandler actionChannel
    pure $ appConfig
        { initialState = set (socket <<< connection) (Just socket') appConfig.initialState
        , inputs = [ pingSignal ] <> appConfig.inputs
        }

-- | Config to synchronize data by polling
pollingConfig :: forall eff . (AppConfig eff) -> Eff (CoreEffects (AppEffects eff)) (AppConfig eff)
pollingConfig appConfig =
    let reloadSignal = every (60.0 * second) ~> const Ex.Reload in
    pure $ appConfig
        { inputs = [ reloadSignal ] <> appConfig.inputs
        }

-- | Common config
commonConfig :: forall eff . Ex.State -> Ex.ActionChannel -> Eff (CoreEffects (AppEffects eff)) (AppConfig eff)
commonConfig state actionChannel = do
    -- routing
    urlSignal <- sampleURL =<< window
    let routeSignal = urlSignal ~> (Ex.UpdateView <<< match)
    -- timer
    let clockSignal = every second ~> const Ex.UpdateClock
    -- detected locale
    locale <- fromMaybe English <$> detectLocale
    -- register actionSignal
    let actionSignal = subscribe actionChannel :: Signal Ex.Action
    -- register global (document) click listener
    -- globalClickListener :: forall eff. Event -> Eff (channel :: CHANNEL | eff) Unit
    let globalClickListener event = send actionChannel $ Ex.DocumentClicked event

    _ <- window >>=
            document >>=
                htmlDocumentToEventTarget >>>
                    addEventListener click (eventListener globalClickListener) false

    pure
        { initialState: set lang locale state
        , foldp: Ex.update
        , view
        , inputs:
              [
              -- clockSignal
              actionSignal
              -- Important note:
              -- routeSignal has to be the last signal in row !!!
              , routeSignal
              ]
        }

appSelector :: String
appSelector = "#explorer"

type WebApp = App (DOMEvent -> Ex.Action) Ex.Action Ex.State

main :: forall eff . Ex.State -> Eff (CoreEffects (AppEffects eff)) WebApp
main state = do
    actionChannel <- channel Ex.NoOp
    appConfig <- commonConfig state actionChannel
    config <- case state ^. syncAction of
                  SyncByPolling -> pollingConfig appConfig
                  SyncBySocket -> socketConfig appConfig actionChannel
    app <- start config
    _ <- renderToDOM appSelector app.markup app.input

    -- Trigger `UpdateView` after starting app
    -- to run side effects which are needed by each route
    -- Because it's not triggered by Pux 10.x - maybe a bug???
    _ <- window >>=
            location >>=
                pathname >>= send actionChannel <<< Ex.UpdateView <<< match

    pure app
