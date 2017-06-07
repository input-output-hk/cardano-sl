module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.SocketIO.Client (SocketIO, connect, on)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (click)
import DOM.HTML.Types (htmlDocumentToEventTarget)
import DOM.HTML.Window (document)
import Data.Lens ((^.), set)
import Data.Maybe (Maybe(..), fromMaybe)
import Explorer.Api.Socket (blocksPageUpdatedEventHandler, callYouEventHandler, mkSocketHost, connectEvent, closeEvent, connectHandler, closeHandler, toEvent, txsUpdatedHandler) as Ex
import Explorer.I18n.Lang (Language(..), detectLocale)
import Explorer.Lenses.State (connection, lang, socket, syncAction)
import Explorer.Routes (match)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.Actions (Action(..), ActionChannel) as Ex
import Explorer.Types.State (State) as Ex
import Explorer.Update (update) as Ex
import Explorer.Util.Config (SyncAction(..), hostname, isProduction, secureProtocol)
import Explorer.View.Layout (view)
import Network.HTTP.Affjax (AJAX)
import Pos.Explorer.Socket.Methods (ServerEvent(..))
import Prelude (bind, const, pure, ($), (*), (<$>), (<<<), (<>), (>>=), (>>>))
import Pux (App, Config, CoreEffects, Update, renderToDOM, start)
import Pux.Devtool (Action, start) as Pux.Devtool
import Pux.Router (sampleUrl)
import Signal (Signal, (~>))
import Signal.Channel (channel, send, subscribe)
import Signal.Time (every, second)
import Waypoints (WAYPOINT)

type AppEffects =
    ( dom :: DOM
    , ajax :: AJAX
    , socket :: SocketIO
    , now :: NOW
    , waypoint :: WAYPOINT
    , console :: CONSOLE
    )

type AppConfig = (Config Ex.State Ex.Action AppEffects)

-- | Config to synchronize data by socket-io
socketConfig :: AppConfig -> Ex.ActionChannel -> Eff (CoreEffects AppEffects) AppConfig
socketConfig appConfig actionChannel = do
    -- socket
    let pingSignal = every (10.0 * second) ~> const Ex.SocketPing
    socketHost <- Ex.mkSocketHost (secureProtocol isProduction) <$> hostname
    socket' <- connect socketHost
    on socket' Ex.connectEvent $ Ex.connectHandler actionChannel
    on socket' Ex.closeEvent $ Ex.closeHandler actionChannel
    on socket' (Ex.toEvent TxsUpdated) $ Ex.txsUpdatedHandler actionChannel
    on socket' (Ex.toEvent BlocksLastPageUpdated) $ Ex.blocksPageUpdatedEventHandler actionChannel
    -- Note:
    -- `CallYou` is the answer of `CallMe`.
    -- Handling both events are needed a to be connected with socket.io manually
    on socket' (Ex.toEvent CallYou) $ Ex.callYouEventHandler actionChannel
--  on socket' (Ex.toEvent CallYouString) $ Ex.callYouStringEventHandler actionChannel
--  on socket' (Ex.toEvent CallYouTxId) $ Ex.callYouCTxIdEventHandler actionChannel
    pure $ appConfig
        { initialState = set (socket <<< connection) (Just socket') appConfig.initialState
        , inputs = [ pingSignal ] <> appConfig.inputs
        }

-- | Config to synchronize data by polling
pollingConfig :: AppConfig -> Eff (CoreEffects AppEffects) AppConfig
pollingConfig appConfig =
    let reloadSignal = every (60.0 * second) ~> const Ex.Reload in
    pure $ appConfig
        { inputs = [ reloadSignal ] <> appConfig.inputs
        }

-- | Common config
commonConfig :: Ex.State -> Ex.ActionChannel -> Eff (CoreEffects AppEffects) AppConfig
commonConfig state actionChannel = do
    -- routing
    urlSignal <- sampleUrl
    let routeSignal = urlSignal ~> Ex.UpdateView <<< match
    -- timer
    let clockSignal = every second ~> const Ex.UpdateClock
    -- detected locale
    locale <- fromMaybe English <$> detectLocale
    -- register actionSignal
    let actionSignal = subscribe actionChannel :: Signal Ex.Action
    -- register global (document) click listener
    -- globalClickListener :: forall eff. Event -> Eff (channel :: CHANNEL | eff) Unit
    let globalClickListener event = send actionChannel $ DocumentClicked event

    window >>=
        document >>=
            htmlDocumentToEventTarget >>>
                addEventListener click (eventListener globalClickListener) false

    pure
        { initialState: set lang locale state
        , update: Ex.update :: Update Ex.State Ex.Action AppEffects
        , view: view
        , inputs:
              [ clockSignal
              , actionSignal
              -- Important note:
              -- routeSignal has to be the last signal in row !!!
              , routeSignal
              ]
        }

appSelector :: String
appSelector = "#explorer"

main :: Ex.State -> Eff (CoreEffects AppEffects) (App Ex.State Ex.Action)
main state = do
    actionChannel <- channel NoOp
    appConfig <- commonConfig state actionChannel
    config <- case state ^. syncAction of
                  SyncByPolling -> pollingConfig appConfig
                  SyncBySocket -> socketConfig appConfig actionChannel
    app <- start config
    renderToDOM appSelector app.html
    pure app

debug :: Ex.State -> Eff (CoreEffects AppEffects) (App Ex.State (Pux.Devtool.Action Ex.Action))
debug state = do
    actionChannel <- channel NoOp
    appConfig <- commonConfig state actionChannel
    config <- case state ^. syncAction of
                    SyncByPolling -> pollingConfig appConfig
                    SyncBySocket -> socketConfig appConfig actionChannel
    app <- Pux.Devtool.start config {opened: false}
    renderToDOM appSelector app.html
    pure app
