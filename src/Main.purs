module Main where

import Prelude (($), (<<<), bind, pure)
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Control.SocketIO.Client (SocketIO, connect, on)
import DOM (DOM)
import Explorer.Routes (match)
import Explorer.Api.Socket (socketHost, connectEvent, closeEvent, connectHandler, closeHandler, lastestBlocksEvent, latestBlocksHandler, lastestTransactionsEvent, latestTransactionsHandler) as Ex
import Explorer.Types.Actions (Action(..)) as Ex
import Explorer.Types.State (State) as Ex
import Explorer.Update (update) as Ex
import Explorer.View.Layout (view)
import Network.HTTP.Affjax (AJAX)
import Pux (App, Config, CoreEffects, Update, renderToDOM, start)
import Pux.Devtool (Action, start) as Pux.Devtool
import Pux.Router (sampleUrl)
import Signal (Signal, (~>))
import Signal.Channel (channel, subscribe)

type AppEffects = (dom :: DOM, ajax :: AJAX, socket :: SocketIO)

config :: Ex.State -> Eff (CoreEffects AppEffects) (Config Ex.State Ex.Action AppEffects)
config state = do
  -- routing
  urlSignal <- sampleUrl
  let routeSignal = urlSignal ~> Ex.UpdateView <<< match
  -- socket
  actionChannel <- channel $ Ex.SocketConnected false
  let socketSignal = subscribe actionChannel :: Signal Ex.Action
  -- socket <- connect Ex.socketHost
  -- on socket Ex.connectEvent $ Ex.connectHandler actionChannel
  -- on socket Ex.closeEvent $ Ex.closeHandler actionChannel
  -- on socket Ex.lastestBlocksEvent $ Ex.latestBlocksHandler actionChannel
  -- on socket Ex.lastestTransactionsEvent $ Ex.latestTransactionsHandler actionChannel

  pure
    { initialState: state
    , update: Ex.update :: Update Ex.State Ex.Action AppEffects
    , view: view
    , inputs: [socketSignal, routeSignal]
    }

appSelector :: String
appSelector = "#explorer"

main :: Ex.State -> Eff (CoreEffects AppEffects) (App Ex.State Ex.Action)
main state = do
  app <- start =<< config state
  renderToDOM appSelector app.html
  pure app

debug :: Ex.State -> Eff (CoreEffects AppEffects) (App Ex.State (Pux.Devtool.Action Ex.Action))
debug state = do
  appConfig <- config state
  app <- Pux.Devtool.start appConfig {opened: false}
  renderToDOM appSelector app.html
  pure app
