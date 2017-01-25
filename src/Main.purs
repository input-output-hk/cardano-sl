module Main where

import Prelude (bind, pure, (<<<))
import Control.Monad.Eff (Eff)
import Network.HTTP.Affjax (AJAX)
import DOM (DOM)
import Control.Bind ((=<<))
import Signal ((~>))

import Pux (App, Config, CoreEffects, Update, renderToDOM, start)
import Pux.Router (sampleUrl)
import Pux.Devtool (Action, start) as Pux.Devtool

import Explorer.View.Layout (view)
import Explorer.State (update) as Ex
import Explorer.Types (Action(..), State) as Ex
import Explorer.Routes (match)

type AppEffects = (dom :: DOM, ajax :: AJAX)

config :: forall eff. Ex.State -> Eff (dom :: DOM, ajax :: AJAX | eff)
    (Config Ex.State Ex.Action AppEffects)
config state = do
  urlSignal <- sampleUrl
  let routeSignal = urlSignal ~> Ex.UpdateView <<< match
  pure
    { initialState: state
    , update: Ex.update :: Update Ex.State Ex.Action AppEffects
    , view: view
    , inputs: [routeSignal]
    }

main :: Ex.State -> Eff (CoreEffects AppEffects) (App Ex.State Ex.Action)
main state = do
  app <- start =<< config state
  renderToDOM "#explorer" app.html
  pure app

debug :: Ex.State -> Eff (CoreEffects AppEffects) (App Ex.State (Pux.Devtool.Action Ex.Action))
debug state = do
  appConfig <- config state
  app <- Pux.Devtool.start appConfig {opened: false}
  renderToDOM "#explorer" app.html
  pure app
