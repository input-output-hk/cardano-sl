module Main where

import Explorer.View.Layout (view)
import Explorer.State (Action, State, update)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Control.Bind ((=<<))
import Prelude (pure, bind, ($))
import Pux (App, Config, CoreEffects, fromSimple, renderToDOM, start)
import Pux.Devtool (Action, start, defaultOptions) as Pux.Devtool

type AppEffects = (dom :: DOM)

config :: forall eff. State -> Eff (dom :: DOM | eff) (Config State Action AppEffects)
config state =
  pure
    { initialState: state
    , update: fromSimple update
    , view: view
    , inputs: []
    }

main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  app <- start =<< config state
  renderToDOM "#app" app.html
  pure app

debug :: State -> Eff (CoreEffects AppEffects) (App State (Pux.Devtool.Action Action))
debug state = do
  appConfig <- config state
  -- app <- Pux.Devtool.start appConfig Pux.Devtool.defaultOptions
  app <- Pux.Devtool.start appConfig {opened: false}
  renderToDOM "#app" app.html
  pure app
