module App where

import App.Example (Action, State, update, view)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Control.Bind ((=<<))
import Prelude (pure, bind)
import Pux (App, Config, CoreEffects, fromSimple, renderToDOM, start)
import Pux.Devtool (Action, start) as Pux.Devtool

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
  app <- Pux.Devtool.start =<< config state
  renderToDOM "#app" app.html
  pure app
