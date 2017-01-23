module Explorer.State where

import Prelude
import Network.HTTP.Affjax (AJAX)
import DOM (DOM)

import Pux (EffModel, noEffects)

import Explorer.Routes (Route(..))
import Explorer.I18n.Lang (Language(English))


-- State

type State =
    { lang :: Language
    , route :: Route
    , count :: Int
    }

initialState :: State
initialState =
    { lang: English
    , route: Dashboard
    , count: 0
    }


-- Actions

data Action
    = Count
    | SetLanguage Language
    | UpdateView Route
    | NoOp


-- update

update :: forall eff. Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX | eff)
update Count state = noEffects $ state { count = state.count + 1 }
update (SetLanguage lang) state = noEffects $ state { lang = lang }
update (UpdateView route) state = routeEffects route (state { route = route })
update NoOp state = noEffects state


-- routing

routeEffects :: forall eff. Route -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX | eff)
routeEffects Dashboard state = noEffects $ state
routeEffects Transaction state = noEffects $ state
routeEffects Address state = noEffects $ state
routeEffects Calculator state = noEffects $ state
routeEffects NotFound state = noEffects $ state
