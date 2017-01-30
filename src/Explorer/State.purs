module Explorer.State where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import Explorer.I18n.Lang (Language(..))
import Explorer.Routes (Route(..))
import Explorer.Types (State, Action(..))
import Explorer.Util.DOM (scrollTop)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)


initialState :: State
initialState =
    { lang: English
    , route: Dashboard
    , viewStates: {
        dashboard:
        { blocksExpanded: false
        ,  transactionsExpanded: false
        }
      }
    }


-- update

update :: forall eff. Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX | eff)
update (SetLanguage lang) state = noEffects $ state { lang = lang }
update (UpdateView route) state = routeEffects route (state { route = route })
update ScrollTop state = { state: state, effects: [ do
    _ <- liftEff $ scrollTop
    pure NoOp
  ]}
update (DashboardExpandBlocks toggled) state = noEffects $ state {
        viewStates = state.viewStates {
            dashboard = state.viewStates.dashboard { blocksExpanded = toggled }
        }
    }
update (DashboardExpandTransactions toggled) state = noEffects $ state {
        viewStates = state.viewStates {
            dashboard = state.viewStates.dashboard { transactionsExpanded = toggled }
        }
    }
update NoOp state = noEffects state


-- routing

routeEffects :: forall eff. Route -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX | eff)
routeEffects Dashboard state = { state, effects: [ pure ScrollTop ] }
routeEffects Transaction state = { state, effects: [ pure ScrollTop ] }
routeEffects Address state = { state, effects: [ pure ScrollTop ] }
routeEffects Calculator state = { state, effects: [ pure ScrollTop ] }
routeEffects Block state = { state, effects: [ pure ScrollTop ] }
routeEffects NotFound state = { state, effects: [ pure ScrollTop ] }
