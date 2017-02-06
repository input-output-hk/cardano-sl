module Explorer.Update where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import Data.Lens (set)
import Explorer.Lenses.State (blocksExpanded, connected, dashboard, searchInput, selectedApiCode, socket, transactionsExpanded, viewStates)
import Explorer.Routes (Route(..))
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Explorer.Util.DOM (scrollTop)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)


-- update

update :: forall eff. Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX | eff)
update (SetLanguage lang) state = noEffects $ state { lang = lang }
update (UpdateView route) state = routeEffects route (state { route = route })
update (SocketConnected status) state = noEffects $
  set (socket <<< connected) status state
update Search state = noEffects state
update ScrollTop state = { state: state, effects: [
    liftEff scrollTop >>= \_ -> pure NoOp
  ]}
update (DashboardExpandBlocks toggled) state = noEffects $
    set (viewStates <<< dashboard <<< blocksExpanded) toggled state
update (DashboardExpandTransactions toggled) state = noEffects $
    set (viewStates <<< dashboard <<< transactionsExpanded) toggled state
update (DashboardShowAPICode code) state = noEffects $
    set (viewStates <<< dashboard <<< selectedApiCode) code state
update (DashboardFocusSearchInput value) state = noEffects $
    set (viewStates <<< dashboard <<< searchInput) value state
update NoOp state = noEffects state


-- routing

routeEffects :: forall eff. Route -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX | eff)
routeEffects Dashboard state = { state, effects: [ pure ScrollTop ] }
routeEffects Transaction state = { state, effects: [ pure ScrollTop ] }
routeEffects Address state = { state, effects: [ pure ScrollTop ] }
routeEffects Calculator state = { state, effects: [ pure ScrollTop ] }
routeEffects Block state = { state, effects: [ pure ScrollTop ] }
routeEffects NotFound state = { state, effects: [ pure ScrollTop ] }
