module Explorer.Update where

import Prelude
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Lens (over, set)
import Explorer.Api (fetchLatestBlocks, fetchLatestTransactions)
import Explorer.Lenses.State (blocksExpanded, connected, errors, dashboard, latestBlocks, latestTransactions, loading, searchInput, selectedApiCode, socket, transactionsExpanded, viewStates)
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
update (SocketLatestBlocks (Right blocks)) state = noEffects $
    -- add incoming blocks ahead of previous blocks
     over latestBlocks (\b -> blocks <> b) state
update (SocketLatestBlocks (Left error)) state = noEffects $
    -- add incoming errors ahead of previous errors
    over errors (\errors' -> (show error) : errors') state
update (SocketLatestTransactions (Right transactions)) state = noEffects $
    -- add incoming transactions ahead of previous transactions
     over latestTransactions (\t -> transactions <> t) state
update (SocketLatestTransactions (Left error)) state = noEffects $
    -- add incoming errors ahead of previous errors
    over errors (\errors' -> (show error) : errors') state
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

-- TODO (jk): all of following actions are for debugging only and have to be removed later on
update RequestLatestBlocks state =
    { state: set loading true state
    , effects: [ attempt fetchLatestBlocks >>= pure <<< ReceiveLatestBlocks ]
    }
update (ReceiveLatestBlocks (Right blocks)) state = noEffects $
    set loading false $ over latestBlocks (\b -> blocks <> b) state
update (ReceiveLatestBlocks (Left error)) state = noEffects $
    set loading false $ over errors (\errors' -> (show error) : errors') state
update RequestLatestTransactions state =
    { state: set loading true state
    , effects: [ attempt fetchLatestTransactions >>= pure <<< ReceiveLatestTransactions ]
    }
update (ReceiveLatestTransactions (Right blocks)) state = noEffects $
    set loading false $ over latestTransactions (\b -> blocks <> b) state
update (ReceiveLatestTransactions (Left error)) state = noEffects $
    set loading false $ over errors (\errors' -> (show error) : errors') state

-- routing

routeEffects :: forall eff. Route -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX | eff)
routeEffects Dashboard state = { state, effects: [ pure ScrollTop ] }
routeEffects Transaction state = { state, effects: [ pure ScrollTop ] }
routeEffects Address state = { state, effects: [ pure ScrollTop ] }
routeEffects Calculator state = { state, effects: [ pure ScrollTop ] }
routeEffects Block state = { state, effects: [ pure ScrollTop ] }
routeEffects NotFound state = { state, effects: [ pure ScrollTop ] }
