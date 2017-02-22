module Explorer.Update where

import Prelude
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.HTML.HTMLInputElement (select)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Lens (over, set)
import Explorer.Api.Http (fetchLatestBlocks, fetchLatestTransactions)
import Explorer.Lenses.State (addressDetail, addressTxPagination, blockDetail, blockTxPagination, blocksExpanded, connected, dashboard, dashboardBlockPagination, errors, latestBlocks, latestTransactions, loading, searchInput, selectedApiCode, socket, transactionsExpanded, viewStates)
import Explorer.Routes (Route(..))
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Explorer.Util.DOM (scrollTop)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)



update :: forall eff. Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX | eff)

-- Language

update (SetLanguage lang) state = noEffects $ state { lang = lang }


-- Socket

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


-- Dashboard

update DashboardSearch state = noEffects state
update (DashboardExpandBlocks toggled) state = noEffects $
    set (viewStates <<< dashboard <<< blocksExpanded) toggled state
update (DashboardExpandTransactions toggled) state = noEffects $
    set (viewStates <<< dashboard <<< transactionsExpanded) toggled state
update (DashboardPaginateBlocks value) state = noEffects $
    set (viewStates <<< dashboard <<< dashboardBlockPagination) value state
update (DashboardShowAPICode code) state = noEffects $
    set (viewStates <<< dashboard <<< selectedApiCode) code state
update (DashboardFocusSearchInput value) state = noEffects $
    set (viewStates <<< dashboard <<< searchInput) value state

-- Address

update (AddressPaginateTransactions value) state = noEffects $
    set (viewStates <<< addressDetail <<< addressTxPagination) value state

-- Address

update (BlockPaginateTransactions value) state = noEffects $
    set (viewStates <<< blockDetail <<< blockTxPagination) value state

-- DOM side effects

update ScrollTop state = { state: state, effects: [
    liftEff scrollTop >>= \_ -> pure NoOp
  ]}
update (SelectInputText input) state = { state: state, effects: [
    liftEff $ select input >>= \_ -> pure NoOp
  ]}


-- NoOp

update NoOp state = noEffects state


-- Debugging
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

update (UpdateView route) state = routeEffects route (state { route = route })

routeEffects :: forall eff. Route -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX | eff)
routeEffects Dashboard state = { state, effects: [ pure ScrollTop ] }
routeEffects (Transaction hash) state = { state, effects: [ pure ScrollTop ] }
routeEffects Address state = { state, effects: [ pure ScrollTop ] }
routeEffects Calculator state = { state, effects: [ pure ScrollTop ] }
routeEffects (Block hash) state = { state, effects: [ pure ScrollTop ] }
routeEffects NotFound state = { state, effects: [ pure ScrollTop ] }
