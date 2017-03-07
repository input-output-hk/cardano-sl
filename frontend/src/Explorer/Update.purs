module Explorer.Update where

import Prelude
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.HTML.HTMLInputElement (select)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Lens ((^.), over, set)
import Data.Maybe (Maybe(..))
import Explorer.Api.Http (fetchBlockSummary, fetchBlockTxs, fetchLatestBlocks, fetchLatestTxs)
import Explorer.Lenses.State (addressDetail, addressTxPagination, blockDetail, blockTxPagination, blocksExpanded, currentAddressSummary, currentBlockTxs, connected, dashboard, dashboardBlockPagination, errors, handleLatestBlocksSocketResult, initialBlocksRequested, initialTxsRequested, handleLatestTxsSocketResult, currentBlock, latestBlocks, latestTransactions, loading, searchInput, selectedApiCode, socket, transactionsExpanded, viewStates)
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
    if state ^. handleLatestBlocksSocketResult
    -- add incoming blocks ahead of previous blocks
    then over latestBlocks (\b -> blocks <> b) state
    else state
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

-- Block

update (BlockPaginateTransactions value) state = noEffects $
    set (viewStates <<< blockDetail <<< blockTxPagination) value state

-- DOM side effects

update ScrollTop state =
    { state
    , effects:
        [ liftEff scrollTop >>= \_ -> pure NoOp
        ]
    }
update (SelectInputText input) state =
    { state
    , effects:
        [ liftEff $ select input >>= \_ -> pure NoOp
        ]
    }


-- NoOp

update NoOp state = noEffects state

-- http endpoints

update RequestInitialBlocks state =
    { state: set loading true $ state
    , effects: [ attempt fetchLatestBlocks >>= pure <<< ReceiveInitialBlocks ]
    }
update (ReceiveInitialBlocks (Right blocks)) state =
    noEffects $
    set loading false <<<
    set initialBlocksRequested true <<<
    set handleLatestBlocksSocketResult true $
    set latestBlocks blocks $
    state

update (ReceiveInitialBlocks (Left error)) state =
    noEffects $
    set loading false <<<
    set initialBlocksRequested true <<<
    set handleLatestBlocksSocketResult true $
    over errors (\errors' -> (show error) : errors') state

update (RequestBlockSummary hash) state =
    { state: set loading true $ state
    , effects: [ attempt (fetchBlockSummary hash) >>= pure <<< ReceiveBlockSummary ]
    }
update (ReceiveBlockSummary (Right block)) state =
    noEffects $
    set loading false $
    set currentBlock (Just block) state
update (ReceiveBlockSummary (Left error)) state =
    noEffects $
    set loading false $
    over errors (\errors' -> (show error) : errors') state

update (RequestBlockTxs hash) state =
    { state: set loading true $ state
    , effects: [ attempt (fetchBlockTxs hash) >>= pure <<< ReceiveBlockTxs ]
    }
update (ReceiveBlockTxs (Right txs)) state =
    noEffects $
    set loading false <<<
    set currentBlockTxs txs $
    state
update (ReceiveBlockTxs (Left error)) state =
    noEffects $
    set loading false $
    over errors (\errors' -> (show error) : errors') state

update RequestInitialTxs state =
    { state: set loading true state
    , effects: [ attempt fetchLatestTxs >>= pure <<< ReceiveInitialTxs ]
    }
update (ReceiveInitialTxs (Right blocks)) state =
    noEffects $
    set loading false <<<
    set initialTxsRequested true <<<
    set handleLatestTxsSocketResult true $
    over latestTransactions (\b -> blocks <> b) state
update (ReceiveInitialTxs (Left error)) state = noEffects $
    set loading false <<<
    set initialTxsRequested true <<<
    set handleLatestTxsSocketResult true $
    over errors (\errors' -> (show error) : errors') state

update (RequestAddressSummary address) state =
    { state: set loading true state
    , effects: [ attempt fetchLatestTxs >>= pure <<< ReceiveInitialTxs ]
    }
update (ReceiveAddressSummary (Right address)) state =
    noEffects $
    set loading false $
    set currentAddressSummary (Just address) state
update (ReceiveAddressSummary (Left error)) state =
    noEffects $
    set loading false $
    over errors (\errors' -> (show error) : errors') state

-- routing

update (UpdateView route) state = routeEffects route (state { route = route })

routeEffects :: forall eff. Route -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX | eff)
routeEffects Dashboard state =
    { state
    , effects:
        [ pure ScrollTop
        , if not $ state ^. initialBlocksRequested
          then pure RequestInitialBlocks
          else pure NoOp
        , if not $ state ^. initialTxsRequested
          then pure RequestInitialTxs
          else pure NoOp
        ]
    }
routeEffects (Transaction hash) state = { state, effects: [ pure ScrollTop ] }
routeEffects (Address address) state =
    { state: set currentAddressSummary Nothing state
    , effects:
        [ pure ScrollTop
        , pure $ RequestAddressSummary address
        ]
    }
routeEffects Calculator state = { state, effects: [ pure ScrollTop ] }
routeEffects (Block hash) state =
    { state: set currentBlock Nothing state
    , effects:
        [ pure ScrollTop
        , pure $ RequestBlockSummary hash
        , pure $ RequestBlockTxs hash
        ]
    }
routeEffects NotFound state = { state, effects: [ pure ScrollTop ] }
