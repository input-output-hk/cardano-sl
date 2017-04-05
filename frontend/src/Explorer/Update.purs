module Explorer.Update where

import Prelude
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.SocketIO.Client (SocketIO, emit, emit')
import DOM (DOM)
import DOM.HTML.HTMLInputElement (select)
import Data.Array (difference, (:))
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Lens ((^.), over, set)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Explorer.Api.Http (fetchAddressSummary, fetchBlockSummary, fetchBlockTxs, fetchLatestBlocks, fetchLatestTxs, fetchTxSummary, searchEpoch)
import Explorer.Api.Socket (toEvent)
import Explorer.Lenses.State (addressDetail, addressTxPagination, blockDetail, blockTxPagination, blocksExpanded, connected, connection, currentAddressSummary, currentBlockSummary, currentBlockTxs, currentCAddress, currentTxSummary, dashboard, dashboardBlockPagination, errors, handleLatestBlocksSocketResult, handleLatestTxsSocketResult, initialBlocksRequested, initialTxsRequested, latestBlocks, latestTransactions, loading, searchInput, searchQuery, selectedApiCode, selectedSearch, socket, subscriptions, transactionsExpanded, viewStates)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (emptySearchQuery)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (SocketSubscription(..), Search(..), State)
import Explorer.Util.DOM (scrollTop)
import Explorer.Util.Factory (mkCAddress, mkCTxId, mkEpochIndex, mkLocalSlotIndex)
import Explorer.Util.QrCode (generateQrCode)
import Explorer.Util.String (parseSearchEpoch)
import Network.HTTP.Affjax (AJAX)
import Network.RemoteData (RemoteData(..), _Success)
import Pos.Explorer.Socket.Methods (ClientEvent(..), Subscription(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, _CAddressSummary, caAddress)
import Pux (EffModel, noEffects)
import Pux.Router (navigateTo) as P


update :: forall eff. Action -> State ->
    EffModel State Action
    (dom :: DOM
    , ajax :: AJAX
    , socket :: SocketIO
    -- , console :: CONSOLE
    | eff
    )

-- Language

update (SetLanguage lang) state = noEffects $ state { lang = lang }


-- Socket

update (SocketConnected connected') state =
    { state: set (socket <<< connected) connected' state
    , effects:
          [ if connected'
            then pure SocketReconnectSubscriptions
            else pure NoOp
          ]
    }
update (SocketBlocksUpdated (Right blocks)) state = noEffects $
    if state ^. handleLatestBlocksSocketResult
    -- add incoming blocks ahead of previous blocks
    then over (latestBlocks <<< _Success) (\b -> blocks <> b) state
    else state
update (SocketBlocksUpdated (Left error)) state = noEffects $
    set latestBlocks (Failure error) $
    -- add incoming errors ahead of previous errors
    over errors (\errors' -> (show error) : errors') state
update (SocketTxsUpdated (Right transactions)) state = noEffects $
    -- add incoming transactions ahead of previous transactions
    over latestTransactions (\t -> transactions <> t) state
update (SocketTxsUpdated (Left error)) state = noEffects $
    -- add incoming errors ahead of previous errors
    over errors (\errors' -> (show error) : errors') state
update SocketCallMe state =
    { state
    , effects : [ do
          _ <- case state ^. (socket <<< connection) of
              Just socket' -> liftEff <<< emit' socket' $ toEvent CallMe
              Nothing -> pure unit
          pure NoOp
    ]}
update (SocketCallMeString str) state =
    { state
    , effects : [ do
          _ <- case state ^. (socket <<< connection) of
              Just socket' -> liftEff $ emit socket' (toEvent CallMeString) str
              Nothing -> pure unit
          pure NoOp
    ]}
update (SocketCallMeCTxId id) state =
    { state
    , effects : [ do
          _ <- case state ^. (socket <<< connection) of
              Just socket' -> liftEff $ emit socket' (toEvent CallMeTxId) id
              Nothing -> pure unit
          pure NoOp
    ]}

update (SocketUpdateSubscriptions nextSubs) state =
    let currentSubs = state ^. socket <<< subscriptions in
    { state: set (socket <<< subscriptions) nextSubs state
    , effects : [ do
          _ <- case state ^. (socket <<< connection) of
              Just socket' -> do
                -- 1. unsubscribe all not used events
                let diffNextFromCurrentSubs = difference currentSubs nextSubs
                traverse_ (liftEff <<< emit' socket' <<< toEvent <<< Unsubscribe <<< unwrap) diffNextFromCurrentSubs
                -- 2. subscribe all new subscriptions
                let diffCurrentFromNextSubs = difference nextSubs currentSubs
                traverse_ (liftEff <<< emit' socket' <<< toEvent <<< Subscribe <<< unwrap) diffCurrentFromNextSubs
              Nothing -> pure unit
          pure NoOp
    ]}

update SocketReconnectSubscriptions state =
    let currentSubs = state ^. socket <<< subscriptions in
    { state
    , effects : [ do
          _ <- case state ^. (socket <<< connection) of
              Just socket' -> do
                traverse_ (liftEff <<< emit' socket' <<< toEvent <<< Subscribe <<< unwrap) currentSubs
              Nothing -> pure unit
          pure NoOp
    ]}

-- Dashboard

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

update (AddressPaginateTxs value) state = noEffects $
    set (viewStates <<< addressDetail <<< addressTxPagination) value state

-- Block

update (BlockPaginateTxs value) state = noEffects $
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
update (GenerateQrCode address) state =
    { state
    , effects:
        [ liftEff $ generateQrCode (address ^. _CAddress) "qr_image_id" *> pure NoOp
        ]
    }

-- Search

-- update DashboardSearch state = noEffects state
update DashboardSearch state =
    let query = state ^. searchQuery in
    { state: set searchQuery emptySearchQuery $ state
    , effects: [
      -- set state of focus explicitly
      pure $ DashboardFocusSearchInput false
      , case state ^. selectedSearch of
          SearchAddress ->
              (liftEff <<< P.navigateTo <<< toUrl <<< Address $ mkCAddress query) *> pure NoOp
          SearchTx ->
              (liftEff <<< P.navigateTo <<< toUrl <<< Tx $ mkCTxId query) *> pure NoOp
          SearchEpoch ->
              case parseSearchEpoch query of
                  Right (Tuple (Just epoch) (Just slot)) ->
                      let epochIndex = mkEpochIndex epoch
                          slotIndex  = mkLocalSlotIndex slot
                          epochSlotUrl = EpochSlot epochIndex slotIndex
                      in
                      (liftEff <<< P.navigateTo <<< toUrl $ epochSlotUrl) *> pure NoOp
                  Right (Tuple (Just epoch) Nothing) ->
                      let epochIndex = mkEpochIndex epoch
                          epochUrl   = Epoch $ epochIndex
                      in
                      (liftEff <<< P.navigateTo <<< toUrl $ epochUrl) *> pure NoOp

                  _ -> pure NoOp -- TODO (ks) maybe put up a message?
      ]
    }

update (UpdateSelectedSearch search) state =
    noEffects $ set selectedSearch search state

update (UpdateSearchText search) state =
    noEffects $ set searchQuery search state

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
    set latestBlocks (Success blocks) $
    state

update (ReceiveInitialBlocks (Left error)) state =
    noEffects $
    set loading false <<<
    set initialBlocksRequested true <<<
    set handleLatestBlocksSocketResult true $
    set latestBlocks (Failure error) $
    over errors (\errors' -> (show error) : errors') state

update (RequestBlockSummary hash) state =
    { state: set loading true $ state
    , effects: [ attempt (fetchBlockSummary hash) >>= pure <<< ReceiveBlockSummary ]
    }
update (ReceiveBlockSummary (Right blockSummary)) state =
    noEffects $
    set loading false $
    set currentBlockSummary (Just blockSummary) state
update (ReceiveBlockSummary (Left error)) state =
    noEffects $
    set loading false $
    over errors (\errors' -> (show error) : errors') state

-- Epoch, slot

update (RequestEpochSlot epoch slot) state =
    { state:
          set loading true $
          set latestBlocks Loading $state
    , effects: [ attempt (searchEpoch epoch slot) >>= pure <<< ReceiveEpochSlot ]
    }
update (ReceiveEpochSlot (Right blocks)) state =
    noEffects $
    set loading false <<<
    set initialBlocksRequested true <<<
    set handleLatestBlocksSocketResult true $
    set latestBlocks (Success blocks) $
    state

update (ReceiveEpochSlot (Left error)) state =
    noEffects $
    set loading false <<<
    set initialBlocksRequested true <<<
    set handleLatestBlocksSocketResult true $
    set latestBlocks (Failure error) $
    over errors (\errors' -> (show error) : errors') state

update (RequestBlockTxs hash) state =
    { state: set loading true $ state
    , effects: [ attempt (fetchBlockTxs hash) >>= pure <<< ReceiveBlockTxs ]
    }
update (ReceiveBlockTxs (Right txs)) state =
    noEffects $
    set loading false <<<
    set currentBlockTxs (Just txs) $
    state
update (ReceiveBlockTxs (Left error)) state =
    noEffects $
    set loading false $
    set currentBlockTxs Nothing $
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

update (RequestTxSummary id) state =
    { state: set loading true state
    , effects: [ attempt (fetchTxSummary id) >>= pure <<< ReceiveTxSummary ]
    }
update (ReceiveTxSummary (Right tx)) state =
    noEffects $
    set loading false $
    set currentTxSummary (Success tx) state
update (ReceiveTxSummary (Left error)) state =
    noEffects $
    set loading false $
    set currentTxSummary (Failure error) $
    over errors (\errors' -> (show error) : errors') state

update (RequestAddressSummary address) state =
    { state: set loading true state
    , effects: [ attempt (fetchAddressSummary address) >>= pure <<< ReceiveAddressSummary ]
    }
update (ReceiveAddressSummary (Right address)) state =
    { state:
        set loading false $
        set currentAddressSummary (Success address) state
    , effects:
        [ pure $ GenerateQrCode $ address ^. (_CAddressSummary <<< caAddress) ]
    }
update (ReceiveAddressSummary (Left error)) state =
    noEffects $
    set loading false $
    set currentAddressSummary (Failure error) $
    over errors (\errors' -> (show error) : errors') state

-- routing

update (UpdateView route) state = routeEffects route (state { route = route })

routeEffects :: forall eff. Route -> State -> EffModel State Action (dom :: DOM
    , ajax :: AJAX | eff)
routeEffects Dashboard state =
    { state
    , effects:
        [ pure ScrollTop
        , pure $ SocketUpdateSubscriptions [ SocketSubscription SubBlock, SocketSubscription SubTx ]
        , if not $ state ^. initialBlocksRequested
          then pure RequestInitialBlocks
          else pure NoOp
        , if not $ state ^. initialTxsRequested
          then pure RequestInitialTxs
          else pure NoOp
        ]
    }

routeEffects (Tx tx) state =
    { state:
        set currentTxSummary Loading state
    , effects:
        [ pure ScrollTop
        , pure $ SocketUpdateSubscriptions []
        , pure $ RequestTxSummary tx
        ]
    }

routeEffects (Address cAddress) state =
    { state:
        set currentAddressSummary Loading $
        set currentCAddress cAddress $
        set (viewStates <<< addressDetail <<< addressTxPagination) 1 state
    , effects:
        [ pure ScrollTop
        , pure $ SocketUpdateSubscriptions []
        , pure $ RequestAddressSummary cAddress
        ]
    }

routeEffects (Epoch epochIndex) state =
    { state
    , effects:
        [ pure ScrollTop
        , pure $ RequestEpochSlot epochIndex Nothing
        ]
    }

routeEffects (EpochSlot epochIndex slotIndex) state =
    { state
    , effects:
        [ pure ScrollTop
        , pure $ RequestEpochSlot epochIndex (Just slotIndex)
        ]
    }


routeEffects Calculator state = { state, effects: [ pure ScrollTop ] }

routeEffects (Block hash) state =
    { state: set currentBlockSummary Nothing state
    , effects:
        [ pure ScrollTop
        , pure $ SocketUpdateSubscriptions []
        , pure $ RequestBlockSummary hash
        , pure $ RequestBlockTxs hash
        ]
    }

routeEffects Playground state =
    { state
    , effects:
        [ pure ScrollTop
        , pure $ SocketUpdateSubscriptions []
        ]
    }

routeEffects NotFound state =
    { state
    , effects:
        [ pure ScrollTop
        , pure $ SocketUpdateSubscriptions []
        ]
    }
