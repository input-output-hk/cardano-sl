module Explorer.Update where

import Prelude
import Control.Comonad (extract)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (nowDateTime, NOW)
import Control.SocketIO.Client (Socket, SocketIO, emit, emit')
import DOM (DOM)
import DOM.HTML.HTMLElement (blur)
import DOM.HTML.HTMLInputElement (select)
import Data.Array (filter, length, snoc, take, (:))
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Int (fromString)
import Data.Lens ((^.), over, set)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Explorer.Api.Http (fetchAddressSummary, fetchBlockSummary, fetchBlockTxs, fetchLatestBlocks, fetchLatestTxs, fetchTotalBlocks, fetchTxSummary, searchEpoch)
import Explorer.Api.Socket (toEvent)
import Explorer.Api.Types (RequestLimit(..), RequestOffset(..), SocketSubscription(..))
import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (common, cAddress, cBlock, cCalculator, cEpoch, cSlot, cTitle, cTransaction, notfound, nfTitle) as I18nL
import Explorer.Lenses.State (addressDetail, addressTxPagination, addressTxPaginationEditable, blockDetail, blockTxPagination, blockTxPaginationEditable, blocksViewState, blsViewPagination, blsViewPaginationEditable, connected, connection, currentAddressSummary, currentBlockSummary, currentBlockTxs, currentBlocksResult, currentCAddress, currentTxSummary, dbViewBlockPagination, dbViewBlockPaginationEditable, dbViewBlocksExpanded, dbViewLoadingBlockPagination, dbViewLoadingTotalBlocks, dbViewNextBlockPagination, dbViewSelectedApiCode, dbViewTxsExpanded, errors, gViewMobileMenuOpenend, gViewSearchInputFocused, gViewSearchQuery, gViewSearchTimeQuery, gViewSelectedSearch, gViewTitle, globalViewState, lang, latestBlocks, latestTransactions, loading, socket, subscriptions, syncAction, totalBlocks, viewStates)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (addressQRImageId, emptySearchQuery, emptySearchTimeQuery, minPagination)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (Search(..), State)
import Explorer.Util.Config (SyncAction(..), syncBySocket)
import Explorer.Util.DOM (scrollTop, targetToHTMLElement, targetToHTMLInputElement)
import Explorer.Util.Data (sortBlocksByEpochSlot', sortTxsByTime', unionBlocks, unionTxs)
import Explorer.Util.Factory (mkCAddress, mkCTxId, mkEpochIndex, mkLocalSlotIndex)
import Explorer.Util.QrCode (generateQrCode)
import Explorer.View.Blocks (maxBlockRows)
import Explorer.View.Dashboard.Lenses (dashboardViewState)
import Explorer.View.Dashboard.Transactions (maxTransactionRows)
import Network.HTTP.Affjax (AJAX)
import Network.RemoteData (RemoteData(..), withDefault)
import Pos.Explorer.Socket.Methods (ClientEvent(..), Subscription(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, _CAddressSummary, caAddress)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.Router (navigateTo) as P

update :: forall eff. Action -> State ->
    EffModel State Action
    ( dom :: DOM
    , ajax :: AJAX
    , socket :: SocketIO
    , now :: NOW
    , console :: CONSOLE
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

update (SocketBlocksUpdated (Right blocks)) state =
    noEffects $
    set latestBlocks (Success newBlocks') $
    set totalBlocks (Success newTotalBlocks) state
    where
        prevBlocks = withDefault [] $ state ^. latestBlocks
        newBlocks = sortBlocksByEpochSlot' $ unionBlocks blocks prevBlocks
        -- make sure that we don't have more blocks than before in current page (async problem)
        -- TODO: (jk) This re-calculation can be removed with `CSE-120`
        newBlocks' = take maxBlockRows newBlocks
        previousTotalBlocks = withDefault 0 $ state ^. totalBlocks
        newTotalBlocks = (length newBlocks) - (length prevBlocks) + previousTotalBlocks

update (SocketBlocksUpdated (Left error)) state = noEffects $
    set latestBlocks (Failure error) $
    -- Important note:
    -- Don't set `latestBlocks` to (Failure error) here
    -- because we would lost all the previous (valid) data in `latestBlocks`
    -- and in the UI. So just add incoming errors ahead of previous errors.
    over errors (\errors' -> (show error) : errors') state

update (SocketTxsUpdated (Right txs)) state =
    noEffects $
    over latestTransactions
        (\currentTxs -> Success <<<
                            sortTxsByTime' <<<
                            take maxTransactionRows $
                                unionTxs txs $
                                    withDefault [] currentTxs
        )
    state

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

-- | Creates a new socket subscription
update (SocketAddSubscription sub) state =
    { state:
          over (socket <<< subscriptions) (\subs -> snoc subs sub) state
    , effects : [ do
          _ <- case state ^. (socket <<< connection) of
              Just socket' -> liftEff $ socketSubscribeEvent socket' sub
              Nothing -> pure unit
          pure NoOp
    ]}

-- | Removes an existing socket subscription
update (SocketRemoveSubscription sub) state =
    { state:
          over (socket <<< subscriptions) (filter ((/=) sub)) state
    , effects : [ do
          _ <- case state ^. (socket <<< connection) of
              Just socket' -> liftEff $ socketUnsubscribeEvent socket' sub
              Nothing -> pure unit
          pure NoOp
    ]}

-- | It subscribes a list of new subscriptions
-- | and unsubscribes all previous subscriptions
-- | We do need such an action handler on every page changes
update (SocketUpdateSubscriptions subs) state =
    { state:
          set (socket <<< subscriptions) subs state
    , effects : [ do
          _ <- case state ^. (socket <<< connection) of
              Just socket' -> do
                  -- 1. Unsubscribe all existing subscriptions
                  traverse_ (liftEff <<< socketUnsubscribeEvent socket')
                      (state ^. socket <<< subscriptions)
                  -- 2. Subscribe to all new subscriptions
                  traverse_ (liftEff <<< socketSubscribeEvent socket') subs
              Nothing -> pure unit
          pure NoOp
    ]}

update SocketReconnectSubscriptions state =
    let currentSubs = state ^. socket <<< subscriptions in
    { state
    , effects : [ do
          _ <- case state ^. (socket <<< connection) of
              Just socket' -> do
                -- first unsubscribe from existing subscriptions
                traverse_ (liftEff <<< socketUnsubscribeEvent socket') currentSubs
                -- then subscribe to them again
                traverse_ (liftEff <<< socketSubscribeEvent socket') currentSubs

              Nothing -> pure unit
          pure NoOp
    ]}

-- Dashboard

update (DashboardExpandBlocks expanded) state = noEffects $
    set (dashboardViewState <<< dbViewBlocksExpanded) expanded state

update (DashboardExpandTransactions expanded) state = noEffects $
    set (dashboardViewState <<< dbViewTxsExpanded) expanded state

update (DashboardPaginateBlocks newPage) state =
    { state:
          set (dashboardViewState <<< dbViewNextBlockPagination) newPage state
    , effects:
          [ pure RequestTotalBlocksToPaginateBlocks
            -- ^ get number of total blocks first before we do a request to get data of blocks
          ]
          <> (  if (syncBySocket $ state ^. syncAction)
                then  [ pure <<< SocketRemoveSubscription $ SocketSubscription SubBlock ]
                -- ^ TODO: Use new event type (`CSE-120`) to remove subscription
                else [])
    }
    where
        currentPage = state ^. (dashboardViewState <<< dbViewBlockPagination)
        offset = (currentPage - minPagination) * maxBlockRows

update (DashboardEditBlocksPageNumber target editable) state =
    { state:
          set (dashboardViewState <<< dbViewBlockPaginationEditable) editable state
    , effects:
          [ if editable
            then pure $ SelectInputText $ targetToHTMLInputElement target
            else pure NoOp
          ]
    }

update (DashboardInvalidBlocksPageNumber target) state =
    { state:
          set (dashboardViewState <<< dbViewBlockPaginationEditable) false state
    , effects:
          [ pure $ BlurElement $ targetToHTMLElement target
          ]
    }

update (DashboardShowAPICode code) state = noEffects $
    set (dashboardViewState <<< dbViewSelectedApiCode) code state

-- Address

update (AddressPaginateTxs value) state = noEffects $
    set (viewStates <<< addressDetail <<< addressTxPagination) value state

update (AddressEditTxsPageNumber target editable) state =
    { state:
          set (viewStates <<< addressDetail <<< addressTxPaginationEditable) editable state
    , effects:
          [ if editable
            then pure $ SelectInputText $ targetToHTMLInputElement target
            else pure NoOp
          ]
    }

update (AddressInvalidTxsPageNumber target) state =
    { state:
          set (viewStates <<< addressDetail <<< addressTxPaginationEditable) false state
      , effects:
          [ pure $ BlurElement $ targetToHTMLElement target
          ]
    }

-- Block

update (BlockPaginateTxs value) state = noEffects $
    set (viewStates <<< blockDetail <<< blockTxPagination) value state

update (BlockEditTxsPageNumber target editable) state =
    { state:
          set (viewStates <<< blockDetail <<< blockTxPaginationEditable) editable state
    , effects:
          [ if editable
            then pure $ SelectInputText $ targetToHTMLInputElement target
            else pure NoOp
          ]
    }

update (BlockInvalidTxsPageNumber target) state =
    { state:
          set (viewStates <<< blockDetail <<< blockTxPaginationEditable) false state
      , effects:
          [ pure $ BlurElement $ targetToHTMLElement target
          ]
    }

-- Blocks

update (BlocksPaginateBlocks value) state = noEffects $
    set (viewStates <<< blocksViewState <<< blsViewPagination) value state

update (BlocksEditBlocksPageNumber target editable) state =
    { state:
          set (viewStates <<< blocksViewState <<< blsViewPaginationEditable) editable state
    , effects:
          [ if editable
            then pure $ SelectInputText $ targetToHTMLInputElement target
            else pure NoOp
          ]
    }

update (BlocksInvalidBlocksPageNumber target) state =
    { state:
          set (viewStates <<< blocksViewState <<< blsViewPaginationEditable) false state
      , effects:
          [ pure $ BlurElement $ targetToHTMLElement target
          ]
    }

-- DOM side effects

update ScrollTop state =
    { state
    , effects:
        case state ^. syncAction of
            -- Don't scroll if we are doing polling
            -- TODO (jk) Remove this workaround if socket-io will be fixed
            SyncByPolling -> [ pure NoOp ]
            SyncBySocket -> [ liftEff scrollTop >>= \_ -> pure NoOp ]
    }
update (SelectInputText input) state =
    { state
    , effects:
        [ liftEff $ select input >>= \_ -> pure NoOp
        ]
    }
update (BlurElement elem) state =
    { state
    , effects:
        [ liftEff $ blur elem >>= \_ -> pure NoOp
        ]
    }
update (GenerateQrCode address) state =
    { state
    , effects:
        [ liftEff $ generateQrCode (address ^. _CAddress) addressQRImageId *> pure NoOp
        ]
    }

-- global state
update (GlobalToggleMobileMenu toggled) state = noEffects $
    set (viewStates <<< globalViewState <<< gViewMobileMenuOpenend) toggled state

update (GlobalFocusSearchInput value) state = noEffects $
    set (viewStates <<< globalViewState <<< gViewSearchInputFocused) value state

update GlobalSearch state =
    let query = state ^. (viewStates <<< globalViewState <<< gViewSearchQuery) in
    { state:
          set (viewStates <<< globalViewState <<< gViewSearchQuery) emptySearchQuery $
          set (viewStates <<< globalViewState <<< gViewMobileMenuOpenend) false $
          state
    , effects: [
      -- set state of focus explicitly
      pure $ GlobalFocusSearchInput false
      , case state ^. (viewStates <<< globalViewState <<< gViewSelectedSearch) of
          SearchAddress ->
              (liftEff <<< P.navigateTo <<< toUrl <<< Address $ mkCAddress query) *> pure NoOp
          SearchTx ->
              (liftEff <<< P.navigateTo <<< toUrl <<< Tx $ mkCTxId query) *> pure NoOp
          _ -> pure NoOp  -- TODO (ks) maybe put up a message?
      ]
    }
update GlobalSearchTime state =
    let query = state ^. (viewStates <<< globalViewState <<< gViewSearchTimeQuery)
    in
    { state:
          set (viewStates <<< globalViewState <<< gViewSearchTimeQuery) emptySearchTimeQuery $
          set (viewStates <<< globalViewState <<< gViewMobileMenuOpenend) false $
          state
    , effects: [
      -- set state of focus explicitly
      pure $ GlobalFocusSearchInput false
      , case query of
            Tuple (Just epoch) (Just slot) ->
                let epochIndex = mkEpochIndex epoch
                    slotIndex  = mkLocalSlotIndex slot
                    epochSlotUrl = EpochSlot epochIndex slotIndex
                in
                (liftEff <<< P.navigateTo <<< toUrl $ epochSlotUrl) *> pure NoOp
            Tuple (Just epoch) Nothing ->
                let epochIndex = mkEpochIndex epoch
                    epochUrl   = Epoch $ epochIndex
                in
                (liftEff <<< P.navigateTo <<< toUrl $ epochUrl) *> pure NoOp

            _ -> pure NoOp -- TODO (ks) maybe put up a message?
      ]
    }

update (GlobalUpdateSelectedSearch search) state =
    noEffects $ set (viewStates <<< globalViewState <<< gViewSelectedSearch) search state

update (GlobalUpdateSearchValue search) state =
    noEffects $ set (viewStates <<< globalViewState <<< gViewSearchQuery) search state

update (GlobalUpdateSearchEpochValue value) state =
    let slot = snd $ state ^. (viewStates <<< globalViewState <<< gViewSearchTimeQuery)
        epoch = fromString value
    in
    noEffects $ set (viewStates <<< globalViewState <<< gViewSearchTimeQuery) (Tuple epoch slot) state

update (GlobalUpdateSearchSlotValue value) state =
    let slot = fromString value
        epoch = fst $ state ^. (viewStates <<< globalViewState <<< gViewSearchTimeQuery)
    in
    noEffects $ set (viewStates <<< globalViewState <<< gViewSearchTimeQuery) (Tuple epoch slot) state

-- NoOp

update NoOp state = noEffects state

-- http endpoints

update RequestTotalBlocksToPaginateBlocks state =
    { state:
          set loading true $
          set (dashboardViewState <<< dbViewLoadingTotalBlocks) true
          state
    , effects:
          [ attempt fetchTotalBlocks >>= pure <<< ReceiveTotalBlocksToPaginateBlocks
          ]
    }

update (ReceiveTotalBlocksToPaginateBlocks (Right total)) state =
    { state:
          set totalBlocks (Success total) $
          set (dashboardViewState <<< dbViewLoadingTotalBlocks) false
          state
    , effects:
          [ pure $ RequestPaginatedBlocks (RequestLimit maxBlockRows) (RequestOffset offset) ]
    }
    where
        newPage = state ^. (dashboardViewState <<< dbViewNextBlockPagination)
        offset = (newPage - minPagination) * maxBlockRows

update (ReceiveTotalBlocksToPaginateBlocks (Left error)) state =
    noEffects $
    set loading false $
    set totalBlocks (Failure error) $
    set (dashboardViewState <<< dbViewLoadingTotalBlocks) false $
    set (dashboardViewState <<< dbViewLoadingBlockPagination) true
    state

update (RequestPaginatedBlocks limit offset) state =
    { state:
          set loading true $
          -- Important note: Don't use `set latestBlocks Loading` here,
          -- we will empty `latestBlocks` in this case !!!
          -- set latestBlocks Loading
          set (dashboardViewState <<< dbViewLoadingBlockPagination) true
          state
    , effects: [ attempt (fetchLatestBlocks limit offset) >>= pure <<< ReceivePaginatedBlocks ]
    }

update (ReceivePaginatedBlocks (Right blocks)) state =
    { state:
          set loading false $
          set (dashboardViewState <<< dbViewBlockPagination) newPage $
          set (dashboardViewState <<< dbViewLoadingBlockPagination) false $
          set latestBlocks (Success $ sortBlocksByEpochSlot' blocks) state
    , effects:
        if (syncBySocket $ state ^. syncAction)
        then [ pure <<< SocketAddSubscription $ SocketSubscription SubBlock ]
               -- ^ TODO: Use new event type (`CSE-120`) to add subscription
        else []
    }
    where
        newPage = state ^. (dashboardViewState <<< dbViewNextBlockPagination)
        offset = (newPage - minPagination) * maxBlockRows

update (ReceivePaginatedBlocks (Left error)) state =
    noEffects $
    set loading false <<<
    set (dashboardViewState <<< dbViewLoadingBlockPagination) false <<<
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

update (RequestSearchBlocks epoch slot) state =
    { state:
          set loading true $
          set currentBlocksResult Loading
          state
    , effects: [ attempt (searchEpoch epoch slot) >>= pure <<< ReceiveSearchBlocks ]
    }
update (ReceiveSearchBlocks (Right blocks)) state =
    noEffects $
    set loading false $
    set currentBlocksResult (Success blocks) state

update (ReceiveSearchBlocks (Left error)) state =
    noEffects $
    set loading false <<<
    set currentBlocksResult (Failure error) $
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

update (RequestLastTxs) state =
    { state:
          set loading true <<<
          set latestTransactions Loading $
          state
    , effects:
        [ attempt (fetchLatestTxs (RequestLimit maxTransactionRows) (RequestOffset 0)) >>=
              pure <<< ReceiveLastTxs
        ]
    }

update (ReceiveLastTxs (Right txs)) state =
    { state:
          set loading false $
          over latestTransactions
              (\currentTxs -> Success <<<
                                  sortTxsByTime' <<<
                                  take maxTransactionRows $
                                      unionTxs txs $
                                          withDefault [] currentTxs
              )
          state
    , effects:
        if (syncBySocket $ state ^. syncAction)
        then [ pure <<< SocketAddSubscription $ SocketSubscription SubTx ]
        else []
    }

update (ReceiveLastTxs (Left error)) state = noEffects $
    set loading false $
    over errors (\errors' -> (show error) : errors') state

update (RequestTxSummary id) state =
    { state:
          set loading true $
          set currentTxSummary Loading
          state
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
    { state:
          set loading true $
          set currentAddressSummary Loading
          state
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

-- clock
update UpdateClock state = onlyEffects state $
    [ do
         SetClock <<< extract <$> liftEff nowDateTime
    ]
update (SetClock date) state = noEffects $ state { now = date }

--- Reload pages
-- TODO (jk) Remove it if socket-io is back
update Reload state = update (UpdateView state.route) state

-- routing

update (UpdateView route) state = routeEffects route (state { route = route })

routeEffects :: forall eff. Route -> State -> EffModel State Action (dom :: DOM
    , ajax :: AJAX | eff)
routeEffects Dashboard state =
    { state:
        set (viewStates <<< globalViewState <<< gViewTitle)
            (translate (I18nL.common <<< I18nL.cTitle) $ state ^. lang)
            state
    , effects:
        [ pure ScrollTop
        , pure $ DashboardPaginateBlocks $ state ^. (dashboardViewState <<< dbViewBlockPagination)
        , pure RequestLastTxs
        ]
    }

routeEffects (Tx tx) state =
    { state:
        set (viewStates <<< globalViewState <<< gViewTitle)
            (translate (I18nL.common <<< I18nL.cTransaction) $ state ^. lang)
            state
    , effects:
        [ pure ScrollTop
        , pure $ SocketUpdateSubscriptions []
        , pure $ RequestTxSummary tx
        ]
    }

routeEffects (Address cAddress) state =
    { state:
        set currentCAddress cAddress $
        set (viewStates <<< addressDetail <<< addressTxPagination) minPagination $
        set (viewStates <<< globalViewState <<< gViewTitle)
            (translate (I18nL.common <<< I18nL.cAddress) $ state ^. lang)
            state
    , effects:
        [ pure ScrollTop
        , pure $ SocketUpdateSubscriptions []
        , pure $ RequestAddressSummary cAddress
        ]
    }

routeEffects (Epoch epochIndex) state =
    { state:
        set (viewStates <<< blocksViewState <<< blsViewPagination)
            minPagination $
        set (viewStates <<< globalViewState <<< gViewTitle)
            (translate (I18nL.common <<< I18nL.cEpoch) $ state ^. lang)
            state
    , effects:
        [ pure ScrollTop
        , pure $ RequestSearchBlocks epochIndex Nothing
        ]
    }

routeEffects (EpochSlot epochIndex slotIndex) state =
    let lang' = state ^. lang
        epochTitle = translate (I18nL.common <<< I18nL.cEpoch) lang'
        slotTitle = translate (I18nL.common <<< I18nL.cSlot) lang'
    in
    { state:
        set (viewStates <<< globalViewState <<< gViewTitle)
            (epochTitle <> " / " <> slotTitle)
            state
    , effects:
        [ pure ScrollTop
        , pure $ RequestSearchBlocks epochIndex (Just slotIndex)
        ]
    }


routeEffects Calculator state =
    { state:
        set (viewStates <<< globalViewState <<< gViewTitle)
            (translate (I18nL.common <<< I18nL.cCalculator) $ state ^. lang)
            state
    , effects: [ pure ScrollTop ]
    }

routeEffects (Block hash) state =
    { state:
        set currentBlockSummary Nothing $
        set (viewStates <<< globalViewState <<< gViewTitle)
            (translate (I18nL.common <<< I18nL.cBlock) $ state ^. lang)
            state
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
    { state:
        set (viewStates <<< globalViewState <<< gViewTitle)
            (translate (I18nL.notfound <<< I18nL.nfTitle) $ state ^. lang)
            state
    , effects:
        [ pure ScrollTop
        , pure $ SocketUpdateSubscriptions []
        ]
    }

socketSubscribeEvent :: forall eff . Socket -> SocketSubscription
    -> Eff (socket :: SocketIO | eff) Unit
socketSubscribeEvent socket (SocketSubscription event)  =
    emit' socket <<< toEvent $ Subscribe event

socketUnsubscribeEvent :: forall eff . Socket -> SocketSubscription
    -> Eff (socket :: SocketIO | eff) Unit
socketUnsubscribeEvent socket (SocketSubscription event)  =
    emit' socket <<< toEvent $ Unsubscribe event
