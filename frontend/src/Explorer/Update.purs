module Explorer.Update where

import Prelude

import Control.Comonad (extract)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (nowDateTime)
import Control.SocketIO.Client (Socket, SocketIO, emit, emitData)
import DOM (DOM)
import DOM.Event.Event (target, preventDefault)
import DOM.HTML (window)
import DOM.HTML.HTMLElement (blur, focus)
import DOM.HTML.HTMLInputElement (select)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Window (history)
import DOM.Node.Node (contains)
import DOM.Node.Types (ElementId(..), elementToNode)
import Data.Array (filter, length, snoc, take, (:))
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Foreign (toForeign)
import Data.Int (fromString)
import Data.Lens ((^.), over, set)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Explorer.Api.Http (fetchAddressSummary, fetchBlockSummary, fetchBlockTxs, fetchBlocksTotalPages, fetchLatestTxs, fetchPageBlocks, fetchTxSummary, searchEpoch)
import Explorer.Api.Socket (toEvent)
import Explorer.Api.Types (RequestLimit(..), RequestOffset(..), SocketOffset(..), SocketSubscription(..), SocketSubscriptionData(..))
import Explorer.Lenses.State (addressDetail, addressTxPagination, addressTxPaginationEditable, blockDetail, blockTxPagination, blockTxPaginationEditable, blocksViewState, blsViewPagination, blsViewPaginationEditable, connected, connection, currentAddressSummary, currentBlockSummary, currentBlockTxs, currentBlocksResult, currentCAddress, currentTxSummary, dbViewBlockPagination, dbViewBlockPaginationEditable, dbViewBlocksExpanded, dbViewLoadingBlockPagination, dbViewMaxBlockPagination, dbViewSelectedApiCode, dbViewTxsExpanded, errors, gViewMobileMenuOpenend, gViewSearchInputFocused, gViewSearchQuery, gViewSearchTimeQuery, gViewSelectedSearch, gWaypoints, globalViewState, lang, latestBlocks, latestTransactions, loading, route, socket, subscriptions, syncAction, viewStates)
import Explorer.Routes (Route(..), match, toUrl)
import Explorer.State (addressQRImageId, emptySearchQuery, emptySearchTimeQuery, headerSearchContainerId, heroSearchContainerId, minPagination, mkSocketSubscriptionItem, mobileMenuSearchContainerId)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.App (AppEffects)
import Explorer.Types.State (PageNumber(..), PageSize(..), Search(..), SocketSubscriptionItem(..), State, WaypointItem(..))
import Explorer.Util.Config (SyncAction(..), syncBySocket)
import Explorer.Util.DOM (addClassToElement, findElementById, removeClassFromElement, scrollTop, nodeToHTMLElement, nodeToHTMLInputElement)
import Explorer.Util.Data (sortTxsByTime', unionTxs)
import Explorer.Util.Factory (mkCAddress, mkCTxId, mkEpochIndex, mkLocalSlotIndex)
import Explorer.Util.QrCode (generateQrCode)
import Explorer.View.Blocks (maxBlockRows)
import Explorer.View.CSS (dashBoardBlocksViewId, headerId, moveIn, moveOut) as CSS
import Explorer.View.Dashboard.Lenses (dashboardViewState)
import Explorer.View.Dashboard.Transactions (maxTransactionRows)
import Network.RemoteData (RemoteData(..), _Success, isNotAsked, isSuccess, withDefault)
import Pos.Explorer.Socket.Methods (ClientEvent(..), Subscription(..))
import Pos.Explorer.Web.ClientTypes (CAddress(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, _CAddressSummary, caAddress, caTxList)
import Pux (EffModel, noEffects, onlyEffects)
import Waypoints (WAYPOINT, destroy, waypoint', up) as WP

update :: forall eff. Action -> State -> EffModel State Action (AppEffects eff)

-- Language

update (SetLanguage lang) state = noEffects $ state { lang = lang }


-- Socket

update (SocketConnected connected') state =
    { state: set (socket <<< connected) connected' state
    , effects:
          [ if connected'
            then pure $ Just SocketReconnectSubscriptions
            else pure Nothing
          ]
    }

update SocketPing state =
    { state
    , effects : [ do
          _ <- case state ^. (socket <<< connection) of
              Just socket' -> liftEff <<< emit socket' $ toEvent CallMe
              Nothing -> pure unit
          pure Nothing
    ]}

update (SocketBlocksPageUpdated (Right (Tuple totalPages blocks))) state =
    noEffects $
    over latestBlocks (\bl -> if updateBlocks then Success blocks else bl) $
    over (dashboardViewState <<< dbViewBlockPagination)
        (\ pn@(PageNumber page) -> if updateCurrentPage then PageNumber totalPages else pn) $
        -- ^ to keep staying at last page we have to update `dbViewBlockPagination`
    set (dashboardViewState <<< dbViewMaxBlockPagination) (Success $ PageNumber totalPages) state
  where
    latestBlocksCheck = if updateBlocks
                        then (Success blocks)
                        else (state ^. latestBlocks)
    currentBlockPage = state ^. (dashboardViewState <<< dbViewBlockPagination)
    updateBlocks = (currentBlockPage == PageNumber totalPages) || updateCurrentPage
    updateCurrentPage = currentBlockPage == (PageNumber $ totalPages - 1) && (length blocks == 1)


update (SocketBlocksPageUpdated (Left error)) state = noEffects $
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

update (SocketAddressTxsUpdated (Right txs)) state =
    noEffects $
    if isSuccess addrSummary
    then do
        -- Add latest tx on top to other txs of an address
        -- Note: We have to "over" `addrSummary` and set
        -- `currentAddressSummary` explicitly to update the view
        let addrSummary' = over (_Success <<< _CAddressSummary <<< caTxList)
                              (\txs' -> txs <> txs') addrSummary
        set currentAddressSummary addrSummary' state
    else state
    where
        addrSummary = state ^. currentAddressSummary

update (SocketAddressTxsUpdated (Left error)) state = noEffects $
    over errors (\errors' -> (show error) : errors') state

-- | Creates a new socket subscription
update (SocketAddSubscription subItem) state =
    { state:
          over (socket <<< subscriptions) (\subs -> snoc subs subItem) state
    , effects : [ do
          _ <- case state ^. (socket <<< connection) of
              Just socket' -> liftEff $ socketSubscribeEvent socket' subItem
              Nothing -> pure unit
          pure Nothing
    ]}

-- | Removes an existing socket subscription
update (SocketRemoveSubscription subItem) state =
    { state:
          over (socket <<< subscriptions) (filter ((/=) subItem)) state
    , effects : [ do
          _ <- case state ^. (socket <<< connection) of
              Just socket' -> liftEff $ socketUnsubscribeEvent socket' subItem
              Nothing -> pure unit
          pure Nothing
    ]}


-- | Removes all existing socket subscriptions
update (SocketClearSubscriptions) state =
    { state:
          set (socket <<< subscriptions) [] state
    , effects : [ do
          _ <- case state ^. (socket <<< connection) of
              Just socket' -> do
                  traverse_ (liftEff <<< socketUnsubscribeEvent socket')
                      (state ^. socket <<< subscriptions)
              Nothing -> pure unit
          pure Nothing
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
          pure Nothing
    ]}

-- Dashboard

update (DashboardExpandBlocks expanded) state = noEffects $
    set (dashboardViewState <<< dbViewBlocksExpanded) expanded state

update (DashboardExpandTransactions expanded) state = noEffects $
    set (dashboardViewState <<< dbViewTxsExpanded) expanded state

update (DashboardPaginateBlocks mEvent pageNumber) state =
    { state:
          set (dashboardViewState <<< dbViewBlockPaginationEditable) false state
    , effects:
          [ pure $ maybe Nothing (Just <<< BlurElement <<< nodeToHTMLElement <<< target) mEvent
          -- ^ blur element - needed by iOS to close native keyboard
          , pure <<< Just $ RequestPaginatedBlocks pageNumber (PageSize maxBlockRows)
          ]
    }

update (DashboardEditBlocksPageNumber event editable) state =
    { state:
          set (dashboardViewState <<< dbViewBlockPaginationEditable) editable state
    , effects:
          [ if editable
            then pure <<< Just $ SelectInputText $ nodeToHTMLInputElement (target event)
            else pure Nothing
          ]
    }

update (DashboardInvalidBlocksPageNumber event) state =
    { state:
          set (dashboardViewState <<< dbViewBlockPaginationEditable) false state
    , effects:
          [ pure <<< Just $ BlurElement $ nodeToHTMLElement (target event)
          ]
    }

update (DashboardShowAPICode code) state = noEffects $
    set (dashboardViewState <<< dbViewSelectedApiCode) code state

-- Address

update (AddressPaginateTxs mEvent pageNumber) state =
    { state:
          set (viewStates <<< addressDetail <<< addressTxPagination) pageNumber $
          set (viewStates <<< addressDetail <<< addressTxPaginationEditable) false state
    , effects:
        [ pure $ maybe Nothing (Just <<< BlurElement <<< nodeToHTMLElement <<< target) mEvent
        -- ^ blur element - needed by iOS to close native keyboard
        ]
    }

update (AddressEditTxsPageNumber event editable) state =
    { state:
          set (viewStates <<< addressDetail <<< addressTxPaginationEditable) editable state
    , effects:
          [ if editable
            then pure <<< Just $ SelectInputText $ nodeToHTMLInputElement (target event)
            else pure Nothing
          ]
    }

update (AddressInvalidTxsPageNumber event) state =
    { state:
          set (viewStates <<< addressDetail <<< addressTxPaginationEditable) false state
      , effects:
          [ pure <<< Just $ BlurElement $ nodeToHTMLElement (target event)
          ]
    }

-- Block

update (BlockPaginateTxs mEvent pageNumber) state =
    { state:
          set (viewStates <<< blockDetail <<< blockTxPagination) pageNumber $
          set (viewStates <<< blockDetail <<< blockTxPaginationEditable) false state
    , effects:
        [ pure $ maybe Nothing (Just <<< BlurElement <<< nodeToHTMLElement <<< target) mEvent
        -- ^ blur element - needed by iOS to close native keyboard
        ]
    }

update (BlockEditTxsPageNumber event editable) state =
    { state:
          set (viewStates <<< blockDetail <<< blockTxPaginationEditable) editable state
    , effects:
          [ if editable
            then pure <<< Just $ SelectInputText $ nodeToHTMLInputElement (target event)
            else pure Nothing
          ]
    }

update (BlockInvalidTxsPageNumber event) state =
    { state:
          set (viewStates <<< blockDetail <<< blockTxPaginationEditable) false state
      , effects:
          [ pure <<< Just $ BlurElement $ nodeToHTMLElement (target event)
          ]
    }

-- Blocks

update (BlocksPaginateBlocks mEvent pageNumber) state =
    { state:
          set (viewStates <<< blocksViewState <<< blsViewPagination) pageNumber $
          set (viewStates <<< blocksViewState <<< blsViewPaginationEditable) false state
    , effects:
        [ pure $ maybe Nothing (Just <<< BlurElement <<< nodeToHTMLElement <<< target) mEvent
        -- ^ blur element - needed by iOS to close native keyboard
        ]
    }

update (BlocksEditBlocksPageNumber event editable) state =
    { state:
          set (viewStates <<< blocksViewState <<< blsViewPaginationEditable) editable state
    , effects:
          [ if editable
            then pure <<< Just $ SelectInputText $ nodeToHTMLInputElement (target event)
            else pure Nothing
          ]
    }

update (BlocksInvalidBlocksPageNumber event) state =
    { state:
          set (viewStates <<< blocksViewState <<< blsViewPaginationEditable) false state
      , effects:
          [ pure <<< Just $ BlurElement $ nodeToHTMLElement (target event)
          ]
    }

-- DOM side effects

update ScrollTop state =
    { state
    , effects:
        case state ^. syncAction of
            -- Don't scroll if we are doing polling
            -- TODO (jk) Remove this workaround if socket-io will be fixed
            SyncByPolling -> [ pure Nothing ]
            SyncBySocket -> [ liftEff scrollTop >>= \_ -> pure Nothing ]
    }

update (SelectInputText input) state =
    { state
    , effects:
        [ liftEff $ select input >>= \_ -> pure Nothing
        ]
    }

update (BlurElement elem) state =
    { state
    , effects:
        [ liftEff $ blur elem >>= \_ -> pure Nothing
        ]
    }

update (FocusElement elem) state =
    { state
    , effects:
        [ liftEff $ focus elem >>= \_ -> pure Nothing
        ]
    }

update (GenerateQrCode address) state =
    { state
    , effects:
        [ liftEff $ generateQrCode (address ^. _CAddress) addressQRImageId *> pure Nothing
        ]
    }

update (DashboardAddWaypoint elementId) state =
    { state
    , effects:
        [ liftEff waypoint >>= \wp -> pure <<< Just <<< StoreWaypoint $ WaypointItem { wpInstance: wp, wpRoute: Dashboard }
        ]
    }
    where
        elId = ElementId CSS.headerId
        callback = \(direction) ->
            if direction == WP.up
                then do
                    addClassToElement elId CSS.moveOut
                    removeClassFromElement elId CSS.moveIn
                else do
                    addClassToElement elId CSS.moveIn
                    removeClassFromElement elId CSS.moveOut

        waypoint = WP.waypoint' elementId callback 71 -- 71 == height of header

update (StoreWaypoint wp) state = noEffects $
    over (viewStates <<< globalViewState <<< gWaypoints) ((:) wp) state

update ClearWaypoints state =
    { state: set (viewStates <<< globalViewState <<< gWaypoints) [] state
    , effects:
          [ do
                traverse_ (liftEff <<< disposeWaypoint) waypointItems
                pure Nothing
          ]
    }
    where
      waypointItems = state ^. (viewStates <<< globalViewState <<< gWaypoints)

      -- | Disposes any `Waypoint` stored in state
      -- | Use it to reverse any changes which might be added by a Waypoint before,
      -- | e.g. adding of new CSS classes or something else
      disposeWaypoint :: forall e. WaypointItem -> Eff (dom :: DOM, waypoint :: WP.WAYPOINT | e) Unit
      disposeWaypoint (WaypointItem item) = do
          _ <- case _.wpRoute item of
                    Dashboard -> do
                      -- remove all css classes which might be added
                      -- by waypoint's callback
                      let elId = ElementId CSS.headerId
                      _ <- removeClassFromElement elId CSS.moveOut
                      _ <- removeClassFromElement elId CSS.moveIn
                      pure unit
                    -- Add any other effects for any other route if needed here
                    _ -> pure unit
          _ <- WP.destroy $ _.wpInstance item
          pure unit

update (DocumentClicked event) state =
    { state
    , effects:
        [ liftEff $
            -- First check here is to see if a search container has been clicked or not.
            -- We do ignore this check if the mobile menue is openend,
            -- because we don't need to do any effects there
            if (not (state ^. (viewStates <<< globalViewState <<< gViewMobileMenuOpenend)))
            then
                do
                -- Check if any children of one of our three search containers has been clicked or not
                  heroSearchContainerClicked <- findElementById heroSearchContainerId >>= elementClicked
                  mobileMenuSearchContainerClicked <- findElementById mobileMenuSearchContainerId >>= elementClicked
                  headerSearchContainerClicked <- findElementById headerSearchContainerId >>= elementClicked
                  -- If any of these children are clicked we know, that the search UI has been set to active (focused)
                  let clicked = heroSearchContainerClicked || mobileMenuSearchContainerClicked || headerSearchContainerClicked
                  pure <<< Just $ GlobalFocusSearchInput clicked
            else
                pure Nothing
        ]
    }
    where
        elementClicked mEl =
            case mEl of
                Just el ->
                    contains (elementToNode el) (target event)
                Nothing ->
                    pure false

-- global state
update (GlobalToggleMobileMenu toggled) state = noEffects $
    set (viewStates <<< globalViewState <<< gViewMobileMenuOpenend) toggled state

update (GlobalFocusSearchInput value) state = noEffects $
    set (viewStates <<< globalViewState <<< gViewSearchInputFocused) value $
    over (viewStates <<< globalViewState <<< gViewSelectedSearch)
        (\selectedSearch ->
              -- return to `SearchAddress` in inactive mode, but not in mobile menu
              if value == false &&
                    (not $ state ^. (viewStates <<< globalViewState <<< gViewMobileMenuOpenend))
              then SearchAddress
              else selectedSearch
        )
    state

update (GlobalSearch event) state =
    let query = state ^. (viewStates <<< globalViewState <<< gViewSearchQuery) in
    { state:
          set (viewStates <<< globalViewState <<< gViewSearchQuery) emptySearchQuery $
          set (viewStates <<< globalViewState <<< gViewMobileMenuOpenend) false $
          state
    , effects:
        [ pure <<< Just $ GlobalFocusSearchInput false
        -- ^ set state of focus explicitly
        , pure <<< Just <<< BlurElement <<< nodeToHTMLElement $ target event
        -- ^ blur element - needed by iOS to close native keyboard
        , case state ^. (viewStates <<< globalViewState <<< gViewSelectedSearch) of
            SearchAddress ->
                pure <<< Just $ Navigate (toUrl <<< Address $ mkCAddress query) event
            SearchTx ->
                pure <<< Just $ Navigate (toUrl <<< Tx $ mkCTxId query) event
            _ -> pure Nothing  -- TODO (ks) maybe put up a message?
        ]
    }
update (GlobalSearchTime event) state =
    let query = state ^. (viewStates <<< globalViewState <<< gViewSearchTimeQuery)
    in
    { state:
          set (viewStates <<< globalViewState <<< gViewSearchTimeQuery) emptySearchTimeQuery $
          set (viewStates <<< globalViewState <<< gViewMobileMenuOpenend) false $
          state
    , effects:
        [ pure <<< Just $ GlobalFocusSearchInput false
          -- ^ set state of focus explicitly
          , pure <<< Just <<< BlurElement <<< nodeToHTMLElement $ target event
          -- ^ blur element - needed by iOS to close native keyboard
          , case query of
                Tuple (Just epoch) (Just slot) ->
                    let epochIndex = mkEpochIndex epoch
                        slotIndex  = mkLocalSlotIndex slot
                        epochSlotUrl = EpochSlot epochIndex slotIndex
                    in
                    pure <<< Just $ Navigate (toUrl epochSlotUrl) event
                Tuple (Just epoch) Nothing ->
                    let epochIndex = mkEpochIndex epoch
                        epochUrl   = Epoch $ epochIndex
                    in
                    pure <<< Just $ Navigate (toUrl epochUrl) event

                _ -> pure Nothing -- TODO (ks) maybe put up a message?
        ]
    }

update (GlobalUpdateSelectedSearch search) state =
    noEffects $
        set (viewStates <<< globalViewState <<< gViewSelectedSearch) search
        state

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

update DashboardRequestBlocksTotalPages state =
    { state:
          set loading true $
          set (dashboardViewState <<< dbViewMaxBlockPagination) Loading
          state
    , effects: [ attempt fetchBlocksTotalPages >>= pure <<< Just <<< DashboardReceiveBlocksTotalPages ]
    }

update (DashboardReceiveBlocksTotalPages (Right totalPages)) state =
    { state:
          set loading false $
          set (dashboardViewState <<< dbViewMaxBlockPagination)
              (Success $ PageNumber totalPages) $
          set (dashboardViewState <<< dbViewBlockPagination)
              (PageNumber totalPages) state
    , effects:
        [ pure <<< Just $ DashboardPaginateBlocks Nothing (PageNumber totalPages) ]
    }

update (DashboardReceiveBlocksTotalPages (Left error)) state =
    noEffects $
    set loading false $
    set (dashboardViewState <<< dbViewMaxBlockPagination) (Failure error) $
    over errors (\errors' -> (show error) : errors') state

update (RequestPaginatedBlocks pageNumber pageSize) state =
    { state:
          set loading true $
          set (dashboardViewState <<< dbViewBlockPagination) pageNumber $
          -- Important note: Don't use `set latestBlocks Loading` here,
          -- we will empty `latestBlocks` in this case !!!
          -- set latestBlocks Loading
          set (dashboardViewState <<< dbViewLoadingBlockPagination) true
          state
    , effects: [ attempt (fetchPageBlocks pageNumber pageSize) >>= pure <<< Just <<< ReceivePaginatedBlocks ]
    }

update (ReceivePaginatedBlocks (Right (Tuple totalPages blocks))) state =
    { state:
          set loading false $
          set (dashboardViewState <<< dbViewMaxBlockPagination) (Success $ PageNumber totalPages) $
          set (dashboardViewState <<< dbViewLoadingBlockPagination) false $
          set latestBlocks (Success blocks) state
    , effects:
        if (syncBySocket $ state ^. syncAction)
        then [ pure <<< Just $ SocketAddSubscription subItem ]
        else []
    }
    where
        subItem = mkSocketSubscriptionItem (SocketSubscription SubBlockLastPage) SocketNoData

update (ReceivePaginatedBlocks (Left error)) state =
    noEffects $
    set loading false <<<
    set (dashboardViewState <<< dbViewLoadingBlockPagination) false <<<
    set latestBlocks (Failure error) $
    over errors (\errors' -> (show error) : errors') state

update (RequestBlockSummary hash) state =
    { state:
          set loading true $
          set currentBlockSummary Loading
          state
    , effects: [ attempt (fetchBlockSummary hash) >>= pure <<< Just <<< ReceiveBlockSummary ]
    }
update (ReceiveBlockSummary (Right blockSummary)) state =
    noEffects $
    set loading false $
    set currentBlockSummary (Success blockSummary) state
update (ReceiveBlockSummary (Left error)) state =
    noEffects $
    set loading false $
    set currentBlockSummary (Failure error) $
    over errors (\errors' -> (show error) : errors')
    state

-- Epoch, slot

update (RequestSearchBlocks epoch slot) state =
    { state:
          set loading true $
          set currentBlocksResult Loading
          state
    , effects: [ attempt (searchEpoch epoch slot) >>= pure <<< Just <<< ReceiveSearchBlocks ]
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
    { state:
          set loading true $
          set currentBlockTxs Loading
          state
    , effects: [ attempt (fetchBlockTxs hash) >>= pure <<< Just <<< ReceiveBlockTxs ]
    }
update (ReceiveBlockTxs (Right txs)) state =
    noEffects $
    set loading false <<<
    set currentBlockTxs (Success txs) $
    state
update (ReceiveBlockTxs (Left error)) state =
    noEffects $
    set loading false $
    set currentBlockTxs (Failure error) $
    over errors (\errors' -> (show error) : errors') state

update (RequestLastTxs) state =
    { state:
          set loading true <<<
          set latestTransactions Loading $
          state
    , effects:
        [ attempt (fetchLatestTxs (RequestLimit maxTransactionRows) (RequestOffset 0)) >>=
              pure <<< Just <<< ReceiveLastTxs
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
        then [ pure <<< Just $ SocketAddSubscription subItem ]
        else []
    }
    where
        subItem = mkSocketSubscriptionItem (SocketSubscription SubTx) SocketNoData

update (ReceiveLastTxs (Left error)) state = noEffects $
    set loading false $
    over errors (\errors' -> (show error) : errors') state

update (RequestTxSummary id) state =
    { state:
          set loading true $
          set currentTxSummary Loading
          state
    , effects: [ attempt (fetchTxSummary id) >>= pure <<< Just <<< ReceiveTxSummary ]
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
    , effects: [ attempt (fetchAddressSummary address) >>= pure <<< Just <<< ReceiveAddressSummary ]
    }
update (ReceiveAddressSummary (Right address)) state =
    { state:
        set loading false $
        set currentAddressSummary (Success address) state
    , effects:
        [ pure <<< Just $ GenerateQrCode caAddress'
        ]
        <>  ( if (syncBySocket $ state ^. syncAction)
              then [ pure <<< Just $ SocketAddSubscription subItem ]
              else []
            )
    }
    where
        caAddress' = address ^. (_CAddressSummary <<< caAddress)
        subItem = mkSocketSubscriptionItem (SocketSubscription SubAddr) (SocketCAddressData caAddress')



update (ReceiveAddressSummary (Left error)) state =
    noEffects $
    set loading false $
    set currentAddressSummary (Failure error) $
    over errors (\errors' -> (show error) : errors') state

-- clock
update UpdateClock state = onlyEffects state
    [ do
        Just <<< SetClock <<< extract <$> liftEff nowDateTime
    ]

update (SetClock date) state = noEffects $ state { now = date }

-- Reload pages
-- TODO (jk) Remove it if socket-io is back
update Reload state = update (UpdateView state.route) state

-- routing

update (Navigate url ev) state = onlyEffects state
    [ do
        liftEff do
            preventDefault ev
            h <- history =<< window
            -- TODO (jk) Set document title
            pushState (toForeign {}) (DocumentTitle "") (URL url) h
        pure <<< Just $ UpdateView (match url)
    ]

update (UpdateView r@Dashboard) state =
    { state:
        set (viewStates <<< globalViewState <<< gViewMobileMenuOpenend) false $
        set route r state
    , effects:
        [ pure $ Just ScrollTop
        , pure $ Just ClearWaypoints
        , pure <<< Just <<< DashboardAddWaypoint $ ElementId CSS.dashBoardBlocksViewId
        , if isSuccess maxBlockPage
          then pure <<< Just $ DashboardPaginateBlocks Nothing (state ^. (dashboardViewState <<< dbViewBlockPagination))
          else
              -- Note: Request `total pages `only once.
              -- Check is needed due reloading by http pooling
              if isNotAsked maxBlockPage
              then pure $ Just DashboardRequestBlocksTotalPages
              else pure Nothing
        , pure $ Just RequestLastTxs
        ]
    }
    where
        maxBlockPage = state ^. (dashboardViewState <<< dbViewMaxBlockPagination)

update (UpdateView r@(Tx tx)) state =
    { state:
        set route r state
    , effects:
        [ pure $ Just ScrollTop
        , pure $ Just ClearWaypoints
        , pure $ Just SocketClearSubscriptions
        , pure <<< Just $ RequestTxSummary tx
        ]
    }

update (UpdateView r@(Address cAddress)) state =
    { state:
        set currentCAddress cAddress $
        set (viewStates <<< addressDetail <<< addressTxPagination) (PageNumber minPagination) $
        set route r state
    , effects:
        [ pure $ Just ScrollTop
        , pure $ Just ClearWaypoints
        , pure $ Just SocketClearSubscriptions
        , pure <<< Just $ RequestAddressSummary cAddress
        ]
    }

update (UpdateView r@(Epoch epochIndex)) state =
    { state:
        set (viewStates <<< blocksViewState <<< blsViewPagination)
            (PageNumber minPagination) $
        set route r state
    , effects:
        [ pure $ Just ScrollTop
        , pure $ Just ClearWaypoints
        , pure <<< Just $ RequestSearchBlocks epochIndex Nothing
        ]
    }

update (UpdateView r@(EpochSlot epochIndex slotIndex)) state =
    let lang' = state ^. lang

    in
    { state:
        set route r state
    , effects:
        [ pure $ Just ScrollTop
        , pure $ Just ClearWaypoints
        , pure <<< Just $ RequestSearchBlocks epochIndex (Just slotIndex)
        ]
    }

update (UpdateView r@Calculator) state =
    { state:
        set route r state
    , effects:
        [ pure $ Just ScrollTop
        , pure $ Just ClearWaypoints
        ]
    }

update (UpdateView r@(Block hash)) state =
    { state:
        set route r state
    , effects:
        [ pure $ Just ScrollTop
        , pure $ Just ClearWaypoints
        , pure $ Just SocketClearSubscriptions
        , pure <<< Just $ RequestBlockSummary hash
        , pure <<< Just $ RequestBlockTxs hash
        ]
    }

update (UpdateView r@(Playground)) state =
    { state: set route r state
    , effects:
        [ pure $ Just ScrollTop
        , pure $ Just ClearWaypoints
        , pure $ Just SocketClearSubscriptions
        ]
    }

update (UpdateView r@(NotFound)) state =
    { state:
        set route r state
    , effects:
        [ pure $ Just ScrollTop
        , pure $ Just ClearWaypoints
        , pure $ Just SocketClearSubscriptions
        ]
    }

socketSubscribeEvent :: forall eff . Socket -> SocketSubscriptionItem
    -> Eff (socket :: SocketIO | eff) Unit
socketSubscribeEvent socket (SocketSubscriptionItem item) =
    subscribe socket event subData
    where
        event = toEvent <<< Subscribe <<< unwrap $ _.socketSub item
        subData = _.socketSubData item

        subscribe :: Socket -> String -> SocketSubscriptionData -> Eff (socket :: SocketIO | eff) Unit
        subscribe s e SocketNoData = emit s e
        subscribe s e (SocketOffsetData (SocketOffset o)) = emitData s e o
        subscribe s e (SocketCAddressData (CAddress addr)) = emitData s e addr

socketUnsubscribeEvent :: forall eff . Socket -> SocketSubscriptionItem
    -> Eff (socket :: SocketIO | eff) Unit
socketUnsubscribeEvent socket (SocketSubscriptionItem item)  =
    emit socket event
    where
        event = toEvent <<< Unsubscribe <<< unwrap $ _.socketSub item
