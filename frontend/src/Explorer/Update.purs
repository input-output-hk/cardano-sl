module Explorer.Update where

import Prelude
import Control.Comonad (extract)
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (nowDateTime, NOW)
import Control.SocketIO.Client (SocketIO, emit, emit')
import DOM (DOM)
import DOM.HTML.HTMLElement (blur)
import DOM.HTML.HTMLInputElement (select)
import Data.Array (difference, length, unionBy, (:))
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Int (fromString)
import Data.Lens ((^.), over, set)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Explorer.Api.Http (fetchAddressSummary, fetchBlockSummary, fetchBlockTxs, fetchLatestBlocks, fetchLatestTxs, fetchTotalBlocks, fetchTxSummary, searchEpoch)
import Explorer.Api.Socket (toEvent)
import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (common, cAddress, cBlock, cCalculator, cEpoch, cSlot, cTitle, cTransaction, notfound, nfTitle) as I18nL
import Explorer.Lenses.State (addressDetail, addressTxPagination, addressTxPaginationEditable, blockDetail, blockTxPagination, blockTxPaginationEditable, blocksViewState, blsViewPagination, blsViewPaginationEditable, connected, connection, currentAddressSummary, currentBlockSummary, currentBlockTxs, currentBlocksResult, currentCAddress, currentTxSummary, dbViewBlockPagination, dbViewBlockPaginationEditable, dbViewBlocksExpanded, dbViewSelectedApiCode, dbViewTxsExpanded, errors, gViewMobileMenuOpenend, gViewSearchInputFocused, gViewSearchQuery, gViewSearchTimeQuery, gViewSelectedSearch, gViewTitle, globalViewState, lang, latestBlocks, latestTransactions, loading, pullLatestBlocks, route, socket, subscriptions, totalBlocks, viewStates)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (addressQRImageId, emptySearchQuery, emptySearchTimeQuery, minPagination)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CBlockEntriesOffset, Search(..), SocketSubscription(..), State)
import Explorer.Util.DOM (targetToHTMLElement, targetToHTMLInputElement)
import Explorer.Util.Factory (mkCAddress, mkCTxId, mkEpochIndex, mkLocalSlotIndex)
import Explorer.Util.QrCode (generateQrCode)
import Explorer.Util.Sort (sortBlocksByTime)
import Explorer.View.Blocks (maxBlockRows)
import Explorer.View.Dashboard.Lenses (dashboardViewState)
import Network.HTTP.Affjax (AJAX)
import Network.RemoteData (RemoteData(..), _Success, isNotAsked, isSuccess, withDefault)
import Pos.Explorer.Socket.Methods (ClientEvent(..), Subscription(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, _CAddressSummary, _CBlockEntry, _CHash, _CTxEntry, _CTxId, caAddress, cbeBlkHash, cteId)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.Router (navigateTo) as P


update :: forall eff. Action -> State ->
    EffModel State Action
    (dom :: DOM
    , ajax :: AJAX
    , socket :: SocketIO
    , now :: NOW
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

update (SocketBlocksUpdated (Right blocks)) state =
    noEffects $
    if isSuccess $ state ^. latestBlocks
    -- add incoming blocks ahead of previous blocks
    then  set latestBlocks (Success newBlocks) $
          over (totalBlocks <<< _Success) ((+) numberNewBlocks) state
    else state
    where
        previousBlocks = withDefault [] $ state ^. latestBlocks
        newBlocks = sortBlocksByTime $ blocks <> previousBlocks
        numberNewBlocks = (length newBlocks) - (length previousBlocks)

update (SocketBlocksUpdated (Left error)) state = noEffects $
    set latestBlocks (Failure error) $
    -- add incoming errors ahead of previous errors
    over errors (\errors' -> (show error) : errors') state

update (SocketTxsUpdated (Right transactions)) state =
    noEffects $
    if isSuccess $ state ^. latestTransactions
    -- add incoming transactions ahead of previous transactions
    then over (latestTransactions <<< _Success) (\t -> transactions <> t) state
    else state

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

update (DashboardExpandBlocks expanded) state = noEffects $
    set (dashboardViewState <<< dbViewBlocksExpanded) expanded state

update (DashboardExpandTransactions expanded) state = noEffects $
    set (dashboardViewState <<< dbViewTxsExpanded) expanded state

update (DashboardPaginateBlocks value) state =
    { state:
        set (dashboardViewState <<< dbViewBlockPagination) value state
    , effects:
        if doRequest
        then [ pure $ RequestPaginatedBlocks limit offset ]
        else []
    }
    where
        lengthBlocks = length $ withDefault [] (state ^. latestBlocks)
        totalBlocks' = withDefault 0 (state ^. totalBlocks)
        doRequest = value > state ^. (dashboardViewState <<< dbViewBlockPagination)
                    && lengthBlocks < (value * maxBlockRows)
                    && lengthBlocks < totalBlocks'
        offset = (value - (state ^. (dashboardViewState <<< dbViewBlockPagination))) * maxBlockRows
        limit = value * maxBlockRows - lengthBlocks

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
        -- FIXME: scrollTop disabled as a workaround for socketio
        [ -- liftEff scrollTop >>= \_ -> pure NoOp
        ]
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

update RequestTotalBlocks state =
    { state:
          set totalBlocks Loading $ state
    , effects:
          [ attempt fetchTotalBlocks >>= pure <<< ReceiveTotalBlocks
          ]
    }

update (ReceiveTotalBlocks (Right total)) state =
    { state:
          set totalBlocks (Success total) $ state
    , effects:
        -- request `latestBlocks` if needed only
        if isNotAsked $ state ^. latestBlocks
        then [ pure RequestInitialBlocks ]
        else []
    }

update (ReceiveTotalBlocks (Left error)) state =
    noEffects $
    set totalBlocks (Failure error)
    $ over errors (\errors' -> (show error) : errors') state

update RequestInitialBlocks state =
    { state: set loading true $ state
    , effects: [ attempt (fetchLatestBlocks maxBlockRows 0) >>= pure <<< ReceiveInitialBlocks ]
    }

update (ReceiveInitialBlocks (Right blocks)) state =
    { state:
          set loading false <<<
          -- add blocks
          set latestBlocks (Success $ sortBlocksByTime blocks) $
          -- at this point we are ready to pull block updates
          set pullLatestBlocks true state
    , effects:
          -- add subscription of `SubBlock` if we are on `Dashboard` only
          if (state ^. route) == Dashboard
          then  [ pure $ SocketUpdateSubscriptions [ SocketSubscription SubBlock ] ]
          else []
    }

update (ReceiveInitialBlocks (Left error)) state =
    noEffects $
    set loading false <<<
    set latestBlocks (Failure error) $
    over errors (\errors' -> (show error) : errors') state

-- START #Pulling blocks
-- Pulling blocks is just a workaround to avoid issues w/ socket-io
-- TODO (jk) Remove this workaround if socket-io is back
update RequestBlocksUpdate state =
    { state: set loading true $ state
    , effects: [ attempt (fetchLatestBlocks 10 0) >>= pure <<< ReceiveBlocksUpdate ]
    }

update (ReceiveBlocksUpdate (Right blocks)) state =
    noEffects $
    set loading false <<<
    set latestBlocks (Success newBlocks) $
    over (totalBlocks <<< _Success) ((+) numberNewBlocks) state
    where
        getHash block = block ^. (_CBlockEntry <<< cbeBlkHash <<< _CHash)
        -- Note:  To "union" current with new blocks we have to compare CBlockEntry
        --        Because we don't have an Eq instance of generated CBlockEntry's
        --        As a workaround we do have to compare CBlockEntry by its hash
        unionBlocks = unionBy (\b1 b2 -> getHash b1 == getHash b2)
        previousBlocks = withDefault [] $ state ^. latestBlocks
        newBlocks = sortBlocksByTime $ unionBlocks blocks previousBlocks
        numberNewBlocks = (length newBlocks) - (length previousBlocks)

update (ReceiveBlocksUpdate (Left error)) state =
    noEffects $
    set loading false <<<
    set latestBlocks (Failure error) $
    over errors (\errors' -> (show error) : errors') state

-- END #Pulling blocks

update (RequestPaginatedBlocks limit offset) state =
    { state: set loading true $ state
    , effects: [ attempt (fetchLatestBlocks limit offset) >>= pure <<< ReceivePaginatedBlocks ]
    }

update (ReceivePaginatedBlocks (Right blocks)) state =
    noEffects $
    set loading false $
    set latestBlocks (Success newBlocks) state
    where
        previousBlocks = withDefault [] $ state ^. latestBlocks
        newBlocks = sortBlocksByTime $ previousBlocks <> blocks

update (ReceivePaginatedBlocks (Left error)) state =
    noEffects $
    set loading false <<<
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
          set currentBlocksResult Loading $
          state
    , effects: [ attempt (searchEpoch epoch slot) >>= pure <<< ReceiveSearchBlocks ]
    }
update (ReceiveSearchBlocks (Right blocks)) state =
    noEffects $
    set loading false <<<
    set currentBlocksResult (Success blocks) $
    state

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
update RequestInitialTxs state =
    { state: set loading true state
    , effects: [ attempt fetchLatestTxs >>= pure <<< ReceiveInitialTxs ]
    }
update (ReceiveInitialTxs (Right txs)) state =
    { state:
          set loading false $
          over latestTransactions (\currentTxs ->
                                        if isSuccess currentTxs
                                        -- union txs together
                                        then Success $ unionTxs txs (currentTxs ^. _Success)
                                        else Success txs
                                  )
          state
    , effects:
          -- add subscription of `SubTx` if we are on `Dashboard` only
          if (state ^. route) == Dashboard
          then [ pure $ SocketUpdateSubscriptions [ SocketSubscription SubTx ] ]
          else []

    }
    where
      getId tx = tx ^. (_CTxEntry <<< cteId <<< _CTxId <<< _CHash)
      -- Note:  To "union" current with new `txs` we have to compare CTxEntry
      --        Because we don't have an Eq instance of generated CTxEntry's
      --        As a workaround we do have to compare CTxEntry by its id
      unionTxs = unionBy (\tx1 tx2 -> getId tx1 == getId tx2)

update (ReceiveInitialTxs (Left error)) state = noEffects $
    set loading false $
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

-- clock
update UpdateClock state = onlyEffects state $
    [ do
         SetClock <<< extract <$> liftEff nowDateTime
    ]
update (SetClock date) state = noEffects $ state { now = date }

-- socket-io workaround
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
        , pure RequestInitialTxs
        ]
        -- get `totalBlocks` only once
        <>  if isNotAsked $ state ^. totalBlocks
            then [ pure RequestTotalBlocks ]
            else []
        -- request `latestBlocks` only if `totalBlocks` has been loaded before
        <>  if  (isSuccess $ state ^. totalBlocks) &&
                (isNotAsked $ state ^. latestBlocks) &&
                (not $ state ^. pullLatestBlocks)
            then [ pure RequestInitialBlocks ]
            else []
        -- pull latest blocks if neededMai
        <>  if state ^. pullLatestBlocks
            then [ pure RequestBlocksUpdate ]
            else []
        -- subscribe to `SubBlock` if `latestBlocks` has been loaded before
        <>  if isSuccess $ state ^. latestBlocks
            then [ pure $ SocketUpdateSubscriptions [ SocketSubscription SubBlock ] ]
            else []
        -- subscribe to `SubTx` if `latestTransactions` has been loaded before
        <>  if isSuccess $ state ^. latestTransactions
            then [ pure $ SocketUpdateSubscriptions [ SocketSubscription SubTx ] ]
            else []
    }

routeEffects (Tx tx) state =
    { state:
        set currentTxSummary Loading $
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
        set currentAddressSummary Loading $
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

-- helper functions

-- | Returns an offset of blocks at initial start of application
initialBlocksOffset :: State -> CBlockEntriesOffset
initialBlocksOffset state =
    (state ^. (dashboardViewState <<< dbViewBlockPagination)) - 1
