module Explorer.Types.Actions where

import Control.Monad.Eff.Exception (Error)
import DOM.Event.Event (Event)
import DOM.HTML.Types (HTMLElement, HTMLInputElement)
import DOM.Node.Types (ElementId)
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Explorer.Types.State (CBlockEntries, CTxBriefs, CTxEntries, DashboardAPICode, PageNumber, PageSize, Search, SocketSubscriptionItem, WaypointItem)
import Pos.Core.Types (EpochIndex, LocalSlotIndex)
import Pos.Explorer.Web.ClientTypes (CAddress, CAddressSummary, CBlockSummary, CHash, CTxId, CTxSummary)
import Pux.DOM.Events (DOMEvent)
import Signal.Channel (Channel)

data Action
    = SetLanguage Language
    -- routing
    | Navigate String DOMEvent
    | UpdateView Route
    -- DOM
    | ScrollTop
    | SelectInputText HTMLInputElement
    | ClearWaypoints
    | StoreWaypoint WaypointItem
    | BlurElement HTMLElement
    | FocusElement HTMLElement
    | DocumentClicked Event
    -- QR code
    | GenerateQrCode CAddress
    -- socket endpoints
    | SocketConnected Boolean
    | SocketBlocksPageUpdated (Either Error (Tuple Int CBlockEntries))
    | SocketTxsUpdated (Either Error CTxEntries)
    | SocketAddressTxsUpdated (Either Error CTxBriefs)
    | SocketAddSubscription SocketSubscriptionItem
    | SocketRemoveSubscription SocketSubscriptionItem
    | SocketClearSubscriptions
    | SocketReconnectSubscriptions
    | SocketPing
    -- socket endpoints for debugging only
    | SocketCallMe
    | SocketCallMeString String
    | SocketCallMeCTxId CTxId
    -- http endpoints
    | RequestPaginatedBlocks PageNumber PageSize
    | ReceivePaginatedBlocks (Either Error (Tuple Int CBlockEntries))
    | RequestBlockSummary CHash
    | ReceiveBlockSummary (Either Error CBlockSummary)
    | RequestBlockTxs CHash
    | ReceiveBlockTxs (Either Error CTxBriefs)
    | RequestLastTxs
    | ReceiveLastTxs (Either Error CTxEntries)
    | RequestTxSummary CTxId
    | ReceiveTxSummary (Either Error CTxSummary)
    | RequestAddressSummary CAddress
    | ReceiveAddressSummary (Either Error CAddressSummary)
    | RequestSearchBlocks EpochIndex (Maybe LocalSlotIndex)
    | ReceiveSearchBlocks (Either Error CBlockEntries)
    -- global view states
    | GlobalToggleMobileMenu Boolean
    | GlobalSearch DOMEvent                          -- search for address + transaction
    | GlobalSearchTime DOMEvent                      -- search for time
    | GlobalUpdateSelectedSearch Search
    | GlobalUpdateSearchValue String
    | GlobalUpdateSearchEpochValue String
    | GlobalUpdateSearchSlotValue String
    | GlobalFocusSearchInput Boolean
    -- dashboard view
    | DashboardRequestBlocksTotalPages
    | DashboardReceiveBlocksTotalPages (Either Error Int)
    | DashboardExpandBlocks Boolean                   -- expand list of blocks
    | DashboardPaginateBlocks (Maybe DOMEvent) PageNumber     -- pagination of blocks
    | DashboardEditBlocksPageNumber DOMEvent Boolean  -- toggle editable state of page numbers
    | DashboardInvalidBlocksPageNumber DOMEvent       -- invalid page number
    | DashboardExpandTransactions Boolean             -- expand dashboard transactions
    | DashboardShowAPICode DashboardAPICode           -- toggle dashboard api
    | DashboardAddWaypoint ElementId
    -- address detail view
    | AddressPaginateTxs (Maybe DOMEvent) PageNumber      -- current pagination of transactions
    | AddressEditTxsPageNumber DOMEvent Boolean   -- toggle editable state of page numbers
    | AddressInvalidTxsPageNumber DOMEvent        -- invalid page number
    -- block detail view
    | BlockPaginateTxs (Maybe DOMEvent) PageNumber                 -- current pagination of transactions
    | BlockEditTxsPageNumber DOMEvent Boolean     -- toggle editable state of page numbers
    | BlockInvalidTxsPageNumber DOMEvent          -- invalid page number
    -- blocks view
    | BlocksPaginateBlocks (Maybe DOMEvent) PageNumber        -- current pagination of blocks
    | BlocksEditBlocksPageNumber DOMEvent Boolean     -- toggle editable state of page numbers
    | BlocksInvalidBlocksPageNumber DOMEvent          -- invalid page number
    -- clock
    | SetClock DateTime
    | UpdateClock
    -- misc
    | Reload -- Reload pages - TODO (jk) Remove it if socket-io will be fixed
    | NoOp

type ActionChannel = Channel Action
