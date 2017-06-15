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
import Pos.Explorer.Web.ClientTypes (CAddress, CAddressSummary, CBlockSummary, CHash, CTxBrief(..), CTxId, CTxSummary)
import Pux.Html.Events (Target)
import Signal.Channel (Channel)

data Action
    = SetLanguage Language
    -- routing
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
    | SocketAddressTxsUpdated (Either Error CTxBrief)
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
    | GlobalSearch                          -- search for address + transaction
    | GlobalSearchTime                      -- search for time
    | GlobalUpdateSelectedSearch Search
    | GlobalUpdateSearchValue String
    | GlobalUpdateSearchEpochValue String
    | GlobalUpdateSearchSlotValue String
    | GlobalFocusSearchInput Boolean
    -- dashboard view
    | DashboardRequestBlocksTotalPages
    | DashboardReceiveBlocksTotalPages (Either Error Int)
    | DashboardExpandBlocks Boolean                 -- expand list of blocks
    | DashboardPaginateBlocks PageNumber            -- pagination of blocks
    | DashboardEditBlocksPageNumber Target Boolean  -- toggle editable state of page numbers
    | DashboardInvalidBlocksPageNumber Target       -- invalid page number
    | DashboardExpandTransactions Boolean           -- expand dashboard transactions
    | DashboardShowAPICode DashboardAPICode         -- toggle dashboard api
    | DashboardAddWaypoint ElementId
    -- address detail view
    | AddressPaginateTxs PageNumber             -- current pagination of transactions
    | AddressEditTxsPageNumber Target Boolean   -- toggle editable state of page numbers
    | AddressInvalidTxsPageNumber Target        -- invalid page number
    -- block detail view
    | BlockPaginateTxs PageNumber               -- current pagination of transactions
    | BlockEditTxsPageNumber Target Boolean     -- toggle editable state of page numbers
    | BlockInvalidTxsPageNumber Target          -- invalid page number
    -- blocks view
    | BlocksPaginateBlocks PageNumber               -- current pagination of blocks
    | BlocksEditBlocksPageNumber Target Boolean     -- toggle editable state of page numbers
    | BlocksInvalidBlocksPageNumber Target          -- invalid page number
    -- clock
    | SetClock DateTime
    | UpdateClock
    -- misc
    | Reload -- Reload pages - TODO (jk) Remove it if socket-io will be fixed
    | NoOp

type ActionChannel = Channel Action
