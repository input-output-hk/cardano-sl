module Explorer.Types.Actions where

import Control.Monad.Eff.Exception (Error)
import DOM.HTML.Types (HTMLInputElement)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Explorer.Types.State (CBlockEntries, CTxBriefs, CTxEntries, DashboardAPICode, SocketSubscription, Search)
import Pos.Core.Types (EpochIndex, LocalSlotIndex)
import Pos.Explorer.Web.ClientTypes (CAddress, CAddressSummary, CBlockSummary, CHash, CTxId, CTxSummary)
import Signal.Channel (Channel)
import Waypoints (Waypoint, WaypointSelector)

data Action
    = SetLanguage Language
    -- routing
    | UpdateView Route
    -- DOM
    | ScrollTop
    | SelectInputText HTMLInputElement
    | AddWaypoint WaypointSelector
    | StoreWaypoint Waypoint
    -- QR code
    | GenerateQrCode CAddress
    -- socket endpoints
    | SocketConnected Boolean
    | SocketBlocksUpdated (Either Error CBlockEntries)
    | SocketTxsUpdated (Either Error CTxEntries)
    | SocketUpdateSubscriptions (Array SocketSubscription)
    | SocketReconnectSubscriptions
    -- socket endpoints for debugging only
    | SocketCallMe
    | SocketCallMeString String
    | SocketCallMeCTxId CTxId
    -- http endpoints
    | RequestInitialBlocks
    | ReceiveInitialBlocks (Either Error CBlockEntries)
    | RequestBlockSummary CHash
    | ReceiveBlockSummary (Either Error CBlockSummary)
    | RequestBlockTxs CHash
    | ReceiveBlockTxs (Either Error CTxBriefs)
    | RequestInitialTxs
    | ReceiveInitialTxs (Either Error CTxEntries)
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
    | DashboardExpandBlocks Boolean         -- expand list of blocks
    | DashboardPaginateBlocks Int           -- pagination of blocks
    | DashboardExpandTransactions Boolean   -- expand dashboard transactions
    | DashboardShowAPICode DashboardAPICode -- toggle dashboard api
    -- address detail view
    | AddressPaginateTxs Int                -- current pagination of transactions
    -- block detail view
    | BlockPaginateTxs Int                  -- current pagination of transactions
    -- blocks view
    | BlocksPaginateBlocks Int              -- current pagination of blocks
    -- misc
    | NoOp


type ActionChannel = Channel Action
