module Explorer.Types.Actions where

import Control.Monad.Eff.Exception (Error)
import DOM.HTML.Types (HTMLElement, HTMLInputElement)
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Explorer.Types.State (CBlockEntries, CBlockEntriesOffset, CTxBriefs, CTxEntries, DashboardAPICode, Search, SocketSubscription, CBlockEntriesLimit)
import Pos.Core.Types (EpochIndex, LocalSlotIndex)
import Pos.Explorer.Web.ClientTypes (CAddress, CAddressSummary, CBlockSummary, CHash, CTxId, CTxSummary)
import Pux.Html.Events (Target)
import Signal.Channel (Channel)

data Action
    = SetLanguage Language
    -- routing
    | UpdateView Route
    -- DOM
    | ScrollTop
    | SelectInputText HTMLInputElement
    | BlurElement HTMLElement
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
    | RequestTotalBlocks
    | ReceiveTotalBlocks (Either Error Int)
    | RequestInitialBlocks
    | ReceiveInitialBlocks (Either Error CBlockEntries)
    | RequestPaginatedBlocks CBlockEntriesLimit CBlockEntriesOffset
    | ReceivePaginatedBlocks (Either Error CBlockEntries)
    | RequestBlocksUpdate                               -- TODO (jk) Remove it if socket-io is back
    | ReceiveBlocksUpdate (Either Error CBlockEntries)  -- TODO (jk) Remove it if socket-io is back
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
    | DashboardExpandBlocks Boolean                 -- expand list of blocks
    | DashboardPaginateBlocks Int                   -- pagination of blocks
    | DashboardEditBlocksPageNumber Target Boolean  -- toggle editable state of page numbers
    | DashboardInvalidBlocksPageNumber Target       -- invalid page number
    | DashboardExpandTransactions Boolean           -- expand dashboard transactions
    | DashboardShowAPICode DashboardAPICode         -- toggle dashboard api
    -- address detail view
    | AddressPaginateTxs Int                    -- current pagination of transactions
    | AddressEditTxsPageNumber Target Boolean   -- toggle editable state of page numbers
    | AddressInvalidTxsPageNumber Target        -- invalid page number
    -- block detail view
    | BlockPaginateTxs Int                      -- current pagination of transactions
    | BlockEditTxsPageNumber Target Boolean     -- toggle editable state of page numbers
    | BlockInvalidTxsPageNumber Target          -- invalid page number
    -- blocks view
    | BlocksPaginateBlocks Int                      -- current pagination of blocks
    | BlocksEditBlocksPageNumber Target Boolean     -- toggle editable state of page numbers
    | BlocksInvalidBlocksPageNumber Target          -- invalid page number
    -- clock
    | SetClock DateTime
    | UpdateClock
    -- misc
    | NoOp

type ActionChannel = Channel Action
