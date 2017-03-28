module Explorer.Types.Actions where

import           Control.Monad.Eff.Exception  (Error)
import           Data.Either                  (Either)
import           DOM.HTML.Types               (HTMLInputElement)
import           Explorer.I18n.Lang           (Language)
import           Explorer.Routes              (Route)
import           Explorer.Types.State         (CBlockEntries, CTxBriefs, CTxEntries,
                                               DashboardAPICode)
import           Pos.Explorer.Web.ClientTypes (CAddress, CAddressSummary, CBlockSummary,
                                               CHash, CTxId, CTxSummary)
import           Signal.Channel               (Channel)

data Action
    = SetLanguage Language
    -- routing
    | UpdateView Route
    -- DOM
    | ScrollTop
    | SelectInputText HTMLInputElement
    -- QR code
    | GenerateQrCode CAddress
    -- socket endpoints
    | SocketConnected Boolean
    | SocketLatestBlocks (Either Error CBlockEntries)
    | SocketLatestTransactions (Either Error CTxEntries)
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
    -- dashboard
    | DashboardExpandBlocks Boolean         -- toggle blocks
    | DashboardPaginateBlocks Int           -- current pagination of blocks
    | DashboardExpandTransactions Boolean   -- dashboard transactions
    | DashboardShowAPICode DashboardAPICode -- dashboard api
    | DashboardSearch                       -- dasboard search
    | DashboardFocusSearchInput Boolean
    -- address detail
    | AddressPaginateTxs Int       -- current pagination of transactions
    -- block detail
    | BlockPaginateTransactions Int       -- current pagination of transactions
    -- misc
    | NoOp


type ActionChannel = Channel Action
