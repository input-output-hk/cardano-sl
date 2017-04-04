module Explorer.State where

import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (Language(..))
import Explorer.Routes (Route(..))
import Explorer.Types.State (DashboardAPICode(..), Search(..), State)
import Explorer.Util.Factory (mkCAddress)
import Network.RemoteData (RemoteData(..))


initialState :: State
initialState =
    { lang: English
    , route: Dashboard
    , socket:
        { connected: false
        }
    , viewStates:
        { dashboard:
            { blocksExpanded: false
            , dashboardBlockPagination: 1 -- Note: We do start with 1 (not 0)
            , transactionsExpanded: false
            , selectedApiCode: Curl
            , searchInput: false
            }
        , addressDetail:
            { addressTxPagination: 1 -- Note: We do start with 1 (not 0)
            }
        , blockDetail:
            { blockTxPagination: 1 -- Note: We do start with 1 (not 0)
            }
        }
    , latestBlocks: []
    , initialBlocksRequested: false
    , handleLatestBlocksSocketResult: false
    , initialTxsRequested: false
    , handleLatestTxsSocketResult: false
    , currentBlockSummary: Nothing
    , currentBlockTxs: Nothing
    , latestTransactions: []
    , currentTxSummary: NotAsked
    , currentCAddress: mkCAddress ""
    , currentAddressSummary: NotAsked
    , selectedSearch: SearchAddress
    , searchQuery: emptySearchQuery
    , errors: []
    , loading: false
    }

emptySearchQuery :: String
emptySearchQuery = ""
