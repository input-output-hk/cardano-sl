module Explorer.State where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Explorer.I18n.Lang (Language(..), translate)
import Explorer.I18n.Lenses (common, cTitle) as I18nL
import Explorer.Routes (Route(..))
import Explorer.Types.State (DashboardAPICode(..), Search(..), State, SearchEpochSlotQuery)
import Explorer.Util.Factory (mkCAddress)
import Network.RemoteData (RemoteData(..))


initialState :: State
initialState =
    { lang: English
    , route: Dashboard
    , socket:
        { connected: false
        , connection: Nothing
        , subscriptions: []
        }
    , viewStates:
        { globalViewState:
            { gViewMobileMenuOpenend: false
            , gViewTitle: translate (I18nL.common <<< I18nL.cTitle) English
            }
        ,  dashboard:
            { dbViewBlocksExpanded: false
            , dbViewBlockPagination: minPagination
            , dbViewTxsExpanded: false
            , dbViewSelectedApiCode: Curl
            , dbViewSearchInput: false
            }
        , addressDetail:
            { addressTxPagination: minPagination
            }
        , blockDetail:
            { blockTxPagination: minPagination
            }
        , blocksViewState:
            { blsViewPagination: minPagination
            }
        }
    , latestBlocks: NotAsked
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
    , searchTimeQuery: emptySearchTimeQuery
    , currentBlocksResult: NotAsked
    , errors: []
    , loading: false
    }

-- all constants are following here:

emptySearchQuery :: String
emptySearchQuery = ""

emptySearchTimeQuery :: SearchEpochSlotQuery
emptySearchTimeQuery = Tuple Nothing Nothing

maxSlotInEpoch :: Int
maxSlotInEpoch = 21600

minPagination :: Int
minPagination = 1 -- Note: We do start with 1 (not 0)

addressQRImageId :: String
addressQRImageId = "qr_image_id"
