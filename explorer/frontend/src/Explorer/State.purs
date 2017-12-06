module Explorer.State where

import Prelude

import DOM.Node.Types (ElementId(..))
import Data.DateTime.Instant (instant, toDateTime)
import Data.Foldable (elem)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromJust)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Explorer.Api.Types (SocketSubscription, SocketSubscriptionData)
import Explorer.I18n.Lang (Language(..))
import Explorer.Lenses.State (socket, subscriptions)
import Explorer.Routes (Route(..))
import Explorer.Types.State (AddressesFilter(..), DashboardAPICode(..), PageNumber(..), Search(..), SearchEpochSlotQuery, SocketSubscriptionItem(..), State)
import Explorer.Util.Config (SyncAction(..))
import Explorer.Util.Factory (mkCAddress)
import Network.RemoteData (RemoteData(..))
import Partial.Unsafe (unsafePartial)
import Pos.Explorer.Web.ClientTypes (CAddressesFilter(..))

initialState :: State
initialState =
    { lang: English
    , route: Dashboard
    , socket:
        { connected: false
        , connection: Nothing
        , subscriptions: []
        }
    , syncAction: SyncBySocket
    -- , syncAction: SyncByPolling
    , viewStates:
        { globalViewState:
            { gViewMobileMenuOpenend: false
            , gViewSearchInputFocused: false
            , gViewSelectedSearch: SearchAddress
            , gViewSearchQuery: emptySearchQuery
            , gViewSearchTimeQuery: emptySearchTimeQuery
            , gWaypoints: []
            }
        ,  dashboard:
            { dbViewBlocksExpanded: false
            , dbViewBlockPagination: PageNumber minPagination
            , dbViewMaxBlockPagination: NotAsked
            , dbViewLoadingBlockPagination: false
            , dbViewBlockPaginationEditable: false
            , dbViewTxsExpanded: false
            , dbViewSelectedApiCode: Curl
            }
        , addressDetail:
            { addressTxPagination: PageNumber minPagination
            , addressTxPaginationEditable: false
            }
        , blockDetail:
            { blockTxPagination: PageNumber minPagination
            , blockTxPaginationEditable: false
            }
        , blocksViewState:
            { blsViewPagination: PageNumber minPagination
            , blsViewPaginated: false
            , blsViewMaxPagination: PageNumber minPagination
            , blsViewPaginationEditable: false
            , blsViewEpochIndex: Nothing
            , blsViewLoadingPagination: false
            }
        , genesisBlockViewState:
            { gblAddressInfosPagination: PageNumber minPagination
            , gblMaxAddressInfosPagination: NotAsked
            , gblAddressInfosPaginationEditable: false
            , gblLoadingAddressInfosPagination: false
            , gblAddressFilter: AddressesFilter AllAddresses
            }
        }
    , latestBlocks: NotAsked
    , currentBlockSummary: NotAsked
    , currentBlockTxs: NotAsked
    , latestTransactions: NotAsked
    , currentTxSummary: NotAsked
    , currentCAddress: mkCAddress ""
    , currentAddressSummary: NotAsked
    , currentBlocksResult: NotAsked
    , currentCGenesisSummary: NotAsked
    , currentCGenesisAddressInfos: NotAsked
    , errors: []
    , loading: false
    , now: toDateTime $ unsafePartial $ fromJust $ instant $ Milliseconds 0.0
    , testnet: false
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

heroSearchContainerId :: ElementId
heroSearchContainerId = ElementId "heroSearchContainerId"

headerSearchContainerId :: ElementId
headerSearchContainerId = ElementId "headerSearchContainerId"

mobileMenuSearchContainerId :: ElementId
mobileMenuSearchContainerId = ElementId "mobileMenuSearchContainerId"

mkSocketSubscriptionItem :: SocketSubscription -> SocketSubscriptionData -> SocketSubscriptionItem
mkSocketSubscriptionItem socketSub socketSubData = SocketSubscriptionItem
    { socketSub
    , socketSubData
    }

-- | Check if a subscription has been already stored
hasSubscription :: SocketSubscriptionItem -> State -> Boolean
hasSubscription subItem state =
    elem subItem $ state ^. (socket <<< subscriptions)
