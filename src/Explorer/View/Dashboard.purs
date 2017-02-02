module Explorer.View.Dashboard (dashboardView) where

import Prelude
import Data.Array (slice)
import Data.Lens (Lens', (^.))
import Data.Map (Map, fromFoldable, lookup, toAscUnfoldable) as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (age, height, relayedBy, sizeKB, totalSent, transactions, title, subtitle, transactionFeed) as I18nL
import Explorer.Lenses.State (blocksExpanded, dashboard, searchInput, selectedApiCode, transactionsExpanded, viewStates)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.Generated (CCurrency(..))
import Explorer.Types.State (DashboardAPICode(..), State, DashboardViewState)
import Explorer.View.Common (currencyCSSClass, paginationView)
import Pux.Html (Html, div, h3, text, h1, h2, input, h4, p, code) as P
import Pux.Html.Attributes (className, type_, placeholder) as P
import Pux.Html.Events (onClick, onFocus, onBlur) as P

dashboardView :: State -> P.Html Action
dashboardView state =
    P.div
        [ P.className "explorer-dashboard" ]
        [ heroView state
        , networkView state
        , blocksView state
        , transactionsView state
        , offerView state
        , apiView state
        ]

-- header view

newtype HeaderOptions = HeaderOptions
    { headline :: String
    , link :: Maybe HeaderLink
    }

newtype HeaderLink = HeaderLink
    { label :: String
    , action :: Action
    }

headerView :: State -> HeaderOptions -> P.Html Action
headerView state (HeaderOptions options) =
    P.div
        [ P.className "explorer-dashboard__header" ]
        [ P.h3
            [ P.className "headline"]
            [ P.text options.headline ]
        , P.div
            [ P.className "more__container"]
            [ linkView options.link ]
        ]
    where
      linkView link = case link of
          Just (HeaderLink link') ->
              P.div
                  [ P.className "more__link bg-arrow-right" ]
                  [ P.text link'.label ]
          Nothing -> P.div [] []

-- hero

heroView :: State -> P.Html Action
heroView state =
    let
        searchInputFocused = state ^. (dashboardViewState <<< searchInput)
        focusedClazz = if searchInputFocused then " focused" else ""
        searchIconClazz = if searchInputFocused then " bg-icon-search-hover" else " bg-icon-search"
    in
    P.div
        [ P.className "explorer-dashboard__hero" ]
        [ P.div
            [ P.className "hero-container" ]
            [ P.h1
                [ P.className "hero-headline" ]
                [ P.text $ translate I18nL.title state.lang ]
            , P.h2
                [ P.className "hero-subheadline"]
                [ P.text $ translate I18nL.subtitle state.lang ]
            , P.div
                [ P.className $ "hero-search-container" <> focusedClazz ]
                [ P.input
                    [ P.className $ "hero-input" <> focusedClazz
                      , P.type_ "text"
                      , P.placeholder $ if searchInputFocused
                                        then ""
                                        else "# Search for address, block, token"
                      , P.onFocus <<< const $ DashboardFocusSearchInput true
                      , P.onBlur <<< const $ DashboardFocusSearchInput false ]
                    []
                , P.div
                    [ P.className $ "hero-search-btn" <> searchIconClazz <> focusedClazz
                    , P.onClick $ const Search ]
                    []
                ]
            ]
        ]


-- network


-- FIXME (jk): just for now, will use later `real` ADTs
type NetworkItems = Array NetworkItem

-- FIXME (jk): just for now, will use later `real` ADTs
type NetworkItem =
    { headline :: String
    , subheadline :: String
    , description :: String
    }

networkItems :: NetworkItems
networkItems =
    [ { headline: "Last Block", subheadline: "123456"
      , description: "generated on 01.01.2017 12:00:00 \n\n 50 transactions" }
    , { headline: "Network difficulty", subheadline: "1,234,567,890.12"
      , description: "Difficulty is a measure of how difficult it is to find a new block below a given target." }
    , { headline: "Price (average)", subheadline: "1,000,000$ for 1 ADA"
      , description: "20% more since yesterday." }
    , { headline: "Total supply", subheadline: "9,876,543,210 ADA"
      , description: "Amount of ADA in the system." }
    , { headline: "Transactions", subheadline: "82,491,247,592,742,929"
      , description: "Total amount of transactions detected in system since the beginning." }
    ]


networkView :: State -> P.Html Action
networkView state =
    P.div
        [ P.className "explorer-dashboard__wrapper" ]
        [ P.div
          [ P.className "explorer-dashboard__container" ]
          [ P.h3
                [ P.className "headline"]
                [ P.text "#Network" ]
          , P.div
                [ P.className "explorer-dashboard__teaser" ]
                $ map (networkItem state) networkItems
          ]
        ]

networkItem :: State -> NetworkItem -> P.Html Action
networkItem state item =
    P.div
        [ P.className "teaser-item" ]
        [ P.h3
            [ P.className "teaser-item__headline" ]
            [ P.text item.headline ]
        , P.div
              [ P.className $ "teaser-item__border" ]
              []
        , P.h4
              [ P.className $ "teaser-item__subheadline" ]
              [ P.text item.subheadline ]
        , P.p
              [ P.className $ "teaser-item__description" ]
              [ P.text item.description ]
        ]



-- blocks


-- FIXME (jk): just for now, will use later `real` ADTs
type BlockItems = Array BlockItem

-- FIXME (jk): just for now, will use later `real` ADTs
type BlockItem =
    { height :: Int
    , age :: String
    , transactions :: Int
    , totalSent :: Int
    , relayedBy :: String
    , sizeKb :: Int
    }

blockItems :: BlockItems
blockItems =
    [ { height: 419821, age: "9 minutes", transactions: 358000, totalSent: 58200, relayedBy: "Unknown", sizeKb: 123 }
    , { height: 419821, age: "7 hours", transactions: 1200, totalSent: 600, relayedBy: "Unknown", sizeKb: 1234 }
    , { height: 419821, age: "3 days", transactions: 69000, totalSent: 7300, relayedBy: "KNCMiner", sizeKb: 234 }
    , { height: 419821, age: "2 days", transactions: 67000, totalSent: 7700, relayedBy: "Unknown", sizeKb: 134 }
    ]

blocksView :: State -> P.Html Action
blocksView state =
    P.div
        [ P.className "explorer-dashboard__wrapper" ]
        [ P.div
          [ P.className "explorer-dashboard__container" ]
          [ headerView state headerOptions
            , blocksHeaderView state
            , P.div
              [ P.className "blocks-body" ]
              $ map (blockRow state) blockItems'
            , P.div
              [ P.className "blocks-footer" ]
              [ blocksFooterView ]
          ]
        ]
      where
        headerOptions = HeaderOptions
            { headline: "#Last Blocks"
            , link: Just $ HeaderLink { label: "#Explore blocks", action: NoOp }
            }
        -- expanded = state ^. dashboardBlocksExpanded
        expanded = state ^. (viewStates <<< dashboard <<< searchInput)

        blockItems' :: BlockItems
        blockItems' = if expanded
            then slice 0 maxBlockRows blockItems
            else slice 0 minBlockRows blockItems

        blocksFooterView :: P.Html Action
        blocksFooterView = if expanded then
            paginationView state
            else
            P.div
              [ P.className "btn-expand"
              , P.onClick <<< const $ DashboardExpandBlocks true ]
              [ P.text "#expand"]


maxBlockRows :: Int
maxBlockRows = 10

minBlockRows :: Int
minBlockRows = 3

blockRow :: State -> BlockItem -> P.Html Action
blockRow state item =
    P.div
        [ P.className "blocks-body__row" ]
        [ blockColumn $ show item.height
        , blockColumn item.age
        , blockColumn $ show item.transactions
        , blockColumn $ show item.totalSent
        , blockColumn item.relayedBy
        , blockColumn $ show item.sizeKb
        ]

blockColumn :: String -> P.Html Action
blockColumn value =
    P.div
        [ P.className "blocks-body__column" ]
        [ P.text value ]

blocksHeaderView :: State -> P.Html Action
blocksHeaderView state =
    P.div
          [ P.className "blocks-header"]
          $ map (blockHeaderItemView state) $ mkBlocksHeaderItems state.lang

blockHeaderItemView :: State -> String -> P.Html Action
blockHeaderItemView state label =
    P.div
        [ P.className "blocks-header__item" ]
        [ P.text label ]

mkBlocksHeaderItems :: Language -> Array String
mkBlocksHeaderItems lang =
    [ translate I18nL.height lang
    , translate I18nL.age lang
    , translate I18nL.transactions lang
    , translate I18nL.totalSent lang
    , translate I18nL.relayedBy lang
    , translate I18nL.sizeKB lang
    ]

-- transactions

-- FIXME (jk): just for now, will use later `real` ADTs
type TransactionItems = Array TransactionItem

-- FIXME (jk): just for now, will use later `real` ADTs
type TransactionItem =
    { hash :: String
    , age :: String
    , amount :: Int
    , amountCurrency :: Maybe CCurrency
    , exchange :: Number
    , exchangeCurrency :: Maybe CCurrency
    }

transactionItem :: TransactionItem
transactionItem =
    { hash: "46087134cd072aec7d36c15a6726bfc07ea78884b352d16f333433bb224f7f6"
        , age: "< 1 minute", amount: 123383, amountCurrency: Just ADA
        , exchange: 12.98, exchangeCurrency: Just USD }

transactionItems :: TransactionItems
transactionItems =
    [ transactionItem
    , transactionItem
    , transactionItem
    , transactionItem
    , transactionItem
    , transactionItem
    , transactionItem
    , transactionItem
    , transactionItem
    , transactionItem
    , transactionItem
    ]

maxTransactionRows :: Int
maxTransactionRows = 10

minTransactionRows :: Int
minTransactionRows = 5

transactionsView :: State -> P.Html Action
transactionsView state =
    P.div
        [ P.className "explorer-dashboard__wrapper" ]
        [ P.div
          [ P.className "explorer-dashboard__container" ]
          [ headerView state headerOptions
          , P.div
              [ P.className "transactions__container" ]
              $ map (transactionRow state) $ transactionItems'
          , P.div
            [ P.className "transactions__footer" ]
            [ P.div
                [ P.className "btn-expand"
                , P.onClick <<< const <<< DashboardExpandTransactions $ not expanded ]
                [ P.text expandLabel]
            ]
          ]
        ]
    where
      expanded = state ^. dashboardTransactionsExpanded
      expandLabel = if expanded then "#collapse" else "#expand"
      headerOptions = HeaderOptions
          { headline: translate I18nL.transactionFeed state.lang
          , link: Just $ HeaderLink { label: "#Explore transactions", action: NoOp }
          }
      transactionItems' :: TransactionItems
      transactionItems' = if expanded
          then slice 0 maxTransactionRows transactionItems
          else slice 0 minTransactionRows transactionItems

transactionRow :: State -> TransactionItem -> P.Html Action
transactionRow state item =
    P.div
        [ P.className "transactions__row" ]
        [ transactionColumn item.hash "hash"
        , transactionColumn item.age ""
        , transactionColumn (show item.amount) $ currencyCSSClass item.amountCurrency
        , transactionColumn (show item.exchange) $ currencyCSSClass item.exchangeCurrency
        ]

transactionColumn :: String -> String -> P.Html Action
transactionColumn value clazzName =
    P.div
        [ P.className $ "transactions__column " <> clazzName ]
        [ P.text value ]


-- offer


-- FIXME (jk): just for now, will use later `real` ADTs
type OfferItems = Array OfferItem

-- FIXME (jk): just for now, will use later `real` ADTs
type OfferItem =
    { headline :: String
    , description :: String
    }

offerItems :: OfferItems
offerItems =
    [ { headline: "Block Search"
      , description: "Block is a box where transactions are stored." }
    , { headline: "Address Search"
      , description: "Address is similar to what an account in ordinary bank is." }
    , { headline: "Transactions Search"
      , description: "Transaction is transfer of coins from user A to user B." }
    , { headline: "API"
      , description: "Our robust API is available in a variety of languages & SDKs." }
    ]


offerView :: State -> P.Html Action
offerView state =
    P.div
        [ P.className "explorer-dashboard__wrapper" ]
        [ P.div
          [ P.className "explorer-dashboard__container" ]
          [ P.h3
                [ P.className "headline"]
                [ P.text "#WHAT DO WE OFFER ON OUR BLOCK EXPLORER" ]
          , P.div
                [ P.className "explorer-dashboard__teaser" ]
                $ map (offerItem state) offerItems
          ]
        ]

offerItem :: State -> OfferItem -> P.Html Action
offerItem state item =
    P.div
        [ P.className "teaser-item" ]
        [ P.h3
            [ P.className "teaser-item__headline" ]
            [ P.text item.headline ]
        , P.div
              [ P.className $ "teaser-item__border" ]
              []
        , P.p
              [ P.className $ "teaser-item__description" ]
              [ P.text item.description ]
        ]


-- API

type ApiTabLabel = String

type ApiCode =
    { getAddress :: String
    , response :: String
    }

emptyApiCode :: ApiCode
emptyApiCode = { getAddress: "", response: ""}

apiCodes :: M.Map DashboardAPICode ApiCode
apiCodes =
  M.fromFoldable([
    Tuple Curl { getAddress: "Curl getAddress", response: "{\n\t\"hash\": }"}
    , Tuple Node { getAddress: "Node getAddress", response: "Node response ..."}
    , Tuple JQuery { getAddress: "jQuery getAddress", response: "jQuery response ..."}
    ])

apiView :: State -> P.Html Action
apiView state =
    P.div
        [ P.className "explorer-dashboard__wrapper" ]
        [ P.div
          [ P.className "explorer-dashboard__container" ]
          [ headerView state headerOptions
          , P.div
            [ P.className "api-content" ]
            [ P.div
                [ P.className "api-content__container api-code"]
                [ P.div
                  [ P.className "api-code__tabs" ]
                  $ map (apiCodeTabView state) $ M.toAscUnfoldable apiTabs
                , apiCodeSnippetView "#Get Address" addressSnippet
                , apiCodeSnippetView "#Get Response" responseSnippet
                ]
            , P.div
                [ P.className "api-content__container api-about"]
                [ P.h3
                    [ P.className "api-about__headline" ]
                    [ P.text "#About Blockchain" ]
                , P.p
                    [ P.className "api-about__description" ]
                    [ P.text "#Blockchain API makes it easy yo build cryptocurrensies applications and features. We are focused on ... \n\n This API is free and unlimited while we are in beta. We are just getting started, and will be rolling out more ..."
                    ]
                , P.div
                  [ P.className "api-about__button" ]
                  [ P.text "#Get API Key"]
                ]
            ]
          ]
        ]
    where
      apiCode :: ApiCode
      apiCode = fromMaybe emptyApiCode $ M.lookup (state ^. dashboardSelectedApiCode) apiCodes
      addressSnippet = _.getAddress $ apiCode
      responseSnippet = _.response $ apiCode
      headerOptions = HeaderOptions
          { headline: "#API"
          , link: Just $ HeaderLink { label: "#See more examples", action: NoOp }
          }

apiTabs :: M.Map DashboardAPICode ApiTabLabel
apiTabs =
    M.fromFoldable(
        [ Tuple Curl "#Curl"
        , Tuple Node "#Node"
        , Tuple JQuery "#jQuery"
        ])


apiCodeTabView :: State -> Tuple DashboardAPICode ApiTabLabel -> P.Html Action
apiCodeTabView state (Tuple code label) =
    P.div
      [ P.className $ "api-code__tab " <> selectedClazz
      , P.onClick <<< const $ DashboardShowAPICode code ]
      [ P.text label ]
    where
      selectedClazz = if state ^. dashboardSelectedApiCode == code then "selected" else ""


apiCodeSnippetView :: String -> String -> P.Html Action
apiCodeSnippetView headline snippet =
    P.div
        [ P.className "api-snippet" ]
        [ P.h3
            [ P.className "api-snippet__headline" ]
            [ P.text headline ]
        ,  P.div
              [ P.className "api-snippet__border" ]
              []
        , P.code
            [ P.className "api-snippet__code" ]
            [ P.text snippet ]

        ]


-- lenses

dashboardViewState :: Lens' State DashboardViewState
dashboardViewState = viewStates <<< dashboard

dashboardBlocksExpanded :: Lens' State Boolean
dashboardBlocksExpanded = dashboardViewState <<< blocksExpanded

dashboardTransactionsExpanded :: Lens' State Boolean
dashboardTransactionsExpanded = dashboardViewState <<< transactionsExpanded

dashboardSelectedApiCode :: Lens' State DashboardAPICode
dashboardSelectedApiCode = dashboardViewState <<< selectedApiCode
