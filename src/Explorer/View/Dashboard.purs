module Explorer.View.Dashboard (dashboardView) where

import Prelude
import Data.Array (slice)
import Explorer.I18n.Lang (I18nAccessor, translate)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types (Action(..), State)
import Explorer.View.Common (paginationView)
import Pux.Html (Html, div, h3, text, h1, h2, input, h4, p) as P
import Pux.Html.Attributes (className, type_, placeholder) as P
import Pux.Html.Events (onClick) as P
import Pux.Router (link) as P

dashboardView :: State -> P.Html Action
dashboardView state =
    P.div
        [ P.className "explorer-dashboard" ]
        [ heroView state
        , networkView state
        , blocksView state
        , transactionsView state
        ]

-- hero

heroView :: State -> P.Html Action
heroView state =
    P.div
        [ P.className "explorer-dashboard__hero"]
        [ P.div
              [ P.className "explorer-dashboard__container" ]
              [ P.h1
                    [ P.className "headline__hero"]
                    [ P.text $ translate _.title state.lang ]
                , P.h2
                    [ P.className "subheadline__hero"]
                    [ P.text $ translate _.subtitle state.lang ]
                , P.input
                    [ P.className "input__hero"
                      , P.type_ "text"
                      , P.placeholder "# Search for address, block, token"]
                    []
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
                [ P.className "explorer-dashboard__network" ]
                $ map (networkItem state) networkItems
          ]
        ]

networkItem :: State -> NetworkItem -> P.Html Action
networkItem state item =
    P.div
        [ P.className "network-item" ]
        [ P.h3
            [ P.className "network-item__headline" ]
            [ P.text item.headline ]
        , P.div
              [ P.className $ "network-item__border" ]
              []
        , P.h4
              [ P.className $ "network-item__subheadline" ]
              [ P.text item.subheadline ]
        , P.p
              [ P.className $ "network-item__description" ]
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
          [ P.h3
                [ P.className "headline"]
                [ P.text "#Last Blocks" ]
            , blocksHeaderView state
            , P.div
              [ P.className "blocks-body" ]
              $ map (blockRow state) $ blockItems'
            , P.div
              [ P.className "blocks-footer" ]
              [ blocksFooterView ]
          ]
        ]
      where
        expanded = state.viewStates.dashboard.blocksExpanded
        blockItems' :: BlockItems
        blockItems' = if expanded then slice 0 maxBlockRows blockItems else slice 0 minBlockRows blockItems
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
          $ map (blockHeaderItemView state) blocksHeaderItems

blockHeaderItemView :: State -> I18nAccessor -> P.Html Action
blockHeaderItemView state i18nAccessor =
    P.div
        [ P.className "blocks-header__item" ]
        [ P.text $ translate i18nAccessor state.lang ]

blocksHeaderItems :: Array I18nAccessor
blocksHeaderItems =
    [ _.height
    , _.age
    , _.transactions
    , _.totalSent
    , _.relayedBy
    , _.sizeKB
    ]

-- transactions

transactionsView :: State -> P.Html Action
transactionsView state =
    P.div
        [ P.className "explorer-dashboard__wrapper" ]
        [ P.div
          [ P.className "explorer-dashboard__container" ]
          [ P.h3
                [ P.className "headline"]
                [ P.text $ translate _.transactions state.lang ]
          , P.link (toUrl Transaction)
              [ P.className "btn" ]
              [ P.text $ translate _.transaction state.lang ]
          , P.link (toUrl Address)
              [ P.className "btn" ]
              [ P.text $ translate _.address state.lang ]
          , P.link (toUrl Calculator)
              [ P.className "btn" ]
              [ P.text $ translate _.calculator state.lang ]
          ]
        ]
