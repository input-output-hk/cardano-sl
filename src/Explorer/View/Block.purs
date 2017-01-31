module Explorer.View.Block (blockView) where

import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (translateL)
import Explorer.I18n.Lenses (block, summary, hashes) as I18nL
import Explorer.Types.Actions (Action)
import Explorer.Types.Generated (CCurrency(..))
import Explorer.Types.State (State)
import Explorer.View.Common (currencyCSSClass, transactionPaginationView
    , transactionHeaderView, transactionBodyView)
import Prelude ((<>), ($), map)
import Pux.Html (Html, div, text, h3) as P
import Pux.Html.Attributes (className) as P



blockView :: State -> P.Html Action
blockView state =
    P.div
        [ P.className "explorer-block" ]
        [ P.div
              [ P.className "explorer-block__wrapper" ]
              [ P.div
                    [ P.className "explorer-block__container" ]
                    [ P.h3
                            [ P.className "headline"]
                            [ P.text $ translateL I18nL.block state.lang ]
                      , P.div
                          [ P.className "blocks-wrapper" ]
                          [ P.div
                          -- summary
                              [ P.className "summary-container" ]
                              [ P.h3
                                  [ P.className "subheadline" ]
                                  [ P.text $ translateL I18nL.summary state.lang ]
                                , P.div
                                    []
                                    $ map summaryRow summaryItems
                              ]
                          -- hashes
                          , P.div
                              [ P.className "hashes-container" ]
                              [ P.h3
                                  [ P.className "subheadline" ]
                                  [ P.text $ translateL I18nL.hashes state.lang ]
                              , P.div
                                  []
                                  $ map hashesRow hashItems
                              ]
                          ]
                    ]

              ]
          , P.div
                [ P.className "explorer-block__wrapper" ]
                [ P.div
                    [ P.className "explorer-block__container" ]
                    [ P.h3
                        [ P.className "headline"]
                        [ P.text $ translateL I18nL.summary state.lang ]
                    , transactionHeaderView state
                    , transactionBodyView state
                    , transactionPaginationView state
                    ]
                ]
        ]

--  summary

-- FIXME (jk): just for now, will use later `real` ADTs
type SummaryRowItem =
    { label :: String
    , amount :: String
    , currency :: Maybe CCurrency
    }

-- FIXME (jk): just for now, will use later `real` ADTs
type SummaryItems = Array SummaryRowItem

summaryItems :: SummaryItems
summaryItems =
    [ { label: "#Transactions", amount: "2564", currency: Nothing }
    , { label: "#Output Total", amount: "3,042,900", currency: Just ADA }
    , { label: "#Est. Volume", amount: "2,845", currency: Just ADA }
    , { label: "#Fees", amount: "10", currency: Just ADA }
    ]

summaryRow :: SummaryRowItem -> P.Html Action
summaryRow item =
    P.div
        [ P.className "row row__summary" ]
        [ P.div
            [ P.className "column column__label" ]
            [ P.text item.label ]
        , P.div
              [ P.className $ "column column__amount" <> currencyCSSClass item.currency ]
              [ P.text item.amount ]
        ]

-- hashes

-- FIXME (jk): just for now, will use later `real` ADTs
type HashItems = Array HashRowItem

-- FIXME (jk): just for now, will use later `real` ADTs
type HashRowItem =
    { label :: String
    , hash :: String -- TODO (jk) Use Hash type which has tbd
    }

hashItems :: HashItems
hashItems =
    [ { label: "#Hash", hash: "fb1817c0fadeb1efae09b1308…ac4361af0ea7" }
    , { label: "#Previuos Block", hash: "eb1efae09b1308fb1817c0fad…af0ea7ac4361" }
    , { label: "#Next Block", hash: "9b1308fb1eb1efae0817c0fad…fjs393hf93h83" }
    , { label: "#Merkle Root", hash: "25c48508b1a444c1508…64260f2a14279d09" }
    ]


hashesRow :: HashRowItem -> P.Html Action
hashesRow item =
    P.div
        [ P.className "row row__hashes" ]
        [ P.div
            [ P.className "column column__label" ]
            [ P.text item.label ]
        , P.div
              [ P.className $ "column column__hash" ]
              [ P.text item.hash ]
        ]
