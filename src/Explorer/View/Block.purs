module Explorer.View.Block (blockView) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Lens ((^.))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cBlock, common, block, blFees, blRoot, blNextBlock, blPrevBlock, blEstVolume, cHash, cSummary, cTotalOutput, cHashes, cTransactions) as I18nL
import Explorer.Types.Actions (Action)
import Explorer.Types.State (CCurrency(..), State)
import Explorer.Lenses.State (lang)
import Explorer.View.Common (currencyCSSClass, transactionPaginationView, transactionHeaderView, transactionBodyView)
import Pos.Explorer.Web.ClientTypes (CHash)
import Pux.Html (Html, div, text, h3) as P
import Pux.Html.Attributes (className) as P



blockView :: State -> CHash -> P.Html Action
blockView state hash =
    let lang' = state ^. lang in
    P.div
        [ P.className "explorer-block" ]
        [ P.div
              [ P.className "explorer-block__wrapper" ]
              [ P.div
                    [ P.className "explorer-block__container" ]
                    [ P.h3
                            [ P.className "headline"]
                            [ P.text $ translate (I18nL.common <<< I18nL.cBlock) lang' ]
                      , P.div
                          [ P.className "blocks-wrapper" ]
                          [ P.div
                          -- summary
                              [ P.className "summary-container" ]
                              [ P.h3
                                  [ P.className "subheadline" ]
                                  [ P.text $ translate (I18nL.common <<< I18nL.cSummary) lang' ]
                                , P.div
                                    []
                                    <<< map summaryRow $ summaryItems lang'
                              ]
                          -- hashes
                          , P.div
                              [ P.className "hashes-container" ]
                              [ P.h3
                                  [ P.className "subheadline" ]
                                  [ P.text $ translate (I18nL.common <<< I18nL.cHashes) lang' ]
                              , P.div
                                  []
                                  <<< map hashesRow $ hashItems lang'
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
                        [ P.text $ translate (I18nL.common <<< I18nL.cSummary) lang' ]
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

summaryItems :: Language -> SummaryItems
summaryItems lang =
    [ { label: translate (I18nL.common <<< I18nL.cTransactions) lang
      , amount: "2564"
      , currency: Nothing
      }
    , { label: translate (I18nL.common <<< I18nL.cTotalOutput) lang
      , amount: "3,042,900"
      , currency: Just ADA
      }
    , { label: translate (I18nL.block <<< I18nL.blEstVolume) lang
      , amount: "2,845"
      , currency: Just ADA
      }
    , { label: translate (I18nL.block <<< I18nL.blFees) lang
      , amount: "10"
      , currency: Just ADA
      }
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

hashItems :: Language -> HashItems
hashItems lang =
    [ { label: translate (I18nL.common <<< I18nL.cHash) lang
      , hash: "fb1817c0fadeb1efae09b1308…ac4361af0ea7"
      }
    , { label: translate (I18nL.block <<< I18nL.blPrevBlock) lang
      , hash: "eb1efae09b1308fb1817c0fad…af0ea7ac4361"
      }
    , { label: translate (I18nL.block <<< I18nL.blNextBlock) lang
      , hash: "9b1308fb1eb1efae0817c0fad…fjs393hf93h83"
      }
    , { label: translate (I18nL.block <<< I18nL.blRoot) lang
      , hash: "25c48508b1a444c1508…64260f2a14279d09"
      }
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
