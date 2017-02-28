module Explorer.View.Block (blockView) where

import Prelude
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cBlock, common, cOf, cNotAvailable, block, blFees, blRoot, blNextBlock, blPrevBlock, blEstVolume, cHash, cSummary, cTotalOutput, cHashes, cHeight, cTransactions) as I18nL
import Explorer.Lenses.State (lang, latestBlock)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), State)
import Explorer.Util.DOM (targetToHTMLInputElement)
import Explorer.View.Common (currencyCSSClass, transactionPaginationView, transactionHeaderView, transactionBodyView)
import Pos.Explorer.Web.ClientTypes (CBlockEntry(..), CBlockSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CBlockEntry, _CBlockSummary, _CHash, cbeBlkHash, cbeHeight, cbeTotalSent, cbeTxNum, cbsEntry, cbsMerkleRoot, cbsNextHash, cbsPrevHash)
import Pos.Types.Lenses.Core (_Coin, getCoin)
import Pux.Html (Html, div, text, h3) as P
import Pux.Html.Attributes (className) as P
import Pux.Router (link) as P



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
                      [ P.text $ translate (I18nL.common <<< I18nL.cBlock) lang' ]
                    , case block of
                      Nothing -> P.div [] [P.text "block not found" ]
                      Just block' ->
                          P.div
                            [ P.className "blocks-wrapper" ]
                            [ P.div
                              -- summary
                              [ P.className "summary-container" ]
                              [ P.h3
                                [ P.className "subheadline" ]
                                [ P.text $ translate (I18nL.common <<< I18nL.cSummary) lang' ]
                              , P.div
                                  []
                                  <<< map summaryRow <<< mkSummaryItems lang' $ block' ^. (_CBlockSummary <<< cbsEntry)
                              ]-- hashes
                            , P.div
                              [ P.className "hashes-container" ]
                              [ P.h3
                                  [ P.className "subheadline" ]
                                  [ P.text $ translate (I18nL.common <<< I18nL.cHashes) lang' ]
                              , P.div
                                  []
                                  <<< map hashesRow $ mkHashItems lang' block'
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
                , transactionPaginationView paginationViewProps
                ]
            ]
        ]
        where
            lang' = state ^. lang
            block = state ^. latestBlock
            paginationViewProps =
                { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
                , currentPage: 1
                , maxPage: 1
                , changePageAction: BlockPaginateTransactions
                , onFocusAction: SelectInputText <<< targetToHTMLInputElement
                }

--  summary

type SummaryRowItem =
    { label :: String
    , amount :: String
    , currency :: Maybe CCurrency
    }

type SummaryItems = Array SummaryRowItem

mkSummaryItems :: Language -> CBlockEntry -> SummaryItems
mkSummaryItems lang (CBlockEntry entry) =
    [ { label: translate (I18nL.common <<< I18nL.cTransactions) lang
      , amount: show $ entry ^. cbeTxNum
      , currency: Nothing
      }
    , { label: translate (I18nL.common <<< I18nL.cTotalOutput) lang
      , amount: show $ entry ^. (cbeTotalSent <<< _Coin <<< getCoin)
      , currency: Just ADA
      }
    , { label: translate (I18nL.block <<< I18nL.blEstVolume) lang
      , amount: "0"
      , currency: Just ADA
      }
    , { label: translate (I18nL.block <<< I18nL.blFees) lang
      , amount: "0"
      , currency: Just ADA
      }
    , { label: translate (I18nL.common <<< I18nL.cHeight) lang
      , amount: show $ entry ^. cbeHeight
      , currency: Nothing
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

type HashItems = Array HashRowItem

type HashRowItem =
    { label :: String
    , hash :: String
    , link :: Maybe String
    }

mkHashItems :: Language -> CBlockSummary -> HashItems
mkHashItems lang (CBlockSummary blockSummery) =
    [ { label: translate (I18nL.common <<< I18nL.cHash) lang
      , hash: blockSummery ^. (cbsEntry <<< _CBlockEntry <<< cbeBlkHash <<< _CHash)
      , link: Nothing
      }
    , { label: translate (I18nL.block <<< I18nL.blPrevBlock) lang
      , hash: blockSummery ^. (cbsPrevHash <<< _CHash)
      , link: Just <<< toUrl <<< Block $ blockSummery ^. cbsPrevHash
      }
    , { label: translate (I18nL.block <<< I18nL.blNextBlock) lang
      , hash: case blockSummery ^. cbsNextHash of
          Nothing -> translate (I18nL.common <<< I18nL.cNotAvailable) lang
          Just hash -> hash ^. _CHash
      , link: case blockSummery ^. cbsNextHash of
          Nothing -> Nothing
          Just hash -> Just <<< toUrl $ Block hash
      }
    , { label: translate (I18nL.block <<< I18nL.blRoot) lang
      , hash: blockSummery ^. (cbsMerkleRoot <<< _CHash)
      , link: Nothing
      }
    ]


hashesRow :: HashRowItem -> P.Html Action
hashesRow item =
    P.div
        [ P.className "row row__hashes" ]
        [ P.div
            [ P.className "column column__label" ]
            [ P.text item.label ]
        , case item.link of
              Nothing ->  P.div
                              [ P.className $ "column column__hash" ]
                              [ P.text item.hash ]
              Just link -> P.link link
                              [ P.className $ "column column__hash--link" ]
                              [ P.text item.hash ]
        ]
