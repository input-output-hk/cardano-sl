module Explorer.View.Block (blockView) where

import Prelude
import Data.Array (length, null, (!!))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cBlock, common, cOf, cNotAvailable, block, blFees, blRoot, blNextBlock, blPrevBlock, blEstVolume, cHash, cSummary, cTotalOutput, cHashes, cSlot, cTransactions) as I18nL
import Explorer.Lenses.State (blockDetail, blockTxPagination, blockTxPaginationEditable, currentBlockSummary, currentBlockTxs, lang, viewStates)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (minPagination)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), State)
import Explorer.View.Common (currencyCSSClass, mkEmptyViewProps, mkTxBodyViewProps, mkTxHeaderViewProps, txBodyView, txEmptyContentView, txHeaderView, txPaginationView)
import Pos.Explorer.Web.ClientTypes (CBlockEntry(..), CBlockSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CCoin, getCoin, _CBlockEntry, _CBlockSummary, _CHash, cbeBlkHash, cbeSlot, cbeTotalSent, cbeTxNum, cbsEntry, cbsMerkleRoot, cbsNextHash, cbsPrevHash)
import Pux.Html (Html, div, text, h3) as P
import Pux.Html.Attributes (className) as P
import Pux.Router (link) as P



blockView :: State -> P.Html Action
blockView state =
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
                    , blockSummaryView (state ^. currentBlockSummary) lang'
                  ]

            ]
        , P.div
            [ P.className "explorer-block__wrapper" ]
            [ P.div
                [ P.className "explorer-block__container" ]
                [ P.h3
                      [ P.className "headline"]
                      [ P.text $ translate (I18nL.common <<< I18nL.cSummary) lang' ]
                , case state ^. currentBlockTxs of
                      Nothing -> txEmptyContentView lang'
                      Just blockTxs ->
                          if null blockTxs then
                              txEmptyContentView lang'
                          else
                              let txPagination = state ^. (viewStates <<< blockDetail <<< blockTxPagination)
                                  currentTxBrief = blockTxs !! (txPagination - 1)
                              in
                              P.div
                                  []
                                  [ txHeaderView lang' $ case currentTxBrief of
                                                              Nothing -> mkTxHeaderViewProps mkEmptyViewProps
                                                              Just txBrief -> mkTxHeaderViewProps txBrief
                                  , txBodyView $ case currentTxBrief of
                                                      Nothing -> mkTxBodyViewProps mkEmptyViewProps
                                                      Just txBrief -> mkTxBodyViewProps txBrief
                                  , txPaginationView  { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
                                                      , currentPage: txPagination
                                                      , minPage: minPagination
                                                      , maxPage: length blockTxs
                                                      , changePageAction: BlockPaginateTxs
                                                      , editable: state ^. (viewStates <<< blockDetail <<< blockTxPaginationEditable)
                                                      , editableAction: BlockEditTxsPageNumber
                                                      , invalidPageAction: BlockInvalidTxsPageNumber
                                                      }
                                  ]
                ]
            ]
        ]

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
      , amount: entry ^. (cbeTotalSent <<< _CCoin <<< getCoin)
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
    , { label: translate (I18nL.common <<< I18nL.cSlot) lang
      , amount: show $ entry ^. cbeSlot
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

blockSummaryView :: Maybe CBlockSummary -> Language -> P.Html Action
blockSummaryView Nothing _ =
    P.div
        [ P.className "blocks-wrapper" ]
        [P.text "" ]
blockSummaryView (Just block) lang =
    P.div
      [ P.className "blocks-wrapper" ]
      [ P.div
        -- summary
        [ P.className "summary-container" ]
        [ P.h3
          [ P.className "subheadline" ]
          [ P.text $ translate (I18nL.common <<< I18nL.cSummary) lang ]
        , P.div
            []
            <<< map summaryRow <<< mkSummaryItems lang $ block ^. (_CBlockSummary <<< cbsEntry)
        ]-- hashes
      , P.div
        [ P.className "hashes-container" ]
        [ P.h3
            [ P.className "subheadline" ]
            [ P.text $ translate (I18nL.common <<< I18nL.cHashes) lang ]
        , P.div
            []
            <<< map hashesRow $ mkHashItems lang block
        ]
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
