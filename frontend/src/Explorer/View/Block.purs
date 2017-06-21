module Explorer.View.Block (blockView) where

import Prelude

import Data.Array (length, null, slice)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), isJust)

import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cBlock, blSlotNotFound, common, cBack2Dashboard, cOf, cLoading, cNotAvailable, block, blFees, blRoot, blNextBlock, blPrevBlock, blEstVolume, cHash, cSummary, cTotalOutput, cHashes, cSlot, cTransactions, tx, txNotFound, txEmpty) as I18nL
import Explorer.Lenses.State (_PageNumber, blockDetail, blockTxPagination, blockTxPaginationEditable, currentBlockSummary, currentBlockTxs, lang, viewStates)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (minPagination)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), CTxBriefs, PageNumber(..), State)
import Explorer.Util.Factory (mkCoin)
import Explorer.Util.String (formatADA)
import Explorer.View.Common (currencyCSSClass, emptyView, getMaxPaginationNumber, mkTxBodyViewProps, mkTxHeaderViewProps, txBodyView, txEmptyContentView, txHeaderView, txPaginationView)

import Network.RemoteData (RemoteData(..), isFailure)

import Pos.Explorer.Web.ClientTypes (CBlockEntry(..), CBlockSummary(..), CTxBrief)
import Pos.Explorer.Web.Lenses.ClientTypes (_CBlockEntry, _CBlockSummary, _CHash, cbeBlkHash, cbeSlot, cbeTotalSent, cbeTxNum, cbsEntry, cbsMerkleRoot, cbsNextHash, cbsPrevHash)

import Pux.DOM.HTML (HTML) as P
import Pux.DOM.Events (onClick) as P

import Text.Smolder.HTML (div, h3, span)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (#!), (!))



blockView :: State -> P.HTML Action
blockView state =
    let lang' = state ^. lang
        blockSummary = state ^. currentBlockSummary
        blockTxs = state ^. currentBlockTxs
    in
    div ! className "explorer-block" $ do
        div ! className "explorer-block__wrapper"
            $ div ! className "explorer-block__container"  $ do
                  h3  ! className "headline"
                      $ text (translate (I18nL.common <<< I18nL.cBlock) lang')
                  case blockSummary of
                      NotAsked -> blockSummaryEmptyView ""
                      Loading -> blockSummaryEmptyView $ translate (I18nL.common <<< I18nL.cLoading) lang'
                      Failure _ -> blockSummaryEmptyView $ translate (I18nL.block <<< I18nL.blSlotNotFound) lang'
                      Success block -> blockSummaryView block lang'
        div ! className "explorer-block__wrapper" $ do
            div ! className "explorer-block__container" $ do
                h3 ! className "headline"
                   $ text (translate (I18nL.common <<< I18nL.cSummary) lang')
                case blockTxs of
                    NotAsked -> txEmptyContentView ""
                    Loading -> txEmptyContentView $ translate (I18nL.common <<< I18nL.cLoading) lang'
                    Failure _ -> txEmptyContentView $ translate (I18nL.tx <<< I18nL.txNotFound) lang'
                    Success txs -> blockTxsView txs state
            if (isFailure blockSummary && isFailure blockTxs)
                -- Show back button if both results ^ are failed
                then
                    div ! className "explorer-block__container"
                        $ a ! href (toUrl Dashboard)
                            #! onClick (toUrl Dashboard)
                            !className "btn-back"
                            $ text (translate (I18nL.common <<< I18nL.cBack2Dashboard) lang')
                else
                    emptyView

--  summary

type SummaryRowItem =
    { label :: String
    , amount :: String
    , mCurrency :: Maybe CCurrency
    }

type SummaryItems = Array SummaryRowItem

mkSummaryItems :: Language -> CBlockEntry -> SummaryItems
mkSummaryItems lang (CBlockEntry entry) =
    [ { label: translate (I18nL.common <<< I18nL.cTransactions) lang
      , amount: show $ entry ^. cbeTxNum
      , mCurrency: Nothing
      }
    , { label: translate (I18nL.common <<< I18nL.cTotalOutput) lang
      , amount: formatADA (entry ^. cbeTotalSent) lang
      , mCurrency: Just ADA
      }
    , { label: translate (I18nL.block <<< I18nL.blEstVolume) lang
      -- TODO: We do need real data here
      , amount: formatADA (mkCoin "0") lang
      , mCurrency: Just ADA
      }
    , { label: translate (I18nL.block <<< I18nL.blFees) lang
      -- TODO: We do need real data here
      , amount: formatADA (mkCoin "0") lang
      , mCurrency: Just ADA
      }
    , { label: translate (I18nL.common <<< I18nL.cSlot) lang
      , amount: show $ entry ^. cbeSlot
      , mCurrency: Nothing
      }
    ]

summaryRow :: SummaryRowItem -> P.HTML Action
summaryRow item =
    div ! className "row row__summary" $ do
        div ! className "column column__label"
            $ text item.label
        div ! className $ "column column__amount"
            $ if isJust item.mCurrency
                  then span ! className (currencyCSSClass item.mCurrency)
                            $ text item.amount
                  else text item.amount

blockSummaryEmptyView :: String -> P.HTML Action
blockSummaryEmptyView message =
    div ! className "summary-empty__container"
        $ text message

blockSummaryView :: CBlockSummary -> Language -> P.HTML Action
blockSummaryView block lang =
    div ! className "blocks-wrapper" $ do
        div ! className "summary-container" $ do
            h3  ! className "subheadline"
                $ text (translate (I18nL.common <<< I18nL.cSummary) lang)
            div $ map summaryRow <<< mkSummaryItems lang $ block ^. (_CBlockSummary <<< cbsEntry)
        div ! className "hashes-container" $ do
            h3  ! className "subheadline"
                $ text (translate (I18nL.common <<< I18nL.cHashes) lang)
            div $ map hashesRow $ mkHashItems lang block

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


hashesRow :: HashRowItem -> P.HTML Action
hashesRow item =
    div ! className "row row__hashes" $ do
        div ! className "column column__label"
            $ text item.label
        case item.link of
            Nothing ->  div ! className "column column__hash"
                            $ text item.hash
            Just link -> a  ! href link
                            #! onClick link
                            ! className "column column__hash--link"
                            $ text item.hash

maxTxRows :: Int
maxTxRows = 5

blockTxsView :: CTxBriefs -> State -> P.HTML Action
blockTxsView txs state =
    if null txs then
        txEmptyContentView $ translate (I18nL.tx <<< I18nL.txEmpty) (state ^. lang)
    else
        let txPagination = state ^. (viewStates <<< blockDetail <<< blockTxPagination <<< _PageNumber)
            lang' = state ^. lang
            minTxIndex = (txPagination - minPagination) * maxTxRows
            currentTxs = slice minTxIndex (minTxIndex + maxTxRows) txs
        in
        div do
            div $ map (\tx -> blockTxView tx lang') currentTxs
            txPaginationView  { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
                              , currentPage: PageNumber txPagination
                              , minPage: PageNumber minPagination
                              , maxPage: PageNumber $ getMaxPaginationNumber (length txs) maxTxRows
                              , changePageAction: BlockPaginateTxs
                              , editable: state ^. (viewStates <<< blockDetail <<< blockTxPaginationEditable)
                              , editableAction: BlockEditTxsPageNumber
                              , invalidPageAction: BlockInvalidTxsPageNumber
                              , disabled: false
                              }

blockTxView :: CTxBrief -> Language -> P.HTML Action
blockTxView tx lang =
    div do
        txHeaderView lang $ mkTxHeaderViewProps tx
        txBodyView lang $ mkTxBodyViewProps tx
