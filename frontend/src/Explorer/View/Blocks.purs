module Explorer.View.Blocks
    ( blocksView
    , blockRow
    , blocksHeaderView
    , blockHeaderItemView
    , maxBlockRows
    , minBlockRows
    ) where

import Prelude
import Data.Array (length, null, slice)
import Data.DateTime (diff)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (take)
import Data.Time.Duration (Milliseconds)
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (block, blEpochSlotNotFound, cBack2Dashboard, cLoading, cOf, common, cUnknown, cEpoch, cSlot, cAge, cTransactions, cTotalSent, cBlockLead, cSize) as I18nL
import Explorer.Lenses.State (_PageNumber, blocksViewState, blsViewPagination, blsViewPaginationEditable, currentBlocksResult, lang, viewStates)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (minPagination)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CBlockEntries, CCurrency(..), PageNumber(..), State)
import Explorer.Util.Factory (mkEpochIndex)
import Explorer.Util.Time (prettyDuration, nominalDiffTimeToDateTime)
import Explorer.View.CSS (blocksBody, blocksBodyRow, blocksColumnAge, blocksColumnEpoch, blocksColumnLead, blocksColumnSize, blocksColumnSlot, blocksColumnTotalSent, blocksColumnTxs, blocksFailed, blocksFooter, blocksHeader) as CSS
import Explorer.View.Common (currencyCSSClass, getMaxPaginationNumber, noData, paginationView)
import Network.RemoteData (RemoteData(..), withDefault)
import Pos.Explorer.Web.ClientTypes (CBlockEntry(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CCoin, getCoin, cbeBlkHash, cbeEpoch, cbeSlot, cbeBlockLead, cbeSize, cbeTotalSent, cbeTxNum)
import Pux.Html (Html, div, text, span, h3, p) as P
import Pux.Html.Attributes (className, dangerouslySetInnerHTML) as P
import Pux.Router (link) as P

maxBlockRows :: Int
maxBlockRows = 10

minBlockRows :: Int
minBlockRows = 3

blocksView :: State -> P.Html Action
blocksView state =
    let lang' = state ^. lang in
    P.div
        [ P.className "explorer-blocks" ]
        [ P.div
            [ P.className "explorer-blocks__wrapper" ]
            [ P.div
                [ P.className "explorer-blocks__container" ]
                [ P.h3
                      [ P.className "headline" ]
                      [ P.text $
                            (translate (I18nL.common <<< I18nL.cEpoch) lang')
                            <> " / " <>
                            (translate (I18nL.common <<< I18nL.cSlot) lang')
                      ]
                , case state ^. currentBlocksResult of
                      NotAsked  -> emptyBlocksView ""
                      Loading   -> emptyBlocksView $ translate (I18nL.common <<< I18nL.cLoading) lang'
                      Failure _ -> failureView lang'
                      Success blocks ->
                          let paginationViewProps =
                                  { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
                                  , currentPage: state ^. (viewStates <<< blocksViewState <<< blsViewPagination)
                                  , minPage: PageNumber minPagination
                                  , maxPage: PageNumber $ getMaxPaginationNumber (length blocks) maxBlockRows
                                  , changePageAction: BlocksPaginateBlocks
                                  , editable: state ^. (viewStates <<< blocksViewState <<< blsViewPaginationEditable)
                                  , editableAction: BlocksEditBlocksPageNumber
                                  , invalidPageAction: BlocksInvalidBlocksPageNumber
                                  , disabled: false
                                  }
                          in
                          P.div
                              []
                              [ blocksHeaderView blocks lang'
                              , P.div
                                  [ P.className CSS.blocksBody ]
                                  $ map (blockRow state) (currentBlocks state)
                              , P.div
                                  [ P.className CSS.blocksFooter ]
                                  [ paginationView paginationViewProps ]
                              ]
                  ]
            ]
        ]



emptyBlocksView :: String -> P.Html Action
emptyBlocksView message =
    P.div
        [ P.className "blocks-message"
        , P.dangerouslySetInnerHTML message ]
        []

failureView :: Language -> P.Html Action
failureView lang =
    P.div
        []
        [ P.p
            [ P.className CSS.blocksFailed ]
            [ P.text $ translate (I18nL.block <<< I18nL.blEpochSlotNotFound) lang ]
        , P.link (toUrl Dashboard)
            [ P.className "btn-back" ]
            [ P.text $ translate (I18nL.common <<< I18nL.cBack2Dashboard) lang ]
        ]

currentBlocks :: State -> CBlockEntries
currentBlocks state =
    slice minBlockIndex (minBlockIndex + maxBlockRows) blocks
    where
        blocks = withDefault [] $ state ^. currentBlocksResult
        currentBlockPage = state ^. (viewStates <<< blocksViewState <<< blsViewPagination <<< _PageNumber)
        minBlockIndex = (currentBlockPage - 1) * maxBlockRows

blockRow :: State -> CBlockEntry -> P.Html Action
blockRow state (CBlockEntry entry) =
    P.div
        [ P.className CSS.blocksBodyRow ]
        [ blockColumn { label: show $ entry ^. cbeEpoch
                      , mRoute: Just <<< Epoch <<< mkEpochIndex $ entry ^. cbeEpoch
                      , clazz: CSS.blocksColumnEpoch
                      , mCurrency: Nothing
                      }
        , blockColumn { label: show $ entry ^. cbeSlot
                      , mRoute: Just <<< Block $ entry ^. cbeBlkHash
                      , clazz: CSS.blocksColumnSlot
                      , mCurrency: Nothing
                      }
        , blockColumn { label: labelAge
                      , mRoute: Nothing
                      , clazz: CSS.blocksColumnAge
                      , mCurrency: Nothing
                      }
        , blockColumn { label: show $ entry ^. cbeTxNum
                      , mRoute: Nothing
                      , clazz: CSS.blocksColumnTxs
                      , mCurrency: Nothing
                      }
        , blockColumn { label: entry ^. (cbeTotalSent <<< _CCoin <<< getCoin)
                      , mRoute: Nothing
                      , clazz: CSS.blocksColumnTotalSent
                      , mCurrency: Just ADA
                      }
        , blockColumn { label: labelBlockLead
                      , mRoute: Nothing
                      , clazz: CSS.blocksColumnLead
                      , mCurrency: Nothing
                      }
        , blockColumn { label: show $ entry ^. cbeSize
                      , mRoute: Nothing
                      , clazz: CSS.blocksColumnSize
                      , mCurrency: Nothing
                      }
        ]
    where
        language = state ^. lang
        labelAge = fromMaybe noData $ (prettyDuration language :: Milliseconds -> String) <<< diff state.now <$> (nominalDiffTimeToDateTime  =<< entry.cbeTimeIssued)
        labelBlockLead = fromMaybe (translate (I18nL.common <<< I18nL.cUnknown) language)
                            $ take 7 <$> (entry ^. cbeBlockLead)


type BlockColumnProps =
    { label :: String
    , clazz :: String
    , mCurrency :: Maybe CCurrency
    , mRoute :: Maybe Route
    }

blockColumn :: BlockColumnProps -> P.Html Action
blockColumn props =
    let tag = case props.mRoute of
                  Just route -> P.link (toUrl route)
                  Nothing -> P.div
    in
    tag
        [ P.className props.clazz ]
        if isJust props.mCurrency
        then
        [ P.span
          [ P.className $ currencyCSSClass props.mCurrency ]
          [ P.text props.label ]
        ]
        else
        [ P.text props.label ]

type BlocksHeaderProps =
    { label :: String
    , clazz :: String
    }

mkBlocksHeaderProps :: Language -> Array BlocksHeaderProps
mkBlocksHeaderProps lang =
    [ { label: translate (I18nL.common <<< I18nL.cEpoch) lang
      , clazz: CSS.blocksColumnEpoch
      }
    , { label: translate (I18nL.common <<< I18nL.cSlot) lang
      , clazz: CSS.blocksColumnSlot
      }
    , { label: translate (I18nL.common <<< I18nL.cAge) lang
      , clazz: CSS.blocksColumnAge
      }
    , { label: translate (I18nL.common <<< I18nL.cTransactions) lang
      , clazz: CSS.blocksColumnTxs
      }
    , { label: translate (I18nL.common <<< I18nL.cTotalSent) lang
      , clazz: CSS.blocksColumnTotalSent
      }
    , { label: translate (I18nL.common <<< I18nL.cBlockLead) lang
      , clazz: CSS.blocksColumnLead
      }
    , { label: translate (I18nL.common <<< I18nL.cSize) lang
      , clazz: CSS.blocksColumnSize
      }
    ]

blocksHeaderView :: CBlockEntries -> Language -> P.Html Action
blocksHeaderView blocks lang =
    P.div
          [ P.className $ CSS.blocksHeader
                <>  if null blocks then " hide" else ""
          ]
          <<< map blockHeaderItemView $ mkBlocksHeaderProps lang

blockHeaderItemView :: BlocksHeaderProps -> P.Html Action
blockHeaderItemView props =
    P.div
        [ P.className props.clazz ]
        [ P.text props.label ]
