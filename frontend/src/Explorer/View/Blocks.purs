module Explorer.View.Blocks
    ( blocksView
    , blockRow
    , blocksHeaderView
    , blockHeaderItemView
    , maxBlockRows
    , minBlockRows
    ) where

import Prelude
import Control.Monad.Eff.Exception (Error)
import Data.Array (length, null, slice)
import Data.DateTime (diff)
import Data.Lens ((^.))
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds)
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (block, blNotFound, cBack2Dashboard, cLoading, cOf, common, cUnknown, cEpoch, cSlot, cAge, cTransactions, cTotalSent, cSizeKB) as I18nL
import Explorer.Lenses.State (blocksViewState, blsViewPagination, blsViewPaginationEditable, currentBlocksResult, lang, viewStates)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (minPagination)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State, CBlockEntries)
import Explorer.Util.Time (prettyDuration, nominalDiffTimeToDateTime)
import Explorer.View.CSS (blocksBody, blocksBodyRow, blocksColumnAge, blocksColumnEpoch, blocksColumnSize, blocksColumnSlot, blocksColumnTotalSent, blocksColumnTxs, blocksFailed, blocksFooter, blocksHeader) as CSS
import Explorer.View.Common (getMaxPaginationNumber, noData, paginationView)
import Network.RemoteData (RemoteData(..), withDefault)
import Pos.Explorer.Web.ClientTypes (CBlockEntry(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CCoin, getCoin, cbeBlkHash, cbeEpoch, cbeSlot, cbeRelayedBy, cbeSize, cbeTotalSent, cbeTxNum)
import Pux.Html (Html, div, text, h3, p) as P
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
                                  , minPage: minPagination
                                  , maxPage: getMaxPaginationNumber (length blocks) maxBlockRows
                                  , changePageAction: BlocksPaginateBlocks
                                  , editable: state ^. (viewStates <<< blocksViewState <<< blsViewPaginationEditable)
                                  , editableAction: BlocksEditBlocksPageNumber
                                  , invalidPageAction: BlocksInvalidBlocksPageNumber
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
            [ P.text $ translate (I18nL.block <<< I18nL.blNotFound) lang ]
        , P.link (toUrl Dashboard)
            [ P.className "btn-back" ]
            [ P.text $ translate (I18nL.common <<< I18nL.cBack2Dashboard) lang ]
        ]

currentBlocks :: State -> CBlockEntries
currentBlocks state =
    slice minBlockIndex (minBlockIndex + maxBlockRows) blocks
    where
        blocks = withDefault [] $ state ^. currentBlocksResult
        currentBlockPage = state ^. (viewStates <<< blocksViewState <<< blsViewPagination)
        minBlockIndex = (currentBlockPage - 1) * maxBlockRows

blockRow :: State -> CBlockEntry -> P.Html Action
blockRow state (CBlockEntry entry) =
    P.link (toUrl <<< Block $ entry ^. cbeBlkHash)
        [ P.className CSS.blocksBodyRow ]
        [ blockColumn { label: show $ entry ^. cbeEpoch
                      , clazz: CSS.blocksColumnEpoch
                      }
        , blockColumn { label: show $ entry ^. cbeSlot
                      , clazz: CSS.blocksColumnSlot
                      }
        , blockColumn { label: labelAge
                      , clazz: CSS.blocksColumnAge
                      }
        , blockColumn { label: show $ entry ^. cbeTxNum
                      , clazz: CSS.blocksColumnTxs
                      }
        , blockColumn { label: entry ^. (cbeTotalSent <<< _CCoin <<< getCoin)
                      , clazz: CSS.blocksColumnTotalSent
                      }
--        , blockColumn { label: labelRelayed
--                      , clazz: CSS.blocksColumnRelayedBy
--                      }
        , blockColumn { label: show $ entry ^. cbeSize
                      , clazz: CSS.blocksColumnSize
                      }
        ]
    where
        language = state ^. lang
        labelAge = fromMaybe noData $ (prettyDuration language :: Milliseconds -> String) <<< diff state.now <$> (nominalDiffTimeToDateTime  =<< entry.cbeTimeIssued)
        labelRelayed = fromMaybe (translate (I18nL.common <<< I18nL.cUnknown) language)
                            $ entry ^. cbeRelayedBy


type BlockColumnProps =
    { label :: String
    , clazz :: String
    }

blockColumn :: BlockColumnProps -> P.Html Action
blockColumn props =
    P.div
        [ P.className props.clazz ]
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
    -- , { label: translate (I18nL.common <<< I18nL.cRelayedBy) lang
    --   , clazz: CSS.blocksColumnRelayedBy
    --   }
    , { label: translate (I18nL.common <<< I18nL.cSizeKB) lang
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
