module Explorer.View.Common (
    placeholderView
    , txHeaderView
    , txBodyView
    , emptyTxBodyView
    , emptyTxHeaderView
    , getMaxPaginationNumber
    , mkTxHeaderViewProps
    , class TxHeaderViewPropsFactory
    , mkTxBodyViewProps
    , class TxBodyViewPropsFactory
    , currencyCSSClass
    , paginationView
    , txPaginationView
    , EmptyViewProps
    , mkEmptyViewProps
    , txEmptyContentView
    , noData
    ) where

import Prelude
import Data.Int (ceil, fromString, toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (common, cDateFormat, tx, txEmpty) as I18nL
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..))
import Explorer.Util.Factory (mkCAddress, mkCTxId, mkCoin, sumCoinOfInputsOutputs)
import Explorer.Util.Time (prettyDate)
import Explorer.View.Lenses (txbAmount, txbInputs, txbOutputs, txhAmount, txhHash, txhTimeIssued)
import Exporer.View.Types (TxBodyViewProps(..), TxHeaderViewProps(..))
import Pos.Core.Lenses.Types (getCoin)
import Pos.Core.Types (Coin(..))
import Pos.Explorer.Web.ClientTypes (CAddress(..), CTxBrief(..), CTxEntry(..), CTxSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CHash, _CTxId, ctbId, ctbInputs, ctbOutputs, ctbTimeIssued, cteId, cteTimeIssued, ctsBlockTimeIssued, ctsId, ctsInputs, ctsOutputs, ctsTotalOutput)
import Pux.Html (Html, text, div, p, span, input) as P
import Pux.Html.Attributes (className, value, disabled, type_, min, max) as P
import Pux.Html.Events (onChange, onFocus, FormEvent, MouseEvent, Target, onClick) as P
import Pux.Router (link) as P

-- -----------------
-- tx header
-- -----------------

-- | Factory to create TxHeaderViewProps by a given type
class TxHeaderViewPropsFactory a where
    mkTxHeaderViewProps :: a -> TxHeaderViewProps

-- | Creates a TxHeaderViewProps by a given CTxEntry
instance cTxEntryTxHeaderViewPropsFactory :: TxHeaderViewPropsFactory CTxEntry where
    mkTxHeaderViewProps (CTxEntry entry) = TxHeaderViewProps
        { txhHash: entry ^. cteId
        , txhTimeIssued: Just $ entry ^. cteTimeIssued
        , txhAmount: entry . cteAmount
        }

-- | Creates a TxHeaderViewProps by a given CTxBrief
instance cTxBriefTxHeaderViewPropsFactory :: TxHeaderViewPropsFactory CTxBrief where
    mkTxHeaderViewProps (CTxBrief txBrief) = TxHeaderViewProps
        { txhHash: txBrief ^. ctbId
        , txhTimeIssued: Just $ txBrief ^. ctbTimeIssued
        , txhAmount: sumCoinOfInputsOutputs $ txBrief ^. ctbOutputs
        }

-- | Creates a TxHeaderViewProps by a given CTxSummary
instance cTxSummaryTxHeaderViewPropsFactory :: TxHeaderViewPropsFactory CTxSummary where
    mkTxHeaderViewProps (CTxSummary txSummary) = TxHeaderViewProps
        { txhHash: txSummary ^. ctsId
        , txhTimeIssued: txSummary ^. ctsBlockTimeIssued
        , txhAmount: txSummary ^. ctsTotalOutput
        }

-- | Creates a TxHeaderViewProps by a given EmptyViewProps
instance emtpyTxHeaderViewPropsFactory :: TxHeaderViewPropsFactory EmptyViewProps where
    mkTxHeaderViewProps _ = TxHeaderViewProps
        { txhHash: mkCTxId noData
        , txhTimeIssued: Nothing
        , txhAmount: mkCoin 0
        }

txHeaderView :: Language -> TxHeaderViewProps -> P.Html Action
txHeaderView lang (TxHeaderViewProps props) =
    P.div
          [ P.className "transaction-header"]
          [ P.link (toUrl <<< Tx $ props ^. txhHash)
              [ P.className "hash" ]
              [ P.text $ props ^. (txhHash <<< _CTxId <<< _CHash) ]
          , P.div
              [ P.className "date"]
              [ P.text $ case props ^. txhTimeIssued of
                              Just time ->
                                  let format = translate (I18nL.common <<< I18nL.cDateFormat) lang
                                  in fromMaybe noData $ prettyDate format time
                              Nothing -> noData
              ]
          , txAmountView $ props ^. txhAmount
          ]

emptyTxHeaderView :: P.Html Action
emptyTxHeaderView =
    P.div
        [ P.className "transaction-header"]
        [ ]

txAmountView :: Coin -> P.Html Action
txAmountView (Coin coin) =
    P.div
        [ P.className "amount-container" ]
        [ P.div
            [ P.className "amount bg-ada" ]
            [ P.text <<< show $ coin ^. getCoin]
        ]
-- -----------------
-- tx body
-- -----------------

-- | Factory to create TxBodyViewProps by a given type
class TxBodyViewPropsFactory a where
    mkTxBodyViewProps :: a -> TxBodyViewProps

-- | Creates a TxBodyViewProps by a given CTxSummary
instance cTxSummaryTxBodyViewPropsFactory :: TxBodyViewPropsFactory CTxSummary where
    mkTxBodyViewProps (CTxSummary txSummary) = TxBodyViewProps
        { txbInputs: txSummary ^. ctsInputs
        , txbOutputs: txSummary ^. ctsOutputs
        , txbAmount: txSummary ^. ctsTotalOutput
        }

-- | Creates a TxBodyViewProps by a given CTxBrief
instance cTxBriefTxBodyViewPropsFactory :: TxBodyViewPropsFactory CTxBrief where
    mkTxBodyViewProps (CTxBrief txBrief) = TxBodyViewProps
        { txbInputs: txBrief ^. ctbInputs
        , txbOutputs: txBrief ^. ctbOutputs
        , txbAmount: sumCoinOfInputsOutputs $ txBrief ^. ctbOutputs
        }

-- | Creates a TxBodyViewProps by a given EmptyViewProps
instance emptyTxBodyViewPropsFactory :: TxBodyViewPropsFactory EmptyViewProps where
    mkTxBodyViewProps _ = TxBodyViewProps
        { txbInputs: []
        , txbOutputs: []
        , txbAmount: mkCoin 0
        }

txBodyView :: TxBodyViewProps -> P.Html Action
txBodyView (TxBodyViewProps props) =
    P.div
        [ P.className "transaction-body" ]
        [ P.div
            [ P.className "from-hash-container" ]
            <<< map txFromView $ props ^. txbInputs
        , P.div
            [ P.className "to-hash-container bg-transaction-arrow" ]
            <<< map txToView $ props ^. txbOutputs
        , P.div
              [ P.className "amounts-container" ]
              <<< map txBodyAmountView $ props ^. txbOutputs
        , txAmountView $ props ^. txbAmount
        ]

emptyTxBodyView :: P.Html Action
emptyTxBodyView =
    P.div
        [ P.className "transaction-body" ]
        []

txFromView :: Tuple CAddress Coin -> P.Html Action
txFromView (Tuple (CAddress cAddress) _) =
    P.link (toUrl <<< Address $ mkCAddress cAddress)
        [ P.className "from-hash" ]
        [ P.text cAddress ]

txToView :: Tuple CAddress Coin -> P.Html Action
txToView (Tuple (CAddress cAddress) _) =
    P.link (toUrl <<< Address $ mkCAddress cAddress)
          [ P.className "to-hash"]
          [ P.text cAddress ]

txBodyAmountView :: Tuple CAddress Coin -> P.Html Action
txBodyAmountView (Tuple _ (Coin coin)) =
    P.div
        [ P.className "amount-wrapper" ]
        [ P.span
            [ P.className "plain-amount bg-ada-dark" ]
            [ P.text <<< show $ coin ^. getCoin ]
        ]

-- -----------------
-- pagination
-- -----------------

type PaginationViewProps =
    { label :: String
    , currentPage :: Int
    , maxPage :: Int
    , changePageAction :: (Int -> Action)
    , onFocusAction :: (P.Target -> Action)
    }

txPaginationView :: PaginationViewProps -> P.Html Action
txPaginationView props =
    P.div
        [ P.className "transaction-pagination"]
        [ paginationView props ]

paginationView :: PaginationViewProps -> P.Html Action
paginationView props =
    P.div
        [ P.className "pagination" ]
        [ P.div
            [ P.className "pagination__wrapper" ]
            [ P.div
                [ P.className $ "btn-page" <> disablePrevBtnClazz
                , P.onClick prevClickHandler ]
                [ P.div
                    [ P.className "icon bg-triangle-left" ]
                    []
                ]
            , P.input
                [ P.className "page-number"
                , P.value <<< show $ props.currentPage
                , P.disabled $ props.maxPage == minPage
                , P.min $ show minPage
                , P.max $ show props.maxPage
                , P.onChange changeHandler
                , P.onFocus $ props.onFocusAction <<< _.target
                ]
                []
            , P.p
                [ P.className "label" ]
                [ P.text props.label ]
            , P.input
                [ P.className "page-number"
                , P.disabled true
                , P.type_ "number"
                , P.value $ show props.maxPage
                ]
                []
            , P.div
                [ P.className $ "btn-page" <> disableNextBtnClazz
                  , P.onClick nextClickHandler ]
                [ P.div
                    [ P.className "icon bg-triangle-right" ]
                    []
                ]
            ]
        ]
        where
          minPage = 1
          disablePrevBtnClazz = if props.currentPage == minPage then " disabled" else ""
          disableNextBtnClazz = if props.currentPage == props.maxPage then " disabled" else ""
          nextClickHandler :: P.MouseEvent -> Action
          nextClickHandler _ =
              if props.currentPage < props.maxPage then
              props.changePageAction $ props.currentPage + 1
              else
              NoOp

          prevClickHandler :: P.MouseEvent -> Action
          prevClickHandler _ =
              if props.currentPage > minPage then
              props.changePageAction $ props.currentPage - 1
              else
              NoOp

          changeHandler :: P.FormEvent -> Action
          changeHandler ev =
              let value = fromMaybe props.currentPage <<< fromString <<< _.value $ _.target ev in
              if value >= minPage && value <= props.maxPage
              then props.changePageAction value
              else NoOp


getMaxPaginationNumber :: Int -> Int -> Int
getMaxPaginationNumber quantity max =
    ceil (toNumber quantity / toNumber max)

-- -----------------
-- txs empty view
-- -----------------

txEmptyContentView :: Language -> P.Html Action
txEmptyContentView lang = P.div
                        [ P.className "tx-empty__container" ]
                        [ P.text $ translate (I18nL.tx <<< I18nL.txEmpty) lang ]

-- -----------------
-- helper
-- -----------------

newtype EmptyViewProps = EmptyViewProps {}

mkEmptyViewProps :: EmptyViewProps
mkEmptyViewProps = EmptyViewProps {}

noData :: String
noData = "--"

currencyCSSClass :: Maybe CCurrency -> String
currencyCSSClass mCurrency =
  case mCurrency of
      Just ADA -> " currency ada bg-ada-dark"
      Just USD -> " currency usd bg-usd-dark"
      _ -> ""

-- TODO (jk) Remove placeholderView if all views are implemented
placeholderView :: String -> P.Html Action
placeholderView label =
    P.div
        [ P.className "explorer-dashboard__content" ]
        [ P.text label ]
