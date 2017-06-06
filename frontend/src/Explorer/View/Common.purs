module Explorer.View.Common (
    placeholderView
    , txHeaderView
    , txBodyView
    , emptyTxBodyView
    , emptyTxHeaderView
    , emptyView
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
    , logoView
    , clickableLogoView
    , langView
    ) where

import Prelude
import Data.Int (ceil, fromString, toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Explorer.I18n.Lang (Language(..), readLanguage, translate)
import Explorer.I18n.Lenses (common, cDateFormat) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (initialState)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), PageNumber(..), State)
import Explorer.Util.Factory (mkCAddress, mkCTxId, mkCoin)
import Explorer.Util.String (formatADA)
import Explorer.Util.Time (prettyDate)
import Explorer.View.Lenses (txbAmount, txbInputs, txbOutputs, txhAmount, txhHash, txhTimeIssued)
import Exporer.View.Types (TxBodyViewProps(..), TxHeaderViewProps(..))
import Pos.Explorer.Web.ClientTypes (CCoin, CAddress(..), CTxBrief(..), CTxEntry(..), CTxSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CHash, _CTxId, ctbId, ctbInputs, ctbOutputs, ctbOutputSum, ctbTimeIssued, cteId, cteTimeIssued, ctsBlockTimeIssued, ctsId, ctsInputs, ctsOutputs, ctsTotalOutput)
import Pux.Html (Html, text, div, p, span, input, option, select) as P
import Pux.Html.Attributes (className, href, value, disabled, type_, min, max, defaultValue) as P
import Pux.Html.Events (onBlur, onChange, onFocus, onKey, KeyboardEvent, MouseEvent, Target, onClick) as P
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
        , txhAmount: txBrief ^. ctbOutputSum
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
        , txhAmount: mkCoin "0"
        }

txHeaderView :: Language -> TxHeaderViewProps -> P.Html Action
txHeaderView lang (TxHeaderViewProps props) =
    P.div
          [ P.className "transaction-header"]
          [ P.div
              [ P.className "hash-container" ]
              [ P.link (toUrl <<< Tx $ props ^. txhHash)
                [ P.className "hash"]
                [ P.text $ props ^. (txhHash <<< _CTxId <<< _CHash) ]
              ]
          , P.div
              [ P.className "date"]
              [ P.text $ case props ^. txhTimeIssued of
                              Just time ->
                                  let format = translate (I18nL.common <<< I18nL.cDateFormat) lang
                                  in fromMaybe noData $ prettyDate format time
                              Nothing -> noData
              ]
          , txAmountView (props ^. txhAmount) lang
          ]

emptyTxHeaderView :: P.Html Action
emptyTxHeaderView =
    P.div
        [ P.className "transaction-header"]
        [ ]

txAmountView :: CCoin -> Language -> P.Html Action
txAmountView coin lang =
    P.div
        [ P.className "amount-container" ]
        [ P.div
            [ P.className "amount bg-ada" ]
            [ P.text $ formatADA coin lang ]
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
        , txbAmount: txBrief ^. ctbOutputSum
        }

-- | Creates a TxBodyViewProps by a given EmptyViewProps
instance emptyTxBodyViewPropsFactory :: TxBodyViewPropsFactory EmptyViewProps where
    mkTxBodyViewProps _ = TxBodyViewProps
        { txbInputs: []
        , txbOutputs: []
        , txbAmount: mkCoin "0"
        }

txBodyView :: Language -> TxBodyViewProps -> P.Html Action
txBodyView lang (TxBodyViewProps props) =
    P.div
        [ P.className "transaction-body" ]
        [ P.div
            [ P.className "from-hash-container" ]
            <<< map txFromView $ props ^. txbInputs
        , P.div
            [ P.className "to-hash-container bg-transaction-arrow" ]
            [ P.div
                [ P.className "to-hash-wrapper"]
                <<< map txToView $ props ^. txbOutputs
            ]
        , P.div
              [ P.className "amounts-container" ]
              <<< map (txBodyAmountView lang) $ props ^. txbOutputs
        , txAmountView (props ^. txbAmount) lang
        ]

emptyTxBodyView :: P.Html Action
emptyTxBodyView =
    P.div
        [ P.className "transaction-body" ]
        []

txFromView :: Tuple CAddress CCoin -> P.Html Action
txFromView (Tuple (CAddress cAddress) _) =
    P.link (toUrl <<< Address $ mkCAddress cAddress)
        [ P.className "from-hash" ]
        [ P.text cAddress ]

txToView :: Tuple CAddress CCoin -> P.Html Action
txToView (Tuple (CAddress cAddress) _) =
    P.link (toUrl <<< Address $ mkCAddress cAddress)
          [ P.className "to-hash"]
          [ P.text cAddress ]

txBodyAmountView :: Language -> Tuple CAddress CCoin -> P.Html Action
txBodyAmountView lang (Tuple _ coin) =
    P.div
        [ P.className "amount-wrapper" ]
        [ P.span
            [ P.className "plain-amount bg-ada-dark" ]
            [ P.text $ formatADA coin lang ]
        ]

-- -----------------
-- pagination
-- -----------------

type PaginationViewProps =
    { label :: String
    , currentPage :: PageNumber
    , minPage :: PageNumber
    , maxPage :: PageNumber
    , editable :: Boolean
    , changePageAction :: (PageNumber -> Action)
    , editableAction :: (P.Target -> Boolean -> Action)
    , invalidPageAction :: (P.Target -> Action)
    , disabled :: Boolean
    }

txPaginationView :: PaginationViewProps -> P.Html Action
txPaginationView props =
    P.div
        [ P.className "transaction-pagination"]
        [ paginationView props ]

paginationView :: PaginationViewProps -> P.Html Action
paginationView props =
    P.div
        [ P.className "pagination-wrapper"]
        [ P.div
              [ P.className "pagination" ]
              [ P.div
                  [ P.className "pagination__container" ]
                  [ P.div
                      [ P.className $ "btn-page" <> disablePrevBtnClazz
                      , P.onClick prevClickHandler ]
                      [ P.div
                          [ P.className "icon bg-triangle-left" ]
                          []
                      ]
                  , P.input
                      ([ P.className "page-number"
                      , P.disabled $ props.maxPage == props.minPage
                      , P.min <<< show $ unwrap props.minPage
                      , P.max <<< show $ unwrap props.maxPage
                      , P.onFocus \event -> props.editableAction (_.target event) true
                      , P.onBlur \event -> props.editableAction (_.target event) false
                      ]
                      <>  if props.editable
                          then [ P.onKey "enter" onEnterHandler ]
                          else [ P.value <<< show $ unwrap props.currentPage ]
                      )
                      []
                  , P.p
                      [ P.className "label" ]
                      [ P.text props.label ]
                  , P.input
                      [ P.className "page-number"
                      , P.disabled true
                      , P.type_ "search"
                      , P.value <<< show $ unwrap props.maxPage
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
          , P.div
              [ P.className $ "pagination-cover"
                    <>  if props.disabled
                        then " show"
                        else ""
              , P.onClick $ const NoOp
              ]
              []
          ]
          where
            disablePrevBtnClazz = if props.currentPage == props.minPage then " disabled" else ""
            disableNextBtnClazz = if props.currentPage == props.maxPage then " disabled" else ""
            nextClickHandler :: P.MouseEvent -> Action
            nextClickHandler event =
                if props.currentPage < props.maxPage then
                props.changePageAction <<< PageNumber $ (unwrap props.currentPage) + 1
                else
                NoOp

            prevClickHandler :: P.MouseEvent -> Action
            prevClickHandler _ =
                if props.currentPage > props.minPage && not props.disabled then
                props.changePageAction <<< PageNumber $ (unwrap props.currentPage) - 1
                else
                NoOp

            onEnterHandler :: P.KeyboardEvent -> Action
            onEnterHandler event =
                if props.disabled
                then NoOp
                else
                    if page >= props.minPage && page <= props.maxPage
                    then props.changePageAction page
                    else props.invalidPageAction target
                    where
                        target = _.target event
                        page = PageNumber <<< fromMaybe (unwrap props.currentPage) <<< fromString $ _.value target



getMaxPaginationNumber :: Int -> Int -> Int
getMaxPaginationNumber quantity max =
    ceil (toNumber quantity / toNumber max)

-- -----------------
-- txs empty view
-- -----------------

txEmptyContentView :: String -> P.Html Action
txEmptyContentView message = P.div
                        [ P.className "tx-empty__container" ]
                        [ P.text message ]

-- -----------------
-- logo
-- -----------------

logoView' :: Maybe Route -> P.Html Action
logoView' mRoute =
    let logoContentTag = case mRoute of
                              Just route -> P.link (toUrl route)
                              Nothing -> P.div
    in
    P.div
        [ P.className "logo__container"]
        [ P.div
            [ P.className "logo__wrapper"]
            [ logoContentTag
                [ P.className "logo__img bg-logo"
                , P.href "/"]
                []
            ]
        ]

logoView :: P.Html Action
logoView = logoView' Nothing

clickableLogoView :: Route -> P.Html Action
clickableLogoView = logoView' <<< Just

-- -----------------
-- lang
-- -----------------

-- currency

langItems :: Array Language
langItems =
    [ English
    , Japanese
    , German
    ]

langView :: State -> P.Html Action
langView state =
  P.select
      [ P.className "lang__select bg-arrow-up"
      , P.defaultValue <<< show $ state ^. lang
      , P.onChange $ SetLanguage <<< fromMaybe (_.lang initialState) <<< readLanguage <<< _.value <<< _.target]
      $ map (langItemView state) langItems

langItemView :: State -> Language -> P.Html Action
langItemView state lang' =
  let selected = (show lang') == (show $ state ^. lang) in
  P.option
    [ P.value $ show lang'
    , P.className $ if selected then "selected" else ""
    ]
    [ P.text $ show lang' ]

-- -----------------
-- helper
-- -----------------

newtype EmptyViewProps = EmptyViewProps {}

mkEmptyViewProps :: EmptyViewProps
mkEmptyViewProps = EmptyViewProps {}

noData :: String
noData = "--"

-- | Helper to add currency classes
currencyCSSClass :: Maybe CCurrency -> String
currencyCSSClass mCurrency =
  case mCurrency of
      Just ADA -> "ada bg-ada-dark"
      Just USD -> "usd bg-usd-dark"
      _ -> ""

-- TODO (jk) Remove placeholderView if all views are implemented
placeholderView :: String -> P.Html Action
placeholderView label =
    P.div
        [ P.className "explorer-dashboard__content" ]
        [ P.text label ]

emptyView :: P.Html Action
emptyView = P.div [] []
