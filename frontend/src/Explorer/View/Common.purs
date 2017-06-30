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

import Data.Foldable (for_)
import Data.Int (ceil, fromString, toNumber)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

import Explorer.I18n.Lang (Language(..), readLanguage, translate)
import Explorer.I18n.Lenses (common, cDateFormat) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (initialState)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), PageNumber(..), State)
import Explorer.Util.DOM (enterKeyPressed)
import Explorer.Util.Factory (mkCAddress, mkCTxId, mkCoin)
import Explorer.Util.String (formatADA)
import Explorer.Util.Time (prettyDate)
import Explorer.View.Lenses (txbAmount, txbInputs, txbOutputs, txhAmount, txhHash, txhTimeIssued)
import Exporer.View.Types (TxBodyViewProps(..), TxHeaderViewProps(..))

import Pos.Explorer.Web.ClientTypes (CCoin, CAddress(..), CTxBrief(..), CTxEntry(..), CTxSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CHash, _CTxId, ctbId, ctbInputs, ctbOutputs, ctbOutputSum, ctbTimeIssued, cteId, cteTimeIssued, ctsBlockTimeIssued, ctsId, ctsInputs, ctsOutputs, ctsTotalOutput)

import Pux.DOM.Events (DOMEvent, onBlur, onChange, onFocus, onKeyDown, onClick, targetValue) as P
import Pux.DOM.HTML (HTML) as P

import Text.Smolder.HTML (a, div, p, span, input, option, select) as S
import Text.Smolder.HTML.Attributes (className, href, value, disabled, type', min, max) as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((#!), (!), (!?))




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

txHeaderView :: Language -> TxHeaderViewProps -> P.HTML Action
txHeaderView lang (TxHeaderViewProps props) =
    S.div ! S.className "transaction-header" $ do
        S.div ! S.className "hash-container"
              $ S.a ! S.href (toUrl txRoute)
              #! P.onClick (Navigate $ toUrl txRoute)
              ! S.className "hash"
              $ S.text (props ^. (txhHash <<< _CTxId <<< _CHash))
        S.div ! S.className "date"
              $ S.text $ case props ^. txhTimeIssued of
                              Just time ->
                                  let format = translate (I18nL.common <<< I18nL.cDateFormat) lang
                                  in fromMaybe noData $ prettyDate format time
                              Nothing -> noData
        txAmountView (props ^. txhAmount) lang
    where
        txRoute = Tx $ props ^. txhHash

emptyTxHeaderView :: P.HTML Action
emptyTxHeaderView =
    S.div ! S.className "transaction-header"
          $ mempty

txAmountView :: CCoin -> Language -> P.HTML Action
txAmountView coin lang =
    S.div ! S.className "amount-container"
          $ S.div ! S.className "amount bg-ada"
                  $ S.text (formatADA coin lang)

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

txBodyView :: Language -> TxBodyViewProps -> P.HTML Action
txBodyView lang (TxBodyViewProps props) =
    S.div ! S.className "transaction-body" $ do
        S.div ! S.className "from-hash-container"
              $ for_ (props ^. txbInputs) txFromView
        S.div ! S.className "to-hash-container bg-transaction-arrow"
              $ S.div ! S.className "to-hash-wrapper"
                      $ for_ (props ^. txbOutputs) txToView
        S.div ! S.className "amounts-container"
              $ for_ (props ^. txbOutputs) (txBodyAmountView lang)
        txAmountView (props ^. txbAmount) lang

emptyTxBodyView :: P.HTML Action
emptyTxBodyView =
    S.div ! S.className "transaction-body"
          $ mempty

txFromView :: Tuple CAddress CCoin -> P.HTML Action
txFromView (Tuple (CAddress cAddress) _) =
    let addressRoute = Address $ mkCAddress cAddress in
    S.a ! S.href (toUrl addressRoute)
        #! P.onClick (Navigate $ toUrl addressRoute)
        ! S.className "from-hash"
        $ S.text cAddress

txToView :: Tuple CAddress CCoin -> P.HTML Action
txToView (Tuple (CAddress cAddress) _) =
    let addressRoute = Address $ mkCAddress cAddress in
    S.a ! S.href (toUrl addressRoute)
        #! P.onClick (Navigate $ toUrl addressRoute)
        ! S.className "to-hash"
        $ S.text cAddress

txBodyAmountView :: Language -> Tuple CAddress CCoin -> P.HTML Action
txBodyAmountView lang (Tuple _ coin) =
    S.div ! S.className "amount-wrapper"
          $ S.span  ! S.className "plain-amount bg-ada-dark"
                    $ S.text (formatADA coin lang)

-- -----------------
-- pagination
-- -----------------

type PaginationViewProps =
    { label :: String
    , currentPage :: PageNumber
    , minPage :: PageNumber
    , maxPage :: PageNumber
    , editable :: Boolean
    , changePageAction :: (Maybe P.DOMEvent -> PageNumber -> Action)
    , editableAction :: (P.DOMEvent -> Boolean -> Action)
    , invalidPageAction :: (P.DOMEvent -> Action)
    , disabled :: Boolean
    }

txPaginationView :: PaginationViewProps -> P.HTML Action
txPaginationView props =
    S.div ! S.className "transaction-pagination"
          $ paginationView props

paginationView :: PaginationViewProps -> P.HTML Action
paginationView props =
    S.div ! S.className "pagination-wrapper" $ do
          S.div ! S.className "pagination" $ do
                S.div ! S.className "pagination__container" $ do
                      S.div ! S.className ("btn-page" <> disablePrevBtnClazz)
                              #! P.onClick prevClickHandler
                              $ S.div ! S.className "icon bg-triangle-left"
                                      $ mempty
                      (S.input !? not props.editable) (S.value <<< show $ unwrap props.currentPage)
                            ! S.className "page-number"
                            -- ! S.disabled (show $ props.maxPage == props.minPage)
                            ! S.disabled (if props.maxPage == props.minPage then "disabled" else "")
                            ! S.min (show $ unwrap props.minPage)
                            ! S.max (show $ unwrap props.maxPage)
                            #! P.onFocus (\ev -> props.editableAction ev true)
                            #! P.onBlur (\ev -> props.editableAction ev false)
                            #! P.onKeyDown onKeyDownHandler
                      S.p ! S.className "label"
                          $ S.text props.label
                      S.input ! S.className "page-number"
                              ! S.disabled (show true)
                              ! S.type' "search"
                              ! S.value (show $ unwrap props.maxPage)
                      S.div ! S.className ("btn-page" <> disableNextBtnClazz)
                            #! P.onClick nextClickHandler
                            $ S.div ! S.className "icon bg-triangle-right"
                                    $ mempty
          S.div ! S.className ("pagination-cover" <> if props.disabled then " show" else "")
                #! P.onClick (const NoOp) -- add click handler to hide clickes from children
                $ mempty
          where
              disablePrevBtnClazz = if props.currentPage == props.minPage then " disabled" else ""
              disableNextBtnClazz = if props.currentPage == props.maxPage then " disabled" else ""
              nextClickHandler :: P.DOMEvent -> Action
              nextClickHandler event =
                  if props.currentPage < props.maxPage then
                  props.changePageAction Nothing (PageNumber $ (unwrap props.currentPage) + 1)
                  else
                  NoOp

              prevClickHandler :: P.DOMEvent -> Action
              prevClickHandler event =
                  if props.currentPage > props.minPage && not props.disabled then
                  props.changePageAction Nothing (PageNumber $ (unwrap props.currentPage) - 1)
                  else
                  NoOp

              onKeyDownHandler :: P.DOMEvent -> Action
              onKeyDownHandler event =
                  if props.disabled
                      then NoOp
                      else
                          if enterKeyPressed event
                          then
                              let page = PageNumber <<< fromMaybe (unwrap props.currentPage) <<< fromString $ P.targetValue event in
                              if page >= props.minPage && page <= props.maxPage
                                  then props.changePageAction (Just event) page
                                  else props.invalidPageAction event
                          else NoOp



getMaxPaginationNumber :: Int -> Int -> Int
getMaxPaginationNumber quantity max =
    ceil (toNumber quantity / toNumber max)

-- -----------------
-- txs empty view
-- -----------------

txEmptyContentView :: String -> P.HTML Action
txEmptyContentView message =
    S.div ! S.className "tx-empty__container"
          $ S.text message

-- -----------------
-- logo
-- -----------------

logoView' :: Maybe Route -> P.HTML Action
logoView' mRoute =
    let logoContentTag = case mRoute of
                              Just route ->
                                  S.a ! S.href (toUrl route)
                                      #! P.onClick (Navigate $ toUrl route)
                                      $ mempty
                              Nothing ->
                                  S.div $ mempty
    in
    S.div ! S.className "logo__container"
          $ S.div ! S.className "logo__wrapper"
                  $ logoContentTag  ! S.className "logo__img bg-logo"
                                    ! S.href (toUrl Dashboard)

logoView :: P.HTML Action
logoView = logoView' Nothing

clickableLogoView :: Route -> P.HTML Action
clickableLogoView = logoView' <<< Just

-- -----------------
-- lang
-- -----------------

langItems :: Array Language
langItems =
    [ English
    , Japanese
    , German
    ]

langView :: State -> P.HTML Action
langView state =
    S.select  ! S.className "lang__select bg-arrow-up"
              ! S.value (show $ state ^. lang)
              #! P.onChange (SetLanguage <<< fromMaybe (_.lang initialState) <<< readLanguage <<< P.targetValue)
              $ for_ langItems (langItemView state)

langItemView :: State -> Language -> P.HTML Action
langItemView state lang' =
  let isSelected = lang' == state ^. lang in
  (S.option !? isSelected) (S.className "selected")
      ! S.value (show lang')
      $ S.text (show lang')

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
placeholderView :: String -> P.HTML Action
placeholderView label =
    S.div ! S.className "explorer-dashboard__content"
          $ S.text label

emptyView :: P.HTML Action
emptyView = S.div $ mempty
