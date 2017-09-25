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
    , langItems
    , langView
    ) where

import Prelude

import Data.Array (length)
import Data.Foldable (for_)
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
import Explorer.Util.Config (testNetVersion)
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
import Text.Smolder.Markup ((#!), (!), (!?))
import Text.Smolder.Markup (text) as S




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
        , txhTimeIssued: entry ^. cteTimeIssued
        , txhAmount: entry . cteAmount
        }

-- | Creates a TxHeaderViewProps by a given CTxBrief
instance cTxBriefTxHeaderViewPropsFactory :: TxHeaderViewPropsFactory CTxBrief where
    mkTxHeaderViewProps (CTxBrief txBrief) = TxHeaderViewProps
        { txhHash: txBrief ^. ctbId
        , txhTimeIssued: txBrief ^. ctbTimeIssued
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
          $ S.text ""

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
    let inputs = props ^. txbInputs
        lInputs = length inputs
        outputs = props ^. txbOutputs
        lOutputs = length outputs
    in
    S.div ! S.className "transaction-body" $ do
        S.div ! S.className "from-hash__container" $ do
              S.div ! S.className "from-hash__wrapper"
                    $ for_ inputs (txMaybeFromView lang)
              -- On mobile devices we wan't to show amounts of `inputs`.
              -- This view is hidden on desktop by CSS.
              S.div ! S.className "from-hash__amounts"
                    $ if (lInputs > lOutputs)
                          then for_ inputs (txBodyMaybeAmountView lang)
                          else S.text ""
        S.div ! S.className "to-hash__container bg-transaction-arrow" $ do
              S.div ! S.className "to-hash__wrapper"
                    $ for_ outputs txToView
              -- On mobile devices we wan't to show amounts of `outputs`.
              -- This view is hidden on desktop by CSS.
              S.div ! S.className "to-hash__amounts"
                    $ if (lOutputs >= lInputs)
                          then for_ outputs (txBodyAmountView lang)
                          else S.text ""
        -- On desktop we do show amounts within an extra column.
        -- This column is hidden on mobile by CSS.
        S.div ! S.className "amounts-container"
              $ if (lOutputs >= lInputs)
                then for_ outputs (txBodyAmountView lang)
                else for_ inputs (txBodyMaybeAmountView lang)

        -- On mobile we do show an extra row of total amount
        -- This view is hidden on desktop by CSS.
        txAmountView (props ^. txbAmount) lang

emptyTxBodyView :: P.HTML Action
emptyTxBodyView =
    S.div ! S.className "transaction-body"
          $ S.text ""

txMaybeFromView :: Language -> Maybe (Tuple CAddress CCoin) -> P.HTML Action
txMaybeFromView _ (Just tuple) = txFromView tuple
txMaybeFromView lang Nothing = txFromEmptyView lang

txFromView :: Tuple CAddress CCoin -> P.HTML Action
txFromView (Tuple (CAddress cAddress) _) =
    let addressRoute = Address $ mkCAddress cAddress in
    S.a ! S.href (toUrl addressRoute)
        #! P.onClick (Navigate $ toUrl addressRoute)
        ! S.className "from-hash__value"
        $ S.text cAddress

txFromEmptyView :: Language -> P.HTML Action
txFromEmptyView lang =
    S.p ! S.className "from-hash__empty"
        $ S.text noData

txToView :: Tuple CAddress CCoin -> P.HTML Action
txToView (Tuple (CAddress cAddress) _) =
    let addressRoute = Address $ mkCAddress cAddress in
    S.a ! S.href (toUrl addressRoute)
        #! P.onClick (Navigate $ toUrl addressRoute)
        ! S.className "to-hash__value"
        $ S.text cAddress

txBodyMaybeAmountView :: Language -> Maybe (Tuple CAddress CCoin) -> P.HTML Action
txBodyMaybeAmountView lang (Just tuple) = txBodyAmountView lang tuple
txBodyMaybeAmountView lang Nothing = txBodyAmountEmptyView lang

txBodyAmountView :: Language -> Tuple CAddress CCoin -> P.HTML Action
txBodyAmountView lang (Tuple _ coin) =
    S.div ! S.className "amount-wrapper"
          $ S.span  ! S.className "plain-amount bg-ada-dark"
                    $ S.text (formatADA coin lang)

txBodyAmountEmptyView :: Language -> P.HTML Action
txBodyAmountEmptyView lang =
    S.div ! S.className "amount-wrapper"
          $ S.span  ! S.className "empty-amount"
                    $ S.text noData

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
                                      $ S.text ""
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
                                    $ S.text ""
          S.div ! S.className ("pagination-cover" <> if props.disabled then " show" else "")
                #! P.onClick (const NoOp) -- add click handler to hide clickes from children
                $ S.text ""
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

logoView' :: Maybe Route -> Boolean -> P.HTML Action
logoView' mRoute isTestnet =
    let logoContentTag = case mRoute of
                              Just route ->
                                  S.a ! S.href (toUrl route)
                                      #! P.onClick (Navigate $ toUrl route)
                              Nothing ->
                                  S.div

        iconHiddenClazz = if isTestnet then "" else " hide"
    in
    S.div
        ! S.className "logo__container"
        $ S.div
            ! S.className "logo__wrapper"
            $ logoContentTag
                ! S.className "logo__img bg-logo"
                $ S.div
                    ! S.className ("testnet-icon" <> iconHiddenClazz)
                    $ S.text ("TN " <> testNetVersion)

logoView :: Boolean -> P.HTML Action
logoView isTestnet = logoView' Nothing isTestnet

clickableLogoView :: Route -> Boolean -> P.HTML Action
clickableLogoView route isTestnet = logoView' (Just route) isTestnet

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
emptyView = S.div $ S.text ""
