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
import Data.Tuple (Tuple(..), snd, fst)
import Explorer.I18n.Lang (Language(..), readLanguage, translate)
import Explorer.I18n.Lenses (common, cDateFormat, cTitle) as I18nL
import Explorer.Lenses.State (lang, route, testnet)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (initialState)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), PageNumber(..), State)
import Explorer.Util.DOM (enterKeyPressed)
import Explorer.Util.Factory (mkCAddress, mkCTxId, mkCoin)
import Explorer.Util.String (formatADA)
import Explorer.Util.Time (prettyDate)
import Explorer.View.Lenses (txbInputs, txbOutputs, txhAmount, txhHash, txhTimeIssued)
import Exporer.View.Types (TxBodyViewProps(..), TxHeaderViewProps(..))
import Pos.Explorer.Web.ClientTypes (CCoin, CAddress(..), CTxBrief(..), CTxEntry(..), CTxSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CHash, _CTxId, ctbId, ctbInputs, ctbOutputs, ctbOutputSum, ctbTimeIssued, cteId, cteTimeIssued, ctsBlockTimeIssued, ctsId, ctsInputs, ctsOutputs, ctsTotalOutput)
import Pux.DOM.Events (DOMEvent, onBlur, onChange, onFocus, onKeyDown, onClick, targetValue) as P
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (a, div, p, span, input, option, select, ul, li, small, thead, table, tbody, td, th, tr) as S
import Text.Smolder.HTML.Attributes (className, href, value, disabled, type', min, max, colspan) as S
import Text.Smolder.Markup ((#!), (!), (!?))
import Text.Smolder.Markup (text, empty) as S




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
    S.thead $
      S.tr $ do
        S.th $ S.a ! S.href (toUrl txRoute)
             #! P.onClick (Navigate $ toUrl txRoute)
             $ S.text (props ^. (txhHash <<< _CTxId <<< _CHash))
        S.th $ S.text $ case props ^. txhTimeIssued of
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
    S.th ! S.className "ada" $
      txBodyAmountView lang coin
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
    S.tbody $ do
        S.td $ do
          for_ inputs (txMaybeFromView lang)
          -- On mobile devices we wan't to show amounts of `inputs`.
          -- This view is hidden on desktop by CSS.
          S.div ! S.className "from-hash__amounts" $
                if (lInputs > lOutputs)
                      then for_ inputs (txBodyMaybeAmountView lang)
                      else S.text ""
        S.td ! S.colspan "2" $ do
          S.div ! S.className "txn-arrow bg-transaction-arrow" $
            S.table $
              S.tbody $
                for_ outputs (txToView lang)
          --  On mobile devices we wan't to show amounts of `outputs`.
          --  This view is hidden on desktop by CSS.
          -- S.div ! S.className "to-hash__amounts"
          --       $ if (lOutputs >= lInputs)
          --             then for_ outputs (txBodyAmountView lang)
          --             else S.text ""
          -- On desktop we do show amounts within an extra column.
          -- This column is hidden on mobile by CSS.
          -- S.div ! S.className "amounts-container"
          --       $ if (lOutputs >= lInputs)
          --         then for_ outputs (txBodyAmountView lang)
          --         else for_ inputs (txBodyMaybeAmountView lang)

          -- On mobile we do show an extra row of total amount
          -- This view is hidden on desktop by CSS.
          -- txAmountView (props ^. txbAmount) lang

emptyTxBodyView :: P.HTML Action
emptyTxBodyView =
    S.div ! S.className "transaction-body"
          $ S.text ""

txMaybeFromView :: Language -> Maybe (Tuple CAddress CCoin) -> P.HTML Action
txMaybeFromView _ (Just tuple) = txAddressView (fst tuple)
txMaybeFromView lang Nothing = txFromEmptyView lang

txAddressView :: CAddress -> P.HTML Action
txAddressView (CAddress cAddress) =
    let addressRoute = Address $ mkCAddress cAddress in
    S.div ! S.className "truncate-address" $
      S.a ! S.href (toUrl addressRoute)
          #! P.onClick (Navigate $ toUrl addressRoute)
          $ S.text cAddress

txFromEmptyView :: Language -> P.HTML Action
txFromEmptyView lang =
    S.p ! S.className "from-hash__empty"
        $ S.text noData

txToView :: Language -> Tuple CAddress CCoin -> P.HTML Action
txToView lang (Tuple address coin) =
    S.tr $ do
      S.td $ txAddressView address
      S.td ! S.className "ada"
        $ txBodyAmountView lang coin

txBodyMaybeAmountView :: Language -> Maybe (Tuple CAddress CCoin) -> P.HTML Action
txBodyMaybeAmountView lang (Just tuple) = txBodyAmountView lang (snd tuple)
txBodyMaybeAmountView lang Nothing = txBodyAmountEmptyView lang

txBodyAmountView :: Language -> CCoin -> P.HTML Action
txBodyAmountView lang coin =
    S.div $
      S.span ! S.className "amount bg-ada" $
        S.text (formatADA coin lang)

txBodyAmountEmptyView :: Language -> P.HTML Action
txBodyAmountEmptyView lang =
    S.div $
      S.span ! S.className "plain-amount bg-ada-dark" $
        S.text noData

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

logoView :: State -> P.HTML Action
logoView state =
    let route' = state ^. route
        lang' = state ^. lang
        isTestnet = state ^. testnet
        logoContentTag c = S.a ! S.href "/"
                              #! P.onClick (Navigate $ toUrl Dashboard)
                              ! S.className c

        iconHiddenClazz = if isTestnet then "" else " hide"
        title = (translate (I18nL.common <<< I18nL.cTitle) lang')
    in
    S.div ! S.className "pure-menu" $ do
      S.div ! S.className "pure-menu-heading" $ do
        logoContentTag  "pure-menu-link header-img" $
          S.div ! S.className ("testnet-icon" <> iconHiddenClazz) $
            S.text ("Testnet")
      S.ul ! S.className "pure-menu-list" $ do
        S.li ! S.className "pure-menu-item" $
        S.li ! S.className "pure-menu-item" $ do
          logoContentTag "pure-menu-link" $ do
            S.text title
            if isTestnet
              then S.small $ S.text " - testnet"
              else S.empty

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
      Just ADA -> "amount bg-ada"
      Just USD -> "amount bg-usd"
      _ -> ""

-- TODO (jk) Remove placeholderView if all views are implemented
placeholderView :: String -> P.HTML Action
placeholderView label =
    S.div ! S.className "explorer-dashboard__content"
          $ S.text label

emptyView :: P.HTML Action
emptyView = S.div $ S.text ""
