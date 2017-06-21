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

import Pux.DOM.Events (onBlur, onChange, onFocus, onKey, KeyboardEvent, MouseEvent, Target, onClick) as P
import Pux.DOM.HTML (Html) as P

import Text.Smolder.HTML (div, p, span, input, option, select)
import Text.Smolder.HTML.Attributes (className, href, value, disabled, type', min, max, defaultValue)
import Text.Smolder.Markup (text, (#!), (!), (!?))




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
    div ! className "transaction-header" $ do
        div ! className "hash-container"
            $ a ! href (toUrl txRoute)
                #! onClick (toUrl txRoute)
                ! className "hash"
                $ text (props ^. (txhHash <<< _CTxId <<< _CHash))
        div ! className "date"
            $ text $ case props ^. txhTimeIssued of
                              Just time ->
                                  let format = translate (I18nL.common <<< I18nL.cDateFormat) lang
                                  in fromMaybe noData $ prettyDate format time
                              Nothing -> noData
        txAmountView (props ^. txhAmount) lang
    where
        txRoute = Tx $ props ^. txhHash

emptyTxHeaderView :: P.Html Action
emptyTxHeaderView =
    div ! className "transaction-header"

txAmountView :: CCoin -> Language -> P.Html Action
txAmountView coin lang =
    div ! className "amount-container"
        $ div ! className "amount bg-ada"
              $ text (formatADA coin lang)

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
    div ! className "transaction-body" $ do
        div ! className "from-hash-container"
            $ map txFromView (props ^. txbInputs)
        div ! className "to-hash-container bg-transaction-arrow"
            $ div ! className "to-hash-wrapper"
                  $ map txToView (props ^. txbOutputs)
        div ! className "amounts-container"
            $ map (txBodyAmountView lang) (props ^. txbOutputs)
        txAmountView (props ^. txbAmount) lang

emptyTxBodyView :: P.Html Action
emptyTxBodyView =
    div ! className "transaction-body"

txFromView :: Tuple CAddress CCoin -> P.Html Action
txFromView (Tuple (CAddress cAddress) _) =
    let addressRoute = Address $ mkCAddress cAddress in
    a ! href (toUrl addressRoute)
      #! onClick addressRoute
      ! className "from-hash"
      $ text cAddress

txToView :: Tuple CAddress CCoin -> P.Html Action
txToView (Tuple (CAddress cAddress) _) =
    let addressRoute = Address $ mkCAddress cAddress in
    a ! href (toUrl addressRoute)
      #! onClick (toUrl addressRoute)
      ! className "to-hash"
      $ text cAddress

txBodyAmountView :: Language -> Tuple CAddress CCoin -> P.Html Action
txBodyAmountView lang (Tuple _ coin) =
    div ! className "amount-wrapper"
        $ span ! className "plain-amount bg-ada-dark"
               $ text (formatADA coin lang)

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
    div ! className "transaction-pagination"
        $ paginationView props

paginationView :: PaginationViewProps -> P.Html Action
paginationView props =
    div ! className "pagination-wrapper" $ do
        div ! className "pagination"
            $ div ! className "pagination__container" $ do
                  div ! className ("btn-page" <> disablePrevBtnClazz)
                      #! onClick prevClickHandler
                      $ div ! className "icon bg-triangle-left"
                  input ! className "page-number"
                        ! disabled $ props.maxPage == props.minPage
                        ! min <<< show $ unwrap props.minPage
                        ! max <<< show $ unwrap props.maxPage
                        !? props.editable value (show $ unwrap props.currentPage)
                        #! P.onFocus (\event -> props.editableAction (_.target event)) true
                        #! P.onBlur (\event -> props.editableAction (_.target event)) false
                        #! P.onKey "enter" onEnterHandler
                  p ! className "label"
                    $ text props.label
                  input ! className "page-number"
                        ! disabled true
                        ! type' "search"
                        ! value (show $ unwrap props.maxPage)
                  div ! className ("btn-page" <> disableNextBtnClazz)
                      #! P.onClick nextClickHandler
                      $ div ! className "icon bg-triangle-right"
        div ! className ("pagination-cover" <> if props.disabled then " show" else "")
            #! onClick (const NoOp) -- add click handler to hide clickes from children
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
    div ! className "logo__container"
        $ div ! className "logo__wrapper"
              $ logoContentTag ! className "logo__img bg-logo"
                               ! href (toUrl Dashboard)

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
  select ! className "lang__select bg-arrow-up"
         ! defaultValue <<< show $ state ^. lang
         #! onChange $ SetLanguage <<< fromMaybe (_.lang initialState) <<< readLanguage <<< _.value <<< _.target
         $ map (langItemView state) langItems

langItemView :: State -> Language -> P.Html Action
langItemView state lang' =
  let selected = (show lang') == (show $ state ^. lang) in
  option ! value $ show lang'
         ! className $ if selected then "selected" else ""
         $ text (show lang')

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
    div ! className "explorer-dashboard__content"
        $ text label

emptyView :: P.Html Action
emptyView = div
