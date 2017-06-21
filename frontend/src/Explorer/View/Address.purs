module Explorer.View.Address where

import Prelude hiding (id)
import Data.Array (length, null, slice)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), isJust)
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (addNotFound, cAddress, cBack2Dashboard, common, cLoading, cOf, cTransactions, address, addScan, addQrCode, addFinalBalance, tx, txEmpty, txNotFound) as I18nL
import Explorer.Lenses.State (_PageNumber, addressDetail, addressTxPagination, addressTxPaginationEditable, currentAddressSummary, lang, viewStates)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (addressQRImageId, minPagination)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), PageNumber(..), State, CTxBriefs)
import Explorer.Util.String (formatADA)
import Explorer.View.Common (currencyCSSClass, getMaxPaginationNumber, mkTxBodyViewProps, mkTxHeaderViewProps, txBodyView, txEmptyContentView, txHeaderView, txPaginationView)
import Network.RemoteData (RemoteData(..))
import Pos.Explorer.Web.ClientTypes (CAddressSummary(..), CTxBrief)
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, caAddress, caBalance, caTxNum)

import Pux.DOM.HTML (Html) as P
import Pux.Renderer.React (dangerouslySetInnerHTML) as P
import Pux.DOM.Events (onClick) as P

import Text.Smolder.HTML (div, text, span, h3, p)
import Text.Smolder.HTML.Attributes (className, href, id)
import Text.Smolder.Markup (text, (#!), (!))

addressView :: State -> P.Html Action
addressView state =
    let lang' = state ^. lang in
    div ! className "explorer-address" $ do
        div ! className "explorer-address__wrapper" $ do
            div ! className "explorer-address__container" $ do
                h3  ! className "headline"
                    $ text (translate (I18nL.common <<< I18nL.cAddress) lang')
                addressOverview (state ^. currentAddressSummary) lang'
            div ! className "explorer-address__wrapper" $ do
                div ! className "explorer-address__container" $ do
                    case state ^. currentAddressSummary of
                        NotAsked  -> txEmptyContentView ""
                        Loading   -> txEmptyContentView $
                            translate (I18nL.common <<< I18nL.cLoading) lang'
                        Failure _ -> txEmptyContentView $
                            translate (I18nL.tx <<< I18nL.txNotFound) lang'
                        Success (CAddressSummary addressSummary) ->
                            addressTxsView addressSummary.caTxList state

-- | Address overview, we leave the error abstract (we are not using it)
addressOverview :: forall e. RemoteData e CAddressSummary -> Language -> P.Html Action
addressOverview NotAsked    lang = emptyAddressDetail ""
addressOverview Loading     lang = emptyAddressDetail <<< translate (I18nL.common <<< I18nL.cLoading) $ lang
addressOverview (Failure _) lang = failureView lang
addressOverview (Success addressSummary) lang =
    div ! className "address-overview" $ do
        addressDetailView addressSummary lang
        addressQr addressSummary lang

addressDetailView :: CAddressSummary -> Language -> P.Html Action
addressDetailView addressSummary lang =
    div ! className "address-detail" $ do
        map addressDetailRow $ addressDetailRowItems addressSummary lang

addressQr :: CAddressSummary -> Language -> P.Html Action
addressQr _ lang =
    div ! className "address-qr" $ do
        p ! className "address-qr__tab"
          $ text $ translate (I18nL.address <<< I18nL.addQrCode) lang
        div ! className "address-qr__wrapper" $ do
            div ! className "address-qr__image"
                ! id addressQRImageId
            p ! className "address-qr__description"
              $ text (translate (I18nL.address <<< I18nL.addScan) lang)

type SummaryRowItem =
    { label :: String
    , value :: String
    , mCurrency :: Maybe CCurrency
    }

type SummaryItems = Array SummaryRowItem

addressDetailRowItems :: CAddressSummary -> Language -> SummaryItems
addressDetailRowItems (CAddressSummary address) lang =
    [ { label: translate (I18nL.common <<< I18nL.cAddress) lang
      , value: address ^. (caAddress <<< _CAddress)
      , mCurrency: Nothing
    }
    , { label: translate (I18nL.common <<< I18nL.cTransactions) lang
      , value: show $ address ^. caTxNum
      , mCurrency: Nothing
    }
    , { label: translate (I18nL.address <<< I18nL.addFinalBalance) lang
      , value: formatADA (address ^. caBalance) lang
      , mCurrency: Just ADA
      }
    ]

addressDetailRow :: SummaryRowItem -> P.Html Action
addressDetailRow item =
    div ! className "address-detail__row" $ do
        div ! className "address-detail__column label"
            $ text item.label
        div ! className $ "address-detail__column amount" $ do
            if isJust item.mCurrency
                then span ! className (currencyCSSClass item.mCurrency)
                          $ text item.value
                else text item.value

emptyAddressDetail :: String -> P.Html Action
emptyAddressDetail message =
    div ! className "message" $ do
        div ! P.dangerouslySetInnerHTML message

maxTxRows :: Int
maxTxRows = 5

addressTxsView :: CTxBriefs -> State -> P.Html Action
addressTxsView txs state =
    if null txs then
        txEmptyContentView $ translate (I18nL.tx <<< I18nL.txEmpty) (state ^. lang)
    else
    let txPagination = state ^. (viewStates <<< addressDetail <<< addressTxPagination <<< _PageNumber)
        lang' = state ^. lang
        minTxIndex = (txPagination - minPagination) * maxTxRows
        currentTxs = slice minTxIndex (minTxIndex + maxTxRows) txs
    in
    div do
        div do
            map (\tx -> addressTxView tx lang') currentTxs
            txPaginationView  { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
                              , currentPage: PageNumber txPagination
                              , minPage: PageNumber minPagination
                              , maxPage: PageNumber $ getMaxPaginationNumber (length txs) maxTxRows
                              , changePageAction: AddressPaginateTxs
                              , editable: state ^. (viewStates <<< addressDetail <<< addressTxPaginationEditable)
                              , editableAction: AddressEditTxsPageNumber
                              , invalidPageAction: AddressInvalidTxsPageNumber
                              , disabled: false
                              }

addressTxView :: CTxBrief -> Language -> P.Html Action
addressTxView tx lang =
    div do
        txHeaderView lang $ mkTxHeaderViewProps tx
        txBodyView lang $ mkTxBodyViewProps tx

failureView :: Language -> P.Html Action
failureView lang =
    div do
        p ! className "address-failed"
          $ text (translate (I18nL.address <<< I18nL.addNotFound) lang)
        a ! href (toUrl Dashboard)
          #! onClick (toUrl Dashboard)
          ! className "btn-back"
          $ text (translate (I18nL.common <<< I18nL.cBack2Dashboard) lang)
