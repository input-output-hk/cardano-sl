module Explorer.View.Address where

import Prelude

import Data.Array (length, null, slice)
import Data.Foldable (for_)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (mempty)
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
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, _CHash, _CTxBrief, _CTxId, caAddress, caBalance, caTxNum, ctbId)
import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P
import Pux.DOM.HTML.Attributes (key) as P
import Pux.Renderer.React (dangerouslySetInnerHTML) as P
import Text.Smolder.HTML (a, div, span, h3, p) as S
import Text.Smolder.HTML.Attributes (className, href, id) as S
import Text.Smolder.Markup ((#!), (!))
import Text.Smolder.Markup (text) as S

addressView :: State -> P.HTML Action
addressView state =
    let lang' = state ^. lang in
    S.div ! S.className "explorer-address" $ do
        S.div ! S.className "explorer-address__wrapper" $ do
            S.div ! S.className "explorer-address__container" $ do
                S.h3  ! S.className "headline"
                      $ S.text (translate (I18nL.common <<< I18nL.cAddress) lang')
                addressOverview (state ^. currentAddressSummary) lang'
            S.div ! S.className "explorer-address__wrapper"
                  $ S.div ! S.className "explorer-address__container"
                          $ case state ^. currentAddressSummary of
                                NotAsked  -> txEmptyContentView ""
                                Loading   -> txEmptyContentView $
                                    translate (I18nL.common <<< I18nL.cLoading) lang'
                                Failure _ -> txEmptyContentView $
                                    translate (I18nL.tx <<< I18nL.txNotFound) lang'
                                Success (CAddressSummary addressSummary) ->
                                    addressTxsView addressSummary.caTxList state

-- | Address overview, we leave the error abstract (we are not using it)
addressOverview :: forall e. RemoteData e CAddressSummary -> Language -> P.HTML Action
addressOverview NotAsked    lang = emptyAddressDetail ""
addressOverview Loading     lang = emptyAddressDetail <<< translate (I18nL.common <<< I18nL.cLoading) $ lang
addressOverview (Failure _) lang = failureView lang
addressOverview (Success addressSummary) lang =
    S.div ! S.className "address-overview" $ do
        addressDetailView addressSummary lang
        addressQr addressSummary lang

addressDetailView :: CAddressSummary -> Language -> P.HTML Action
addressDetailView addressSummary lang =
    S.div ! S.className "address-detail"
          $ for_ (addressDetailRowItems addressSummary lang) addressDetailRow

addressQr :: CAddressSummary -> Language -> P.HTML Action
addressQr _ lang =
    S.div ! S.className "address-qr" $ do
        S.p ! S.className "address-qr__tab"
            $ S.text $ translate (I18nL.address <<< I18nL.addQrCode) lang
        S.div ! S.className "address-qr__wrapper" $ do
            S.div ! S.className "address-qr__image"
                  ! S.id addressQRImageId
                  $ mempty
            S.p ! S.className "address-qr__description"
                $ S.text (translate (I18nL.address <<< I18nL.addScan) lang)

type SummaryRowItem =
    { id :: String -- needed by React https://facebook.github.io/react/docs/lists-and-keys.html
    , label :: String
    , value :: String
    , mCurrency :: Maybe CCurrency
    }

type SummaryItems = Array SummaryRowItem

addressDetailRowItems :: CAddressSummary -> Language -> SummaryItems
addressDetailRowItems (CAddressSummary address) lang =
    [ { id: "0"
      , label: translate (I18nL.common <<< I18nL.cAddress) lang
      , value: address ^. (caAddress <<< _CAddress)
      , mCurrency: Nothing
    }
    , { id: "1"
      , label: translate (I18nL.common <<< I18nL.cTransactions) lang
      , value: show $ address ^. caTxNum
      , mCurrency: Nothing
    }
    , { id: "2"
      ,label: translate (I18nL.address <<< I18nL.addFinalBalance) lang
      , value: formatADA (address ^. caBalance) lang
      , mCurrency: Just ADA
      }
    ]

addressDetailRow :: SummaryRowItem -> P.HTML Action
addressDetailRow item =
    S.div ! S.className "address-detail__row"
          ! P.key item.id
          $ do
          S.div ! S.className "address-detail__column label"
                $ S.text item.label
          S.div ! S.className "address-detail__column amount"
                $ if isJust item.mCurrency
                      then S.span ! S.className (currencyCSSClass item.mCurrency)
                                  $ S.text item.value
                      else S.text item.value

emptyAddressDetail :: String -> P.HTML Action
emptyAddressDetail message =
    S.div ! S.className "message"
          $ S.div ! P.dangerouslySetInnerHTML message
                  $ mempty

maxTxRows :: Int
maxTxRows = 5

addressTxsView :: CTxBriefs -> State -> P.HTML Action
addressTxsView txs state =
    if null txs then
        txEmptyContentView $ translate (I18nL.tx <<< I18nL.txEmpty) (state ^. lang)
    else
    let txPagination = state ^. (viewStates <<< addressDetail <<< addressTxPagination <<< _PageNumber)
        lang' = state ^. lang
        minTxIndex = (txPagination - minPagination) * maxTxRows
        currentTxs = slice minTxIndex (minTxIndex + maxTxRows) txs
    in
    do
        for_ currentTxs (\tx -> addressTxView tx lang')
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

addressTxView :: CTxBrief -> Language -> P.HTML Action
addressTxView tx lang =
    S.div ! P.key (tx ^. (_CTxBrief <<< ctbId <<< _CTxId <<< _CHash))
          ! S.className "explorer-address__tx-container"
          $ do
          txHeaderView lang $ mkTxHeaderViewProps tx
          txBodyView lang $ mkTxBodyViewProps tx

failureView :: Language -> P.HTML Action
failureView lang =
    S.div do
        S.p ! S.className "address-failed"
            $ S.text (translate (I18nL.address <<< I18nL.addNotFound) lang)
        S.a ! S.href (toUrl Dashboard)
            #! P.onClick (Navigate $ toUrl Dashboard)
            ! S.className "btn-back"
            $ S.text (translate (I18nL.common <<< I18nL.cBack2Dashboard) lang)
