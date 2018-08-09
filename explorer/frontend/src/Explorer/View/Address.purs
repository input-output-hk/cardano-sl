module Explorer.View.Address where

import Prelude

import Data.Array (length, null, slice)
import Data.Foldable (for_)
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
import Explorer.View.CSS as CSS
import Explorer.View.Dashboard.Shared (headerView)
import Explorer.View.Dashboard.Types (HeaderOptions(..))
import Pos.Explorer.Web.ClientTypes (CAddressSummary(..), CTxBrief)
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, caAddress, caBalance, caTxNum)
import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML as S
import Text.Smolder.HTML.Attributes as SA
import Text.Smolder.Markup ((#!), (!))
import Text.Smolder.Markup as SM

addressView :: State -> P.HTML Action
addressView state =
  S.div ! SA.className CSS.pureGContainer $ do
    S.div ! SA.className "pure-u-1-1" $ do
      addressOverview state
    S.div ! SA.className "pure-u-1-1" $ do
      case state ^. currentAddressSummary of
          NotAsked  -> txEmptyContentView ""
          Loading   -> txEmptyContentView $
              translate (I18nL.common <<< I18nL.cLoading) lang'
          Failure _ -> txEmptyContentView $
              translate (I18nL.tx <<< I18nL.txNotFound) lang'
          Success (CAddressSummary addressSummary) ->
              addressTxsView addressSummary.caTxList state
    where
      lang' = state ^. lang
      headerOptions t = HeaderOptions
          { headline: t
          , link: Nothing
          , icon: Just "fa-cube"
          }

-- | Address overview, we leave the error abstract (we are not using it)
addressOverview :: State -> P.HTML Action
addressOverview state = case state ^. currentAddressSummary of
  NotAsked    -> emptyAddressDetail ""
  Loading     -> emptyAddressDetail <<< translate (I18nL.common <<< I18nL.cLoading) $ state ^. lang
  (Failure _) -> failureView $ state ^. lang
  (Success addressSummary) ->
    S.div ! SA.className "pure-g" $ do
      S.div ! SA.className "pure-u-2-3" $ do
        headerView state $ headerOptions "fa-address-card" (translate (I18nL.common <<< I18nL.cAddress) lang')
        addressDetailView addressSummary lang'
      S.div ! SA.className "pure-u-1-3" $ do
        headerView state $ headerOptions "fa-qrcode" (translate (I18nL.address <<< I18nL.addQrCode) lang')
        addressQr addressSummary lang'
    where
      lang' = state ^. lang
      headerOptions c t = HeaderOptions
          { headline: t
          , link: Nothing
          , icon: Just c
          }

addressDetailView :: CAddressSummary -> Language -> P.HTML Action
addressDetailView addressSummary lang =
    S.table ! SA.className "pure-table pure-table-horizontal"
          $ for_ (addressDetailRowItems addressSummary lang) addressDetailRow

addressQr :: CAddressSummary -> Language -> P.HTML Action
addressQr _ lang =
    S.div ! SA.className "address-qr pure-g" $ do
        S.div ! SA.className "address-qr__wrapper pure-u-1-3" $ do
            S.div ! SA.className "address-qr__image"
                  ! SA.id addressQRImageId
                  $ SM.text ""
        S.div ! SA.className "pure-u-1-3" $ do
            S.p ! SA.className "address-qr__description"
                $ SM.text (translate (I18nL.address <<< I18nL.addScan) lang)

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
    S.tr $ do
      S.td $ SM.text item.label
      S.td $
        if isJust item.mCurrency
          then S.span ! SA.className (currencyCSSClass item.mCurrency)
                      $ SM.text item.value
          else SM.text item.value

emptyAddressDetail :: String -> P.HTML Action
emptyAddressDetail message =
    S.div ! SA.className "message"
          $ S.div $ SM.text message

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
        headerOptions = HeaderOptions
            { headline:translate (I18nL.common <<< I18nL.cTransactions) lang'
            , link: Nothing
            , icon: Just "fa-list"
            }
    in
    do

        headerView state headerOptions
        S.table ! SA.className "pure-table pure-table-horizontal" $
          for_ currentTxs (addressTxView lang')
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

addressTxView :: Language -> CTxBrief -> P.HTML Action
addressTxView lang tx = do
  txHeaderView lang $ mkTxHeaderViewProps tx
  txBodyView lang $ mkTxBodyViewProps tx

failureView :: Language -> P.HTML Action
failureView lang =
    S.div do
        S.p ! SA.className "address-failed"
            $ SM.text (translate (I18nL.address <<< I18nL.addNotFound) lang)
        S.a ! SA.href (toUrl Dashboard)
            #! P.onClick (Navigate $ toUrl Dashboard)
            ! SA.className "btn-back"
            $ SM.text (translate (I18nL.common <<< I18nL.cBack2Dashboard) lang)
