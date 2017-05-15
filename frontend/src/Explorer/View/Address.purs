module Explorer.View.Address where

import Prelude
import Data.Array (length, (!!))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (addNotFound, cAddress, cBack2Dashboard, common, cLoading, cOf, cTransactions, address, addScan, addQrCode, addFinalBalance) as I18nL
import Explorer.Lenses.State (addressDetail, addressTxPagination, addressTxPaginationEditable, currentAddressSummary, lang, viewStates)
import Explorer.Routes (Route(..), toUrl)
import Explorer.State (addressQRImageId, minPagination)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), State)
import Explorer.View.Common (currencyCSSClass, mkTxBodyViewProps, mkTxHeaderViewProps, txBodyView, txEmptyContentView, txHeaderView, txPaginationView)
import Network.RemoteData (RemoteData(..))
import Pos.Explorer.Web.ClientTypes (CAddressSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CCoin, _CAddress, caAddress, caBalance, caTxList, caTxNum, getCoin)
import Pux.Html (Html, div, text, h3, p) as P
import Pux.Html.Attributes (className, dangerouslySetInnerHTML, id_) as P
import Pux.Router (link) as P


addressView :: State -> P.Html Action
addressView state =
    let lang' = state ^. lang in
    P.div
        [ P.className "explorer-address" ]
        [ P.div
            [ P.className "explorer-address__wrapper" ]
            [ P.div
                  [ P.className "explorer-address__container" ]
                  [ P.h3
                        [ P.className "headline"]
                        [ P.text $ translate (I18nL.common <<< I18nL.cAddress) lang' ]
                  , addressOverview (state ^. currentAddressSummary) lang'
                  ]
            ]
            , P.div
                [ P.className "explorer-address__wrapper" ]
                [ P.div
                      [ P.className "explorer-address__container" ]
                      [ case state ^. currentAddressSummary of
                            NotAsked  -> emptyAddressTxView
                            Loading   -> emptyAddressTxView
                            Failure _ -> emptyAddressTxView
                            Success (CAddressSummary addressSummary) ->
                                let txList = addressSummary ^. caTxList
                                    txPagination = state ^. (viewStates <<< addressDetail <<< addressTxPagination)
                                    currentTxBrief = txList !! (txPagination - 1)
                                in
                                P.div
                                    []
                                    [ P.h3
                                          [ P.className "headline"]
                                          [ P.text $ translate (I18nL.common <<< I18nL.cTransactions) lang' ]
                                    , case currentTxBrief of
                                        Nothing ->
                                            txEmptyContentView lang'
                                        Just txBrief ->
                                            P.div []
                                            [ txHeaderView lang' $ mkTxHeaderViewProps txBrief
                                            , txBodyView $ mkTxBodyViewProps txBrief
                                            , txPaginationView
                                                  { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
                                                  , currentPage: txPagination
                                                  , minPage: minPagination
                                                  , maxPage: length txList
                                                  , changePageAction: AddressPaginateTxs
                                                  , editable: state ^. (viewStates <<< addressDetail <<< addressTxPaginationEditable)
                                                  , editableAction: AddressEditTxsPageNumber
                                                  , invalidPageAction: AddressInvalidTxsPageNumber
                                                  }
                                            ]
                                    ]
                ]
            ]
        ]

-- | Address overview, we leave the error abstract (we are not using it)
addressOverview :: forall e. RemoteData e CAddressSummary -> Language -> P.Html Action
addressOverview NotAsked    lang = emptyAddressDetail ""
addressOverview Loading     lang = emptyAddressDetail <<< translate (I18nL.common <<< I18nL.cLoading) $ lang
addressOverview (Failure _) lang = failureView lang
addressOverview (Success addressSummary) lang =
    P.div
        [ P.className "address-overview"]
        [ addressDetailView addressSummary lang
        , addressQr addressSummary lang
        ]

addressDetailView :: CAddressSummary -> Language -> P.Html Action
addressDetailView addressSummary lang =
    P.div
        [ P.className "address-detail" ]
        $ map addressDetailRow $ addressDetailRowItems addressSummary lang

addressQr :: CAddressSummary -> Language -> P.Html Action
addressQr _ lang =
    P.div
      [ P.className "address-qr" ]
      [ P.p
          [ P.className "address-qr__tab" ]
          [ P.text $ translate (I18nL.address <<< I18nL.addQrCode) lang  ]
      , P.div
          [ P.className "address-qr__wrapper" ]
          [ P.div
              [ P.className "address-qr__image"
              , P.id_ addressQRImageId
              ]
              []
            , P.p
                [ P.className "address-qr__description" ]
                [ P.text $ translate (I18nL.address <<< I18nL.addScan) lang ]
          ]
      ]

type SummaryRowItem =
    { label :: String
    , value :: String
    , currency :: Maybe CCurrency
    }

type SummaryItems = Array SummaryRowItem

addressDetailRowItems :: CAddressSummary -> Language -> SummaryItems
addressDetailRowItems (CAddressSummary address) lang =
    [ { label: translate (I18nL.common <<< I18nL.cAddress) lang
      , value: address ^. (caAddress <<< _CAddress)
      , currency: Nothing
    }
    , { label: translate (I18nL.common <<< I18nL.cTransactions) lang
      , value: show $ address ^. caTxNum
      , currency: Nothing
    }
    , { label: translate (I18nL.address <<< I18nL.addFinalBalance) lang
      , value: address ^. (caBalance <<< _CCoin <<< getCoin)
      , currency: Just ADA
      }
    ]

addressDetailRow :: SummaryRowItem -> P.Html Action
addressDetailRow item =
    P.div
        [ P.className "address-detail__row" ]
        [ P.div
            [ P.className "address-detail__column label" ]
            [ P.text item.label ]
        , P.div
              [ P.className $ "address-detail__column amount" <> currencyCSSClass item.currency ]
              [ P.text item.value ]
        ]

emptyAddressDetail :: String -> P.Html Action
emptyAddressDetail message =
    P.div
        [ P.className "message" ]
        [ P.div
            [ P.dangerouslySetInnerHTML message ]
            []
        ]

emptyAddressTxView :: P.Html Action
emptyAddressTxView =
    P.div
        [ P.className "explorer-address__container" ]
        []

failureView :: Language -> P.Html Action
failureView lang =
    P.div
        []
        [ P.p
            [ P.className "address-failed" ]
            [ P.text $ translate (I18nL.address <<< I18nL.addNotFound) lang ]
        , P.link (toUrl Dashboard)
            [ P.className "btn-back" ]
            [ P.text $ translate (I18nL.common <<< I18nL.cBack2Dashboard) lang ]
        ]
