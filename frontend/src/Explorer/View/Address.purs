module Explorer.View.Address where

import Prelude
import Data.Array (length, (!!))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cAddress, common, cOf, cTransactions, address, addScan, addQrCode, addFinalBalance) as I18nL
import Explorer.Lenses.State (addressDetail, addressTxPagination, currentAddressSummary, lang, viewStates)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), State)
import Explorer.Util.DOM (targetToHTMLInputElement)
import Explorer.Util.Factory (mkCTxEntryFrom, mkEmptyCTxEntry)
import Explorer.View.Common (currencyCSSClass, transactionBodyView, transactionHeaderView, transactionPaginationView)
import Pos.Core.Lenses.Types (_Coin, getCoin)
import Pos.Explorer.Web.ClientTypes (CAddressSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, caAddress, caBalance, caTxList, caTxNum)
import Pux.Html (Html, div, text, h3, p) as P
import Pux.Html.Attributes (className, id_) as P
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
                  , case state ^. currentAddressSummary of
                      Nothing ->
                          P.div
                              [ P.className "address-wrapper" ]
                              [ P.text "" ]
                      Just addressSummary ->
                          P.div
                              [ P.className "address-wrapper" ]
                              [ addressDetailView addressSummary lang'
                              , addressQr addressSummary lang'
                              ]

                      ]
                  ]
            , P.div
                [ P.className "explorer-address__wrapper" ]
                [ P.div
                      [ P.className "explorer-address__container" ]
                      [ P.h3
                          [ P.className "headline"]
                          [ P.text $ translate (I18nL.common <<< I18nL.cTransactions) lang' ]
                        , case state ^. currentAddressSummary of
                              Nothing ->
                                  -- TODO (jk) Use `emptyTxHeaderView` if #15 has been merged
                                  P.div
                                      [ P.className "transaction-header"]
                                      [ ]
                              Just (CAddressSummary addressSummary) ->
                                  let txList = addressSummary ^. caTxList
                                      txPagination = state ^. (viewStates <<< addressDetail <<< addressTxPagination)
                                  in
                                  P.div
                                      []
                                      [ transactionHeaderView $ case txList !! (txPagination - 1) of
                                                                Nothing -> mkEmptyCTxEntry
                                                                Just txBrief -> mkCTxEntryFrom txBrief
                                      , transactionBodyView state
                                      , transactionPaginationView
                                            { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
                                            , currentPage: 1
                                            , maxPage: (length txList) + 1
                                            , changePageAction: AddressPaginateTxs
                                            , onFocusAction: SelectInputText <<< targetToHTMLInputElement
                                            }
                                      ]

                      ]
                ]
            ]


addressDetailView :: CAddressSummary -> Language -> P.Html Action
addressDetailView addressSummary lang =
    P.div
        [ P.className "address-detail" ]
        $ map addressDetailRow $ addressDetailRowItems addressSummary lang

addressQr :: CAddressSummary -> Language -> P.Html Action
addressQr _ lang =
    P.div
      [ P.className "qr" ]
      [ P.p
          [ P.className "tab" ]
          [ P.text $ translate (I18nL.address <<< I18nL.addQrCode) lang  ]
      , P.div
          [ P.className "qr__wrapper" ]
          [ P.div
              [ P.className "qr__image"
              , P.id_ "qr_image_id"
              ]
              []
            , P.p
                [ P.className "qr__description" ]
                [ P.text $ translate (I18nL.address <<< I18nL.addScan) lang ]
          ]
      ]

type SummaryRowItem =
    { label :: String
    , value :: String
    , currency :: Maybe CCurrency
    , link :: Maybe String
    }

type SummaryItems = Array SummaryRowItem

addressDetailRowItems :: CAddressSummary -> Language -> SummaryItems
addressDetailRowItems (CAddressSummary address) lang =
    [ { label: translate (I18nL.common <<< I18nL.cAddress) lang
      , value: address ^. (caAddress <<< _CAddress)
      , currency: Nothing
      , link: Just <<< toUrl <<< Address $ address ^. caAddress
    }
    , { label: translate (I18nL.common <<< I18nL.cTransactions) lang
      , value: show $ address ^. caTxNum
      , currency: Nothing
      , link: Nothing
    }
    , { label: translate (I18nL.address <<< I18nL.addFinalBalance) lang
      , value: show $ address ^. (caBalance <<< _Coin <<< getCoin)
      , currency: Just ADA
      , link: Nothing
      }
    ]

addressDetailRow :: SummaryRowItem -> P.Html Action
addressDetailRow item =
    P.div
        [ P.className "address-detail__row" ]
        [ P.div
            [ P.className "address-detail__column column label" ]
            [ P.text item.label ]
        , P.div
              [ P.className $ "address-detail__column amount" <> currencyCSSClass item.currency ]
              [ case item.link of
                    Nothing ->
                        P.text item.value
                    Just link ->
                        P.link link
                            [ P.className "link" ]
                            [ P.text item.value ]
              ]
        ]
