module Explorer.View.Address where

import Prelude
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cAddress, common, cOf, cTransactions, address, addScan, addQrCode, addFinalBalance) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (CCurrency(..), State)
import Explorer.Util.DOM (targetToHTMLInputElement)
import Explorer.Util.Factory (mkEmptyCTxEntry, mkEmptyCAddressSummary)
import Explorer.View.Common (currencyCSSClass, transactionBodyView, transactionHeaderView, transactionPaginationView)
import Pos.Explorer.Web.ClientTypes (CAddressSummary(..))
import Pos.Explorer.Web.Lenses.ClientTypes (_CAddress, caAddress, caBalance, caTxNum)
import Pos.Types.Lenses.Core (_Coin, getCoin)
import Pux.Html (Html, div, text, h3, p, img) as P
import Pux.Html.Attributes (className, src) as P
import Pux.Router (link) as P

addressView :: State -> P.Html Action
addressView state =
    P.div
        [ P.className "explorer-address" ]
        [ P.div
            [ P.className "explorer-address__wrapper" ]
            [ P.div
                  [ P.className "explorer-address__container" ]
                  [ P.h3
                          [ P.className "headline"]
                          [ P.text $ translate (I18nL.common <<< I18nL.cAddress) lang' ]
                  -- FIXME (jk)
                  -- As long we dont have any live data
                  -- use following mock data.
                  -- Otherwise compare w/ currentAddressSummary as follow:
                  -- , case state ^. currentAddressSummary of
                  , case Just mkEmptyCAddressSummary of
                      Nothing ->
                          P.div
                              [ P.className "address-wrapper" ]
                              [ P.text "" ]
                      Just address ->
                          P.div
                              [ P.className "address-wrapper" ]
                              [ addressDetail address lang'
                              , addressQr address lang'
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
                        -- TODO (jk) use empty CTxEntry if we'll have real data
                        , transactionHeaderView mkEmptyCTxEntry
                        , transactionBodyView state
                        -- TODO (jk) use empty CTxEntry if we'll have real data
                        , transactionHeaderView mkEmptyCTxEntry
                        , transactionBodyView state
                        , transactionPaginationView paginationViewProps
                      ]
                ]
            ]
            where
                lang' = state ^. lang
                paginationViewProps =
                    { label: translate (I18nL.common <<< I18nL.cOf) $ lang'
                    , currentPage: 1
                    , maxPage: 1
                    , changePageAction: AddressPaginateTransactions
                    , onFocusAction: SelectInputText <<< targetToHTMLInputElement
                    }

addressDetail :: CAddressSummary -> Language -> P.Html Action
addressDetail address lang =
    P.div
        [ P.className "address-detail" ]
        $ map addressDetailRow $ addressDetailRowItems address lang

addressQr :: CAddressSummary -> Language -> P.Html Action
addressQr _ lang =
    P.div
      [ P.className "qr" ]
      [ P.p
          [ P.className "tab" ]
          [ P.text $ translate (I18nL.address <<< I18nL.addQrCode) lang  ]
      , P.div
          [ P.className "qr__wrapper" ]
          [ P.img
              [ P.className "qr__image"
              , P.src "" ]
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
