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
import Explorer.Util.Factory (mkCHash, mkCTxEntry)
import Explorer.View.Common (currencyCSSClass, transactionBodyView, transactionHeaderView, transactionPaginationView)
import Pos.Explorer.Web.ClientTypes (CHash)
import Pux.Html (Html, div, text, h3, p, img) as P
import Pux.Html.Attributes (className, src) as P
import Pux.Router (link) as P

addressView :: State -> CHash -> P.Html Action
addressView state hash =
    P.div
        [ P.className "explorer-address" ]
        [ P.div
            [ P.className "explorer-address__wrapper" ]
            [ P.div
                  [ P.className "explorer-address__container" ]
                  [ P.h3
                          [ P.className "headline"]
                          [ P.text $ translate (I18nL.common <<< I18nL.cAddress) lang' ]
                  , P.div
                      [ P.className "address-wrapper" ]
                      [ P.div
                      -- address
                          [ P.className "address-detail" ]
                          $ map addressDetailRow $ addressItems state.lang
                      -- qr
                      , P.div
                          [ P.className "qr" ]
                          [ P.p
                                [ P.className "tab" ]
                                [ P.text $ translate (I18nL.address <<< I18nL.addQrCode) lang'  ]
                            , P.div
                                [ P.className "qr__wrapper" ]
                                [ P.img
                                    [ P.className "qr__image"
                                    , P.src "" ]
                                    []
                                  , P.p
                                      [ P.className "qr__description" ]
                                      [ P.text $ translate (I18nL.address <<< I18nL.addScan) lang' ]
                                ]
                            ]
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
                    , transactionHeaderView mkCTxEntry
                    , transactionBodyView state
                    -- TODO (jk) use empty CTxEntry if we'll have real data
                    , transactionHeaderView mkCTxEntry
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


-- FIXME (jk): just for now, will use later `real` ADTs
type AddressRowItem =
    { id :: CHash
    , label :: String
    , amount :: String
    , currency :: Maybe CCurrency
    , link :: Boolean
    }

-- FIXME (jk): just for now, will use later `real` ADTs
type AddressItems = Array AddressRowItem

-- FIXME (jk): just for now, will use later `real` ADTs
addressItems :: Language -> AddressItems
addressItems lang =
    [ { id: mkCHash "0"
      , label: translate (I18nL.common <<< I18nL.cAddress) lang
      , amount: "1NPj2Y8yswHLuw8Yr1FDdobKAW6WVkUZy9"
      , currency: Nothing
      , link: true
    }
    , { id: mkCHash "1"
      , label: translate (I18nL.common <<< I18nL.cTransactions) lang
      , amount: "177"
      , currency: Nothing
      , link: false
    }
    , { id: mkCHash "2"
      , label: translate (I18nL.address <<< I18nL.addFinalBalance) lang
      , amount: "243,583"
      , currency: Just ADA
      , link: false
  }
    ]

addressDetailRow :: AddressRowItem -> P.Html Action
addressDetailRow item =
    P.div
        [ P.className "address-detail__row" ]
        [ P.div
            [ P.className "address-detail__column column label" ]
            [ P.text item.label ]
        , P.div
              [ P.className $ "address-detail__column amount" <> currencyCSSClass item.currency ]
              [ renderValue item ]
        ]
    where
      renderValue :: AddressRowItem -> P.Html Action
      renderValue item' = if item'.link == true
          then
            P.link (toUrl $ Address item'.id)
                [ P.className "link" ]
                [ P.text item'.amount ]
          else
              P.text item.amount
