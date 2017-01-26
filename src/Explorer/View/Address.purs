module Explorer.View.Address where

import Prelude
import Data.Maybe (Maybe(..))
import Explorer.I18n.Lang (Language, translate)
import Explorer.Routes (Route(..), toUrl)
import Explorer.Types (Action, CCurrency(..), State)
import Explorer.View.Common (currencyCSSClass, transactionBodyView, transactionHeaderView, transactionPaginationView)
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
                          [ P.text $ translate _.address state.lang ]
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
                                [ P.text "#QR-Code" ]
                            , P.div
                                [ P.className "qr__wrapper" ]
                                [ P.img
                                    [ P.className "qr__image"
                                    , P.src "" ]
                                    []
                                  , P.p
                                      [ P.className "qr__description" ]
                                      [ P.text "#Scan this QR Code to copy address to clipboard"]
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
                          [ P.text $ translate _.transactions state.lang ]
                    , transactionHeaderView state
                    , transactionBodyView state
                    , transactionHeaderView state
                    , transactionBodyView state
                    , transactionPaginationView state
                  ]
            ]
        ]


-- FIXME (jk): just for now, will use later `real` ADTs
type AddressRowItem =
    { label :: String
    , amount :: String
    , currency :: Maybe CCurrency
    , link :: Boolean
    }

-- FIXME (jk): just for now, will use later `real` ADTs
type AddressItems = Array AddressRowItem

-- FIXME (jk): just for now, will use later `real` ADTs
addressItems :: Language -> AddressItems
addressItems lang =
    [ { label: translate _.address lang, amount: "1NPj2Y8yswHLuw8Yr1FDdobKAW6WVkUZy9", currency: Nothing, link: true }
    , { label: translate _.transactions lang, amount: "177", currency: Nothing, link: false }
    , { label: "#Final Balance", amount: "243,583", currency: Just ADA, link: false }
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
            P.link (toUrl Address)
                [ P.className "link" ]
                [ P.text item'.amount ]
          else
              P.text item.amount
