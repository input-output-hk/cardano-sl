module Explorer.View.Header (headerView) where

import Prelude
import Data.Lens ((^.))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (blockchain, charts, home, market, nav, tools) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Pux.Html (Html, div, text, header, nav, a, select, option) as P
import Pux.Html.Attributes (value)
import Pux.Html.Attributes (className, href) as P

headerView :: State -> P.Html Action
headerView state = do
    let lang = state.lang
    P.header
        [ P.className "explorer-header"]
        [ P.div
            [ P.className "explorer-header__top"]
            [ P.div
                [ P.className "explorer-header__container" ]
                [ P.div
                    [ P.className "logo__container"]
                    [ P.div
                        [ P.className "logo__wrapper"]
                        [ P.div
                            [ P.className "logo__img bg-logo" ]
                            []
                        ]
                    ]
                , P.div
                    [ P.className "nav__container" ]
                    [ navigationView state ]
                , P.div
                    [P.className "currency__container"]
                    [ currencyView state ]
                ]

            ]
        ]

-- currency

type CurrencyItem =
    { label :: String
    , value :: String
    }

currencyItems :: Array CurrencyItem
currencyItems =
    [ { label: "#ADA", value: "" }
    , { label: "#BC", value: "" }
    ]

currencyView :: State -> P.Html Action
currencyView state =
  P.select
      [ P.className "currency__select bg-arrow-down" ]
      $ map currencyItemView currencyItems

currencyItemView :: CurrencyItem -> P.Html Action
currencyItemView item =
  P.option
    [ value item.value ]
    [ P.text item.label ]


-- navigation

type NavItem =
    { link :: String
    , label :: String
    }

mkNavItems :: Language -> Array NavItem
mkNavItems lang =
    [ { link: "#", label: translate (I18nL.nav <<< I18nL.home) lang }
    , { link: "#", label: translate (I18nL.nav <<< I18nL.blockchain) lang }
    , { link: "#", label: translate (I18nL.nav <<< I18nL.market) lang }
    , { link: "#", label: translate (I18nL.nav <<< I18nL.charts) lang }
    , { link: "#", label: translate (I18nL.nav <<< I18nL.tools) lang }
    ]

navigationView :: State -> P.Html Action
navigationView state =
    P.nav
      [ P.className "nav__list"]
      $ map navItemView <<< mkNavItems $ state ^. lang


navItemView :: NavItem -> P.Html Action
navItemView item =
  P.a
      [ P.className "nav__item", P.href item.link ]
      [ P.text item.label ]
