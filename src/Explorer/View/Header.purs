module Explorer.View.Header (headerView) where

import Prelude
import Explorer.I18n.Lang (Language, I18nAccessor, translate)
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
    , i18nAccessor :: I18nAccessor
    }

navItems :: Array NavItem
navItems =
    [ { link: "#", i18nAccessor: _.nav.home }
    , { link: "#", i18nAccessor: _.nav.blockchain }
    , { link: "#", i18nAccessor: _.nav.market }
    , { link: "#", i18nAccessor: _.nav.charts }
    , { link: "#", i18nAccessor: _.nav.tools }
    ]

navigationView :: State -> P.Html Action
navigationView state =
    P.nav
      [ P.className "nav__list"]
      $ map (navItemView state.lang) navItems


navItemView :: Language -> NavItem -> P.Html Action
navItemView lang item =
  P.a
      [ P.className "nav__item", P.href item.link ]
      [ P.text $ translate item.i18nAccessor lang ]
