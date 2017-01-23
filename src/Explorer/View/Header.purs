module Explorer.View.Header (headerView) where

import Prelude
import Explorer.Assets (logoPath)
import Explorer.I18n.Lang (Language, I18nAccessor, translate)
import Explorer.State (State, Action)
import Pux.Html (Html, div, text, header, nav, a, select, option, img) as P
import Pux.Html.Attributes (value)
import Pux.Html.Attributes (className, href, src) as P

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
                        [ P.img
                            [ P.className "logo__img", P.src logoPath ]
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
      [ P.className "currency__select" ]
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
      $ map (\i -> navItemView i state.lang) navItems


navItemView :: NavItem -> Language -> P.Html Action
navItemView item lang =
  P.a
      [ P.className "nav__item", P.href item.link ]
      [ P.text $ translate item.i18nAccessor lang ]
