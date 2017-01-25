module Explorer.View.Footer (footerView) where

import Prelude
import Data.String (take)
import Explorer.Types (State, Action)
import Explorer.Util.Version (version, commitHash)
import Pux.Html (Html, div, text, nav, a, select, option, p) as P
import Pux.Html.Attributes (value)
import Pux.Html.Attributes (className, href) as P


footerView :: State -> P.Html Action
footerView state =
    P.div [ P.className "explorer-footer" ]
    [
      P.div
          [ P.className "explorer-footer__top" ]
          [ P.div
              [ P.className "explorer-footer__container" ]
              [ P.div
                  [ P.className "logo__container"]
                  [ P.div
                      [ P.className "logo__wrapper"]
                      [  P.div
                            [ P.className "logo__img bg-logo" ]
                            []
                      ]
                  ]
              , P.nav
                  [ P.className "nav__container"]
                  [ navRowView navRow0
                  , navRowView navRow1
                  , navRowView navRow2
                  ]
              ]
          ]
      , P.div
          [ P.className "explorer-footer__bottom" ]
          [ P.div
              [ P.className "explorer-footer__container" ]
              [ P.div
                  [ P.className "content content__left" ]
                  [ P.p
                      [ P.className "copy" ]
                      [ P.text $ "#Cardano blockchain Explorer @ 2017"
                        <> " | v. "
                        <> show version
                        <> " | commit " <> take 8 commitHash ]
                  ]
              ,  P.div
                  [ P.className "content content__right"]
                  [ langView state ]
              ]
          ]
    ]

-- nav

type NavItem =
    { label :: String
    , link :: String
    }

type NaviItemHeader = String

type NavRow =
    { header :: String
    , items :: Array NavItem
    }

navRow0 :: NavRow
navRow0 =
    { header: "Ressources"
    , items: navItemsRow0 }


navItemsRow0 :: Array NavItem
navItemsRow0 =
    [ { label: "#API", link: "#" }
    , { label: "#Link 2", link: "#" }
    , { label: "#Link 3", link: "#" }
    , { label: "#Link 4", link: "#" }
    ]

navRow1 :: NavRow
navRow1 =
    { header: "Follow us"
    , items: navItemsRow1 }

navItemsRow1 :: Array NavItem
navItemsRow1 =
    [ { label: "#IOHK Site", link: "#" }
    , { label: "#Link 2", link: "#" }
    ]

navRow2 :: NavRow
navRow2 =
    { header: "Links"
    , items: navItemsRow2 }

navItemsRow2 :: Array NavItem
navItemsRow2 =
    [ { label: "#Most popular addresses", link: "#" }
    , { label: "#Link 2", link: "#" }
    , { label: "#Link 3", link: "#" }
    , { label: "#Link 4", link: "#" }
    , { label: "#Link 5", link: "#" }
    ]

navRowView :: NavRow -> P.Html Action
navRowView row =
    P.div
        [ P.className "nav-item__container"]
        [ P.p
            [ P.className "nav-item__header"]
            [ P.text row.header ]
          , P.div
            []
            $ map navRowItemView row.items
        ]

navRowItemView :: NavItem -> P.Html Action
navRowItemView item =
    P.a
      [ P.className "nav-item__item", P.href item.link]
      [ P.text item.label ]

-- currency

type LangItem =
    { label :: String
    , value :: String
    }

langItems :: Array LangItem
langItems =
    [ { label: "#English", value: "" }
    , { label: "#Deutsch", value: "" }
    ]

langView :: State -> P.Html Action
langView state =
  P.select
      [ P.className "lang__select bg-arrow-up" ]
      $ map langItemView langItems

langItemView :: LangItem -> P.Html Action
langItemView item =
  P.option
    [ value item.value ]
    [ P.text item.label ]
