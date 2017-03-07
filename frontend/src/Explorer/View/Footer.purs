module Explorer.View.Footer (footerView) where

import Prelude
import Data.Maybe (fromMaybe)
import Data.String (take)
import Data.Lens ((^.))
import Explorer.I18n.Lang (Language(..), readLanguage, translate)
import Explorer.I18n.Lenses (common, cCopyright, cApi, footer, fooLinks, fooRessources, fooFollow) as I18nL
import Explorer.State (initialState)
import Explorer.Lenses.State (lang)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Explorer.Util.Version (version, commitHash)
import Pux.Html (Html, div, text, nav, a, select, option, p) as P
import Pux.Html.Attributes (value, className, href, selected) as P
import Pux.Html.Events (onChange) as P


footerView :: State -> P.Html Action
footerView state =
    let lang' = state ^. lang in
    P.div [ P.className "explorer-footer" ]
    [
      P.div
          [ P.className "explorer-footer__top" ]
          [ P.div
              [ P.className "explorer-footer__container" ]
              [ P.div
                  [ P.className "logo__container"]
                  [ P.div
                      [ P.className "logo__img bg-logo" ]
                      []
                  ]
              , P.nav
                  [ P.className "nav__container"]
                  [ navRowView $ navRow0 lang'
                  , navRowView $ navRow1 lang'
                  , navRowView $ navRow2 lang'
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
                      [ P.text $ translate (I18nL.common <<< I18nL.cCopyright) lang'
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

navRow0 :: Language -> NavRow
navRow0 lang =
    { header: translate (I18nL.footer <<< I18nL.fooRessources) lang
    , items: navItemsRow0 lang }


navItemsRow0 :: Language -> Array NavItem
navItemsRow0 lang =
    [ { label: translate (I18nL.common <<< I18nL.cApi) lang
      , link: "#"
      }
    , { label: "#Link 2"
      , link: "#"
      }
    , { label: "#Link 3"
      , link: "#"
      }
    , { label: "#Link 4"
      , link: "#"
      }
    ]

navRow1 :: Language -> NavRow
navRow1 lang =
    { header: translate (I18nL.footer <<< I18nL.fooFollow) lang
    , items: navItemsRow1 lang }

navItemsRow1 :: Language -> Array NavItem
navItemsRow1 lang =
    [ { label: "#IOHK Site", link: "#" }
    , { label: "#Link 2", link: "#" }
    ]

navRow2 :: Language -> NavRow
navRow2 lang =
    { header: translate (I18nL.footer <<< I18nL.fooLinks) lang
    , items: navItemsRow2 lang }

navItemsRow2 :: Language -> Array NavItem
navItemsRow2 lang =
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

langItems :: Array Language
langItems =
    [ English
    , German
    ]

langView :: State -> P.Html Action
langView state =
  P.select
      [ P.className "lang__select bg-arrow-up"
      , P.onChange $ SetLanguage <<< fromMaybe initialState.lang <<< readLanguage <<< _.value <<< _.target]
      $ map (langItemView state) langItems

langItemView :: State -> Language -> P.Html Action
langItemView state lang =
  P.option
    [ P.value $ show lang
    , P.selected $ state.lang == lang  ]
    [ P.text $ show lang ]
