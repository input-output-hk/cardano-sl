module Explorer.View.Footer (footerView) where

import Prelude
import Data.Lens ((^.))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (common, cApi, footer, fooLinks, fooRessources, fooFollow, fooIohkSupportP, fooDocumentation, fooGithub, fooLinkedin, fooTwitter, fooDaedalusWallet, fooWhyCardano, fooCardanoRoadmap, fooCardanoADAFaucet, fooCardanoSLDocumentation) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.View.Common (langView)
import Pux.Html (Html, div, text, nav, a, p, span) as P
import Pux.Html.Attributes (className, href) as P


footerView :: State -> P.Html Action
footerView state =
    let lang' = state ^. lang in
    P.div [ P.className "explorer-footer" ]
    [
      P.div
          [ P.className "explorer-footer__top" ]
          [ P.div
              [ P.className "explorer-footer__container" ]
              [ P.nav
                  [ P.className "nav__container"]
                  [ navRowView $ resourcesNavRow lang'
                  , navRowView $ followUsNavRow lang'
                  , navRowView $ linksNavRow lang'
                  ]
              ]
          ]
      , P.div
          [ P.className "explorer-footer__bottom" ]
          [ P.div
              [ P.className "explorer-footer__container" ]
              [ P.div
                  [ P.className "content content__left" ]
                  [ P.div
                      [ P.className "logo__container"]
                      [ P.a
                          [ P.className "logo__cardano-name bg-logo-cardano-name"
                          , P.href "https://iohk.io/projects/cardano/"]
                          []
                      ]
                  , P.span
                      [ P.className "split" ]
                      []
                  , P.a
                      [ P.className "support", P.href "//iohk.io/projects/cardano/"]
                      [ P.text $ translate (I18nL.footer <<< I18nL.fooIohkSupportP) lang' ]
                  , P.div
                      [ P.className "logo__container"]
                      [ P.a
                          [ P.className "logo__iohk-name bg-iohk-logo"
                          , P.href "https://iohk.io/"]
                          []
                      ]
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

resourcesNavRow :: Language -> NavRow
resourcesNavRow lang =
    { header: translate (I18nL.footer <<< I18nL.fooRessources) lang
    , items: navItemsRow0 lang }


navItemsRow0 :: Language -> Array NavItem
navItemsRow0 lang =
    [ { label: translate (I18nL.common <<< I18nL.cApi) lang
      , link: "https://github.com/input-output-hk/cardano-sl-explorer/blob/master/docs/cardano-explorer-table-web-api.md"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooDocumentation) lang
      , link: "https://github.com/input-output-hk/cardano-sl-explorer/blob/master/docs/cardano-explorer-web-api.md"
      }
    -- TODO (ks) Add when we have the links
    -- , { label: "Support"
    --   , link: "/"
    --   }
    -- , { label: "Status"
    --   , link: "/"
    --   }
    -- , { label: "Charts"
    --   , link: "/"
    --   }
    ]

followUsNavRow :: Language -> NavRow
followUsNavRow lang =
    { header: translate (I18nL.footer <<< I18nL.fooFollow) lang
    , items: navItemsRow1 lang
    }

navItemsRow1 :: Language -> Array NavItem
navItemsRow1 lang =
    [ { label: translate (I18nL.footer <<< I18nL.fooGithub) lang
      , link: "https://github.com/input-output-hk/"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooLinkedin) lang
      , link: "https://www.linkedin.com/company-beta/6385405/?pathWildcard=6385405"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooTwitter) lang
      , link: "https://twitter.com/InputOutputHK"
      }
    ]

linksNavRow :: Language -> NavRow
linksNavRow  lang =
    { header: translate (I18nL.footer <<< I18nL.fooLinks) lang
    , items: navItemsRow2 lang
    }

navItemsRow2 :: Language -> Array NavItem
navItemsRow2 lang =
    [ { label: translate (I18nL.footer <<< I18nL.fooDaedalusWallet) lang
      , link: "https://daedaluswallet.io/"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooWhyCardano) lang
      , link: "https://whycardano.com/"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooCardanoRoadmap) lang
      , link: "https://cardanoroadmap.com/"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooCardanoADAFaucet) lang
      , link: "https://tada.iohk.io/"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooCardanoSLDocumentation) lang
      , link: "https://cardano-docs.iohk.io/introduction/"
      }
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
