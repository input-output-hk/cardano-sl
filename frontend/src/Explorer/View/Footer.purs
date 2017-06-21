module Explorer.View.Footer (footerView) where

import Prelude
import Data.Lens ((^.))
import Data.String (take)
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (common, cApi, footer, fooLinks, fooResources, fooFollow, fooIohkSupportP, fooDocumentation, fooGithub, fooLinkedin, fooTwitter, fooDaedalusWallet, fooWhyCardano, fooCardanoRoadmap, fooCardanoADAFaucet, fooCardanoSLDocumentation) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.Util.Config (commitHash, version)
import Explorer.View.Common (langView)

import Text.Smolder.HTML (div, text, nav, a, p, span)
import Text.Smolder.HTML.Attributes (className, href)
import Text.Smolder.Markup (text, (#!))

import Pux.DOM.HTML (HTML) as P

footerView :: State -> P.HTML Action
footerView state =
    let lang' = state ^. lang in
    div ! className "explorer-footer" $ do
        div ! className "explorer-footer__top" $ do
            div ! className "explorer-footer__container" $ do
                nav ! className "nav__container" $ do
                    navRowView $ resourcesNavRow lang'
                    navRowView $ followUsNavRow lang'
                    navRowView $ linksNavRow lang'
        div ! className "explorer-footer__bottom" $ do
            div ! className "explorer-footer__container" $ do
                div ! className "content content__left" $ do
                    div ! className "logo__container" $ do
                      a ! className "logo__cardano-name bg-logo-cardano-name"
                        ! href "https://iohk.io/projects/cardano/"
                    span ! className "split"
                    a ! className "support"
                      ! href "//iohk.io/projects/cardano/"
                      $ text (translate (I18nL.footer <<< I18nL.fooIohkSupportP) lang')
                    div ! className "logo__container" $ do
                        a ! className "logo__iohk-name bg-iohk-logo"
                          ! href "https://iohk.io/"
                div ! className "content content__right" $ do
                    langView stat
            div ! className "explorer-footer__container explorer-footer__meta" $ do
                span  ! className "version"
                      $ text ("v. " <> (show version))
                a ! className "commit"
                  ! href ("https://github.com/input-output-hk/cardano-sl-explorer/commit/" <> commitHash)
                  $ text $ "( " <> (take 7 $ commitHash) <> " )"

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
    { header: translate (I18nL.footer <<< I18nL.fooResources) lang
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
      , link: "https://cardanodocs.com/introduction/"
      }
    ]

navRowView :: NavRow -> P.HTML Action
navRowView row =
    div ! className "nav-item__container" $ do
        p ! className "nav-item__header"
          $ text row.header
        div do
            map navRowItemView row.items

navRowItemView :: NavItem -> P.HTML Action
navRowItemView item =
    a ! className "nav-item__item"
      ! href item.link
      $ text item.label
