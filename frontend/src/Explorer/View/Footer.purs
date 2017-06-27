module Explorer.View.Footer (footerView) where

import Prelude
import Data.Foldable (for_)
import Data.Lens ((^.))
import Data.Monoid (mempty)
import Data.String (take)
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (common, cApi, footer, fooLinks, fooResources, fooFollow, fooIohkSupportP, fooDocumentation, fooGithub, fooLinkedin, fooTwitter, fooDaedalusWallet, fooWhyCardano, fooCardanoRoadmap, fooCardanoADAFaucet, fooCardanoSLDocumentation) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.Util.Config (commitHash, version)
import Explorer.View.Common (langView)

import Text.Smolder.HTML (div, nav, a, p, span) as S
import Text.Smolder.HTML.Attributes (className, href) as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((!))

import Pux.DOM.HTML (HTML) as P

footerView :: State -> P.HTML Action
footerView state =
    let lang' = state ^. lang in
    S.div ! S.className "explorer-footer" $ do
        S.div ! S.className "explorer-footer__top" $ do
            S.div ! S.className "explorer-footer__container" $ do
                S.nav ! S.className "nav__container" $ do
                    navRowView $ resourcesNavRow lang'
                    navRowView $ followUsNavRow lang'
                    navRowView $ linksNavRow lang'
        S.div ! S.className "explorer-footer__bottom" $ do
            S.div ! S.className "explorer-footer__container" $ do
                S.div ! S.className "content content__left" $ do
                    S.div ! S.className "logo__container" $ do
                        S.a ! S.className "logo__cardano-name bg-logo-cardano-name"
                            ! S.href "https://iohk.io/projects/cardano/"
                            $ mempty
                    S.span  ! S.className "split"
                            $ mempty
                    S.a ! S.className "support"
                        ! S.href "//iohk.io/projects/cardano/"
                        $ S.text (translate (I18nL.footer <<< I18nL.fooIohkSupportP) lang')
                    S.div ! S.className "logo__container"
                          $ S.a ! S.className "logo__iohk-name bg-iohk-logo"
                                ! S.href "https://iohk.io/"
                                $ mempty
                S.div ! S.className "content content__right"
                      $ langView state
            S.div ! S.className "explorer-footer__container explorer-footer__meta" $ do
                S.span  ! S.className "version"
                        $ S.text ("v. " <> (show version))
                S.a ! S.className "commit"
                    ! S.href ("https://github.com/input-output-hk/cardano-sl-explorer/commit/" <> commitHash)
                    $ S.text $ "( " <> (take 7 $ commitHash) <> " )"

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
    S.div ! S.className "nav-item__container" $ do
        S.p ! S.className "nav-item__header"
            $ S.text row.header
        S.div $ for_ row.items navRowItemView

navRowItemView :: NavItem -> P.HTML Action
navRowItemView item =
    S.a ! S.className "nav-item__item"
        ! S.href item.link
        $ S.text item.label
