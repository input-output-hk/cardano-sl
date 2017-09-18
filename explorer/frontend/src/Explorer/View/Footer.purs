module Explorer.View.Footer (footerView) where

import Prelude

import Data.Foldable (for_)
import Data.Lens ((^.))
import Data.String (take)
import Explorer.I18n.Lang (Language, langCode, translate)
import Explorer.I18n.Lenses (footer, fooCardanoLaunch, fooCardanoOpenSource, fooCardanoHub, fooCardanoSlack, fooDisclaimerPt1, fooDisclaimerPt2, fooCardanoFoundation, fooIohkSupportP, fooCardanoDocumentation, fooCardanoTestnet, fooCardanoSource, fooCardanoFoundationYoutube, fooCardanoFoundationTwitter, fooDaedalusPlatform, fooWhyCardano, fooCardanoRoadmap, fooCardanoReddit, fooCardanoCommunity, fooIOHK, fooIOHKBlog, fooIOHKYoutube) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Explorer.Util.Config (commitHash, version)
import Explorer.View.Common (langItems)
import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div, nav, a, li, p, span, ul) as S
import Text.Smolder.HTML.Attributes (className, href) as S
import Text.Smolder.Markup ((!), (#!))
import Text.Smolder.Markup (text) as S

footerView :: State -> P.HTML Action
footerView state =
    let lang' = state ^. lang in
    S.div ! S.className "explorer-footer" $ do
        S.div ! S.className "explorer-footer__top" $ do
            S.div ! S.className "explorer-footer__container" $ do
                S.div ! S.className "explorer-footer__top--content" $ do
                    S.div ! S.className "content__container content__container--left" $ do
                        langView
                        S.p $
                            S.a ! S.className ""
                                ! S.href "https://opensource.org/licenses/MIT"
                                $ S.text (translate (I18nL.footer <<< I18nL.fooCardanoOpenSource) lang')
                        S.p ! S.className ""
                            $ S.text (translate (I18nL.footer <<< I18nL.fooDisclaimerPt1) lang')
                        S.p ! S.className ""
                            $ S.text (translate (I18nL.footer <<< I18nL.fooDisclaimerPt2) lang')
                    S.div ! S.className "content__container content__container--right"
                          $ S.nav ! S.className "nav__container nav__container--left" $ do
                                navListView $ navItemsLeft lang'
                                navListView $ navItemsRight lang'
        S.div ! S.className "explorer-footer__bottom" $ do
            S.div ! S.className "explorer-footer__container" $ do
                S.div ! S.className "logo__wrapper" $ do
                    S.div ! S.className "logo__container" $ do
                        S.a ! S.className "logo__cardano-name bg-logo-cardano-name"
                            ! S.href "//iohk.io/projects/cardano/"
                            $ S.text ""
                    S.span  ! S.className "split"
                            $ S.text ""
                    S.a ! S.className "support"
                        ! S.href "//iohk.io/projects/cardano/"
                        $ S.text (translate (I18nL.footer <<< I18nL.fooIohkSupportP) lang')
                    S.div ! S.className "logo__container"
                          $ S.a ! S.className "logo__iohk-name bg-iohk-logo"
                                ! S.href "//iohk.io/"
                                $ S.text ""
            S.div ! S.className "explorer-footer__container explorer-footer__meta" $ do
                S.span  ! S.className "version"
                        $ S.text ("v. " <> version)
                S.a ! S.className "commit"
                    ! S.href ("https://github.com/input-output-hk/cardano-sl/commit/" <> commitHash)
                    $ S.text $ "( " <> (take 7 $ commitHash) <> " )"

-- lang

langView :: P.HTML Action
langView =
    S.ul  ! S.className "lang-nav__container"
          $ for_ langItems langItemView

langItemView :: Language -> P.HTML Action
langItemView lang' =
    let flagClazz = "bg-icon-lang-" <> langCode lang' in
    S.li ! S.className ("lang-nav__item " <> flagClazz )
        #! P.onClick (const $ SetLanguage lang')
        $ S.text (show lang')

-- nav

type NavItem =
    { label :: String
    , link :: String
    }

navItemsLeft :: Language -> Array NavItem
navItemsLeft lang =
    [ { label: translate (I18nL.footer <<< I18nL.fooCardanoLaunch) lang
      , link: "https://cardanolaunch.com"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooCardanoDocumentation) lang
      , link: "//cardanodocs.com"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooCardanoRoadmap) lang
      , link: "https://cardanoroadmap.com"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooCardanoTestnet) lang
      , link: "https://tada.iohk.io"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooCardanoSource) lang
      , link: "https://github.com/input-output-hk/cardano-sl"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooCardanoFoundation) lang
      , link: "https://cardanofoundation.org"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooCardanoHub) lang
      , link: "https://cardanohub.org"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooWhyCardano) lang
      , link: "https://whycardano.com"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooCardanoFoundationYoutube) lang
      , link: "https://www.youtube.com/channel/UCbQ9vGfezru1YRI1zDCtTGg"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooCardanoFoundationTwitter) lang
      , link: "https://twitter.com/CardanoStiftung"
      }
    ]

navItemsRight :: Language -> Array NavItem
navItemsRight lang =
    [ { label: translate (I18nL.footer <<< I18nL.fooCardanoSlack) lang
      , link: "https://cardano.herokuapp.com"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooCardanoReddit) lang
      , link: "https://www.reddit.com/r/cardano/"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooCardanoCommunity) lang
      , link: "http://cardanohub.org"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooDaedalusPlatform) lang
      , link: "https://daedaluswallet.io"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooIOHK) lang
      , link: "https://iohk.io"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooIOHKBlog) lang
      , link: "https://iohk.io/blog/"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooIOHKYoutube) lang
      , link: "https://www.youtube.com/channel/UCBJ0p9aCW-W82TwNM-z3V2w"
      }
    ]

navListView :: Array NavItem -> P.HTML Action
navListView items =
    S.ul  ! S.className "nav__container"
          $ for_ items navItemView

navItemView :: NavItem -> P.HTML Action
navItemView item =
    S.a ! S.className "nav__item"
        ! S.href item.link
        $ S.text item.label
