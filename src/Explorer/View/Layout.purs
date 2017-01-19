module Explorer.View.Layout where

import Prelude

import Pux.Html (Html, div, h1, h2, text, footer, ul, li, header, main, nav, a) as P
import Pux.Html.Attributes (className, href) as P

import Explorer.I18n.Lang (translate)
import Explorer.Routes (Route(..))
import Explorer.State (State, Action)
import Explorer.Util.Version (version, commitHash)
import Explorer.View.Address (addressView)
import Explorer.View.Calculator (calculatorView)
import Explorer.View.Dashboard (dashboardView)
import Explorer.View.Transaction (transactionView)

view :: State -> P.Html Action
view state =
    P.div
      []
      [
        headerView state
        , P.div
            [ P.className "explorer-content" ]
            [ P.main
                [ P.className "mx-auto explorer-content__container" ]
                [ case state.route of
                      Dashboard -> dashboardView state
                      Transaction -> transactionView state
                      Address -> addressView state
                      Calculator -> calculatorView state
                      NotFound -> notFoundView
                ]
            ]
        , footerView state
      ]

headerView :: State -> P.Html Action
headerView state = do
    let lang = state.lang
    P.header
        [ P.className "explorer-header"]
        [ P.div
            [ P.className "mx-auto explorer-header__container" ]
            [ P.div
                [ P.className "explorer-header__nav" ]
                [ P.text "nav " ]
            , P.div
                [ P.className "explorer-header__hero" ]
                [ P.text "hero " ]
            ]
        ]
        --   P.div
        --     [ P.className "" ]
        --     [ P.div
        --         [ P.className "" ]
        --         [ P.div
        --             []
        --             [P.text "logo"]
        --         ]
        --     , P.nav
        --         [ P.className "" ]
        --         [ P.a
        --             [ P.className "", P.href "" ]
        --             [ P.text $ translate (_.nav.home) lang ]
        --         , P.a
        --             [ P.className "", P.href "" ]
        --             [ P.text $ translate (_.nav.blockchain) lang ]
        --         , P.a
        --             [ P.className "", P.href "" ]
        --             [ P.text $ translate (_.nav.market) lang ]
        --         , P.a
        --             [ P.className "", P.href "" ]
        --             [ P.text $ translate (_.nav.charts) lang ]
        --         , P.a
        --             [ P.className "", P.href "" ]
        --             [ P.text $ translate (_.nav.tools) lang ]
        --         ]
        --     , P.div
        --         [ P.className "" ]
        --         [ P.div
        --             []
        --             [P.text "curr-checkbox"]
        --         ]
        --     ]
        -- , P.div
        --     [ P.className "" ]
        --     [ P.div
        --         [P.className ""]
        --         [ P.h1
        --             [ P.className ""]
        --             [ P.text $ translate _.title state.lang ]
        --         , P.h2
        --             [ P.className ""]
        --             [ P.text $ translate _.subtitle lang ]
        --         ]
        --     ]

footerView :: State -> P.Html Action
footerView state =
    P.footer [ P.className "explorer-footer" ]
    [ P.div
        [ P.className "mx-auto explorer-footer__content" ]
        [ P.text "footer" ]
            -- P.ul  [ P.className "" ]
            --     [ P.li [] [ P.text $ "version: " <> show version ]
            --     , P.li [] [ P.text $ "commit: " <> commitHash ]
            --     , P.li [] [ P.text $ "lang: " <> show state.lang ]
        -- ]
    ]


notFoundView :: P.Html Action
notFoundView =
  P.div [] [
    P.h1 [] [P.text "404 Not Found"]
  ]
