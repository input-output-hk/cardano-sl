module Explorer.View.Layout where

import Pux.Html (Html, div, h1, text, footer, ul, li, header)
import Pux.Html.Attributes (className)
import Prelude hiding (div)
import Explorer.Util.Version (version, commitHash)
import Explorer.State (State, Action)
import Explorer.View.Dashboard (dashboardView)
import Explorer.I18n.Lang (translate)

view :: State -> Html Action
view state =
    div [ className "app-container mdl-layout"]
        [ headerView state
        , div [ className "content" ]
              [ dashboardView state ]
        , footerView state
        ]

headerView :: State -> Html Action
headerView state =
    header  [ className "mdl-layout__header"]
            [ div [ className "mdl-layout__header-row" ]
                  [ h1 []
                    [ text $ translate _.welcome state.lang ]
                  ]
            ]

footerView :: State -> Html Action
footerView state =
    footer [ className "mdl-mega-footer" ]
    [ div [ className "mdl-mega-footer__bottom-section" ]
          [ ul  [ className "mdl-mega-footer__link-list" ]
                [ li [] [ text $ "version: " <> show version ]
                , li [] [ text $ "commit: " <> commitHash ]
                , li [] [ text $ "lang: " <> show state.lang ]
        ]
      ]
    ]
