module Explorer.View.NotFound (notFoundView) where

import Prelude
import Data.Lens ((^.))
import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (notfound, nfTitle, nfDescription, nfBack2Dashboard) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Routes (Route(Dashboard), toUrl)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Pux.Html (Html, div, h3, text, p) as P
import Pux.Html.Attributes (className) as P
import Pux.Router (link) as P


notFoundView :: State -> P.Html Action
notFoundView state =
    let lang' = state ^. lang in
    P.div
        [ P.className "explorer-404" ]
        [ P.div
            [ P.className "explorer-404__wrapper" ]
            [ P.div
                [ P.className "explorer-404__container" ]
                [ P.h3
                    [ P.className "headline"]
                    [ P.text $ translate (I18nL.notfound <<< I18nL.nfTitle) lang' ]
                ,  P.p
                      [ P.className "description"]
                      [ P.text $ translate (I18nL.notfound <<< I18nL.nfDescription) lang' ]
                , P.link (toUrl Dashboard)
                  [ P.className "btn-back" ]
                  [ P.text $ translate (I18nL.notfound <<< I18nL.nfBack2Dashboard) lang' ]
                ]
            ]
        ]
