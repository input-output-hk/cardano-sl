module Explorer.View.Dashboard.Shared (headerView) where

import Data.Maybe (Maybe(..))
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.View.Dashboard.Types (HeaderLink(..), HeaderOptions(..))
import Pux.Html (Html, div, h3, text) as P
import Pux.Html.Attributes (className) as P

-- header view

headerView :: State -> HeaderOptions -> P.Html Action
headerView state (HeaderOptions options) =
    P.div
        [ P.className "explorer-dashboard__header" ]
        [ P.h3
            [ P.className "headline"]
            [ P.text options.headline ]
        , P.div
            [ P.className "more__container"]
            [ linkView options.link ]
        ]
    where
      linkView link = case link of
          Just (HeaderLink link') ->
              P.div
                  [ P.className "more__link bg-arrow-right" ]
                  [ P.text link'.label ]
          Nothing -> P.div [] []
