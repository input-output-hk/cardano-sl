module Explorer.View.Dashboard.Shared (headerView) where

import Data.Maybe (Maybe(..))

import Pux.DOM.HTML (Html) as P
import Text.Smolder.HTML (div, h3, text)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (!))

import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.View.Common (emptyView)
import Explorer.View.Dashboard.Types (HeaderLink(..), HeaderOptions(..))

headerView :: State -> HeaderOptions -> P.Html Action
headerView state (HeaderOptions options) =
    div ! className "explorer-dashboard__header" $ do 
        h3  ! className "headline"
            $ text options.headline
        -- div
            -- ! className "more__container"
            -- $ linkView options.link
    where
      linkView link = case link of
          Just (HeaderLink link') ->
              div ! className "more__link bg-arrow-right"
                  $ text link'.label
          Nothing -> emptyView
