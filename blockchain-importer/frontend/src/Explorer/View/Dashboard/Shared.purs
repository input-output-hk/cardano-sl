module Explorer.View.Dashboard.Shared (headerView) where

import Prelude

import Data.Maybe (Maybe(..))

import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div, h3) as S
import Text.Smolder.HTML.Attributes (className) as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((!))

import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.View.Common (emptyView)
import Explorer.View.Dashboard.Types (HeaderLink(..), HeaderOptions(..))

headerView :: State -> HeaderOptions -> P.HTML Action
headerView state (HeaderOptions options) =
    S.div ! S.className "explorer-dashboard__header" $ do
          S.h3  ! S.className "headline"
                $ S.text options.headline
        -- S.div
            -- ! S.className "more__container"
            -- $ linkView options.link
    where
      linkView link = case link of
          Just (HeaderLink link') ->
              S.div ! S.className "more__link bg-arrow-right"
                    $ S.text link'.label
          Nothing -> emptyView
