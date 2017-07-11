
-- | View of a `genesis block` https://cardanodocs.com/technical/blocks/#genesis-block

module Explorer.View.Genesis
    ( genesisView
    )
    where

import Prelude

import Data.Lens ((^.))

import Explorer.I18n.Lang (translate)
import Explorer.I18n.Lenses (cBack, cCalculator, cGenesis, cSummary, common) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Types.State (State)
import Explorer.Types.Actions (Action(..))
import Explorer.Routes (Route(Dashboard), toUrl)
import Explorer.View.Common (placeholderView)

import Text.Smolder.HTML (a, div, h3) as S
import Text.Smolder.HTML.Attributes (className, href) as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((!))

import Pux.DOM.HTML (HTML) as P

genesisView :: State -> P.HTML Action
genesisView state =
    let lang' = state ^. lang
    in
    S.div ! S.className "explorer-genesis"$ do
        S.div ! S.className "explorer-block__wrapper"
              $ S.div ! S.className "explorer-genesis__container" $ do
                    S.h3  ! S.className "headline"
                        $ S.text (translate (I18nL.common <<< I18nL.cGenesis) lang')
        S.div ! S.className "explorer-genesis__wrapper" $ do
            S.div ! S.className "explorer-genesis__container" $ do
                S.h3  ! S.className "headline"
                      $ S.text (translate (I18nL.common <<< I18nL.cSummary) lang')
