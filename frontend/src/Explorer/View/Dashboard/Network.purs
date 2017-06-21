module Explorer.View.Dashboard.Network (networkView) where

import Prelude

import Data.Foldable (for_)
import Data.Lens ((^.))

import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cADA, dbTotalAmountOfTransactions, cTransactions, dbTotalAmountOf, dbTotalSupply, dbPriceSince, dbPriceForOne, dbPriceAverage, cNetwork, common, dashboard, dbLastBlocks, dbLastBlocksDescription) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.Util.String (substitute)

import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div, h3, h4, p)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (!))


-- FIXME (jk): just for now, will use later `real` ADTs
type NetworkItems = Array NetworkItem

-- FIXME (jk): just for now, will use later `real` ADTs
type NetworkItem =
    { headline :: String
    , subheadline :: String
    , description :: String
    }

networkItems :: Language -> NetworkItems
networkItems lang =
    let ada = translate (I18nL.common <<< I18nL.cADA) lang in
    [ { headline: translate (I18nL.dashboard <<< I18nL.dbLastBlocks) lang
      , subheadline: "123456"
      , description: flip substitute ["20.02.2017 17:51:00", "50"]
            $ translate (I18nL.dashboard <<< I18nL.dbLastBlocksDescription) lang
      }
    , { headline: translate (I18nL.dashboard <<< I18nL.dbPriceAverage) lang
      , subheadline: flip substitute ["1,000,000$", ada]
            $ translate (I18nL.dashboard <<< I18nL.dbPriceForOne) lang
      , description: flip substitute ["20% more"]
            $ translate (I18nL.dashboard <<< I18nL.dbPriceSince) lang
      }
    , { headline: translate (I18nL.dashboard <<< I18nL.dbTotalSupply) lang
      , subheadline: flip substitute ["9,876,543,210 "] $ translate (I18nL.common <<< I18nL.cADA) lang
      , description: flip substitute [ada] $ translate (I18nL.dashboard <<< I18nL.dbTotalAmountOf) lang
      }
    , { headline: translate (I18nL.common <<< I18nL.cTransactions) lang
      , subheadline: "82,491,247,592,742,929"
      , description: translate (I18nL.dashboard <<< I18nL.dbTotalAmountOfTransactions) lang
      }
    ]


networkView :: State -> P.HTML Action
networkView state =
    let lang' = state ^. lang in
    div ! className "explorer-dashboard__wrapper"
        $ div ! className "explorer-dashboard__container" $ do
              h3  ! className "headline"
                  $ text (translate (I18nL.common <<< I18nL.cNetwork) lang')
              div ! className "explorer-dashboard__teaser"
                  $ for_ (networkItems lang') (networkItem state)

networkItem :: State -> NetworkItem -> P.HTML Action
networkItem state item =
    div ! className "teaser-item" $ do
        h3  ! className "teaser-item__headline"
            $ text item.headline
        h4  ! className "teaser-item__subheadline"
            $ text item.subheadline
        p   ! className "teaser-item__description"
            $ text item.description
