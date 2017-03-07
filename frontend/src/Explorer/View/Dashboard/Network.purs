module Explorer.View.Dashboard.Network (networkView) where

import Prelude
import Data.Lens ((^.))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (cADA, dbTotalAmountOfTransactions, cTransactions, dbTotalAmountOf, dbTotalSupply, dbPriceSince, dbPriceForOne, dbPriceAverage, cNetwork, common, dashboard, dbNetworkDifficulty, dbNetworkDifficultyDescription, dbLastBlocks, dbLastBlocksDescription) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Explorer.Util.String (substitute)
import Pux.Html (Html, div, h3, text, h4, p) as P
import Pux.Html.Attributes (className) as P


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
    , { headline: translate (I18nL.dashboard <<< I18nL.dbNetworkDifficulty) lang
      , subheadline: "1,234,567,890.12"
      , description: translate (I18nL.dashboard <<< I18nL.dbNetworkDifficultyDescription) lang
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


networkView :: State -> P.Html Action
networkView state =
    let lang' = state ^. lang in
    P.div
        [ P.className "explorer-dashboard__wrapper" ]
        [ P.div
          [ P.className "explorer-dashboard__container" ]
          [ P.h3
                [ P.className "headline"]
                [ P.text $ translate (I18nL.common <<< I18nL.cNetwork) lang' ]
          , P.div
                [ P.className "explorer-dashboard__teaser" ]
                <<< map (networkItem state) $ networkItems lang'
          ]
        ]

networkItem :: State -> NetworkItem -> P.Html Action
networkItem state item =
    P.div
        [ P.className "teaser-item" ]
        [ P.h3
            [ P.className "teaser-item__headline" ]
            [ P.text item.headline ]
        , P.h4
              [ P.className $ "teaser-item__subheadline" ]
              [ P.text item.subheadline ]
        , P.p
              [ P.className $ "teaser-item__description" ]
              [ P.text item.description ]
        ]
