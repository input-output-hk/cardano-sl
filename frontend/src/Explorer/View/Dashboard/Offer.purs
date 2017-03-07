module Explorer.View.Dashboard.Offer (offerView) where

import Prelude
import Data.Lens ((^.))
import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (dbApiDescription
  , dbAddressSearch, dbAddressSearchDescription, dbBlockchainOffer, dbBlockSearch, dbBlockSearchDescription
  , dbTransactionSearch, dbTransactionSearchDescription, cApi
  , common, dashboard) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)
import Pux.Html (Html, div, h3, text, p) as P
import Pux.Html.Attributes (className) as P

-- FIXME (jk): just for now, will use later `real` ADTs
type OfferItems = Array OfferItem

-- FIXME (jk): just for now, will use later `real` ADTs
type OfferItem =
    { headline :: String
    , description :: String
    }

offerItems :: Language -> OfferItems
offerItems lang =
    [ { headline: translate (I18nL.dashboard <<< I18nL.dbBlockSearch) lang
      , description: translate (I18nL.dashboard <<< I18nL.dbBlockSearchDescription) lang
      }
    , { headline: translate (I18nL.dashboard <<< I18nL.dbAddressSearch) lang
      , description: translate (I18nL.dashboard <<< I18nL.dbAddressSearchDescription) lang
      }
    , { headline: translate (I18nL.dashboard <<< I18nL.dbTransactionSearch) lang
      , description: translate (I18nL.dashboard <<< I18nL.dbTransactionSearchDescription) lang
      }
    , { headline: translate (I18nL.common <<< I18nL.cApi) lang
      , description: translate (I18nL.dashboard <<< I18nL.dbApiDescription) lang
      }
    ]


offerView :: State -> P.Html Action
offerView state =
    let lang' = state ^. lang in
    P.div
        [ P.className "explorer-dashboard__wrapper" ]
        [ P.div
          [ P.className "explorer-dashboard__container" ]
          [ P.h3
                [ P.className "headline"]
                [ P.text $ translate (I18nL.dashboard <<< I18nL.dbBlockchainOffer) lang' ]
          , P.div
                [ P.className "explorer-dashboard__teaser" ]
                <<< map (offerItem state) $ offerItems lang'
          ]
        ]

offerItem :: State -> OfferItem -> P.Html Action
offerItem state item =
    P.div
        [ P.className "teaser-item" ]
        [ P.h3
            [ P.className "teaser-item__headline" ]
            [ P.text item.headline ]
        , P.p
              [ P.className $ "teaser-item__description" ]
              [ P.text item.description ]
        ]
