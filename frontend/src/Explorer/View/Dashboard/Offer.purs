module Explorer.View.Dashboard.Offer (offerView) where

import Prelude

import Data.Lens ((^.))
import Pux.DOM.HTML (Html) as P
import Text.Smolder.HTML (div, h3, text, p)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (!))

import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (dbApiDescription
  , dbAddressSearch, dbAddressSearchDescription, dbBlockchainOffer, dbBlockSearch
  , dbBlockSearchDescription, dbTransactionSearch, dbTransactionSearchDescription
  , cApi , common, dashboard) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)

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
    div ! className "explorer-dashboard__wrapper" $ do
        div ! className "explorer-dashboard__container" $ ddo
            h3 ! className "headline"
               $ text (translate (I18nL.dashboard <<< I18nL.dbBlockchainOffer) lang')
            div ! className "explorer-dashboard__teaser"
                $ map (offerItem state) $ offerItems lang'

offerItem :: State -> OfferItem -> P.Html Action
offerItem state item =
    div ! className "teaser-item" $ do
        h3 ! className "teaser-item__headline"
           $ text item.headline
        p ! className "teaser-item__description"
          $ P.text item.description
