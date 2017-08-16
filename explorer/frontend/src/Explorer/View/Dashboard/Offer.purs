module Explorer.View.Dashboard.Offer (offerView) where

import Prelude hiding (div)

import Data.Foldable (for_)
import Data.Lens ((^.))

import Explorer.I18n.Lang (Language, translate)
import Explorer.I18n.Lenses (dbApiDescription
  , dbAddressSearch, dbAddressSearchDescription, dbBlockchainOffer, dbBlockSearch
  , dbBlockSearchDescription, dbTransactionSearch, dbTransactionSearchDescription
  , cApi , common, dashboard) as I18nL
import Explorer.Lenses.State (lang)
import Explorer.Types.Actions (Action)
import Explorer.Types.State (State)

import Pux.DOM.HTML (HTML) as P

import Text.Smolder.HTML (div, h3, p) as S
import Text.Smolder.HTML.Attributes (className) as S
import Text.Smolder.Markup (text) as S
import Text.Smolder.Markup ((!))

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

offerView :: State -> P.HTML Action
offerView state =
    let lang' = state ^. lang in
    S.div ! S.className "explorer-dashboard__wrapper"
          $ S.div ! S.className "explorer-dashboard__container" $ do
                  S.h3  ! S.className "headline"
                        $ S.text (translate (I18nL.dashboard <<< I18nL.dbBlockchainOffer) lang')
                  S.div ! S.className "explorer-dashboard__teaser"
                        $ for_ (offerItems lang') (offerItem state)

offerItem :: State -> OfferItem -> P.HTML Action
offerItem state item =
    S.div ! S.className "teaser-item" $ do
          S.h3  ! S.className "teaser-item__headline"
                $ S.text item.headline
          S.p   ! S.className "teaser-item__description"
                $ S.text item.description
