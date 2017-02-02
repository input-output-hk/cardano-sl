module Explorer.Types.State where

import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Prelude (class Eq, class Ord)

-- Add all State types here to generate lenses from it

type State =
    { lang :: Language
    , route :: Route
    , viewStates :: ViewStates
    }

type ViewStates =
    { dashboard :: DashboardViewState
    }

type DashboardViewState =
    { blocksExpanded :: Boolean
    , transactionsExpanded :: Boolean
    , selectedApiCode :: DashboardAPICode
    , searchInput :: Boolean
    }

data DashboardAPICode = Curl | Node | JQuery

derive instance eqDashboardAPICode :: Eq DashboardAPICode
derive instance ordDashboardAPICode :: Ord DashboardAPICode
