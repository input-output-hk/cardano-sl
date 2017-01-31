module Explorer.Types.State where

import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Prelude (class Eq, class Ord)

type State =
    { lang :: Language
    , route :: Route
    , viewStates :: ViewStates
    }

type ViewStates =
    { dashboard :: DashBoardViewState
    }

type DashBoardViewState =
    { blocksExpanded :: Boolean
    , transactionsExpanded :: Boolean
    , selectedApiCode :: DashboardAPICode
    }

data DashboardAPICode = Curl | Node | JQuery

derive instance eqDashboardAPICode :: Eq DashboardAPICode
derive instance ordDashboardAPICode :: Ord DashboardAPICode
