module Explorer.Types.State where

import Explorer.I18n.Lang (Language)
import Explorer.Routes (Route)
import Pos.Explorer.Web.ClientTypes (CBlockEntry, CTxEntry)
import Prelude (class Eq, class Ord)

-- Add all State types here to generate lenses from it

type State =
    { lang :: Language
    , route :: Route
    , socket :: SocketState
    , viewStates :: ViewStates
    , latestBlocks :: CBlockEntries
    , latestTransactions :: CTxEntries
    , errors :: Errors
    }

type SocketState =
    { connected :: Boolean
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

type CBlockEntries = Array CBlockEntry
type CTxEntries = Array CTxEntry

type Errors = Array String
