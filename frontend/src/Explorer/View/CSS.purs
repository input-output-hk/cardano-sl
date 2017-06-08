module Explorer.View.CSS
  ( blocksBody
  , blocksBodyWrapper
  , blocksBodyCover
  , blocksBodyCoverLabel
  , blocksBodyRow
  , blocksHeader
  , blocksColumnEpoch
  , blocksColumnSlot
  , blocksColumnAge
  , blocksColumnTxs
  , blocksColumnTotalSent
  , blocksColumnLead
  , blocksColumnSize
  , blocksFooter
  , blocksWaiting
  , blocksFailed
  , dashboardWrapper
  , dashboardContainer
  , dashBoardBlocksViewId
  , header
  , headerId
  , moveIn
  , moveOut
  , route
  )
  where

import Prelude
import Explorer.Routes (Route(..), addressLit, calculatorLit, dashboardLit, epochLit, notFoundLit, playgroundLit, slotLit, transactionLit)


-----------------------------------------------------------
-- BEM, meaning Block, Element, Modifier
-----------------------------------------------------------

element :: String
element = "__"

modifier :: String
modifier = "--"

-----------------------------------------------------------
-- misc
-----------------------------------------------------------

-- | Suffix to create an id
identifier :: String
identifier = "-id"

-----------------------------------------------------------
-- General
-----------------------------------------------------------

moveIn :: String
moveIn = "moveIn"

moveOut :: String
moveOut = "moveOut"

-----------------------------------------------------------
-- Dashboard
-----------------------------------------------------------

dashboard :: String
dashboard = "explorer-dashboard"

dashboardWrapper :: String
dashboardWrapper = dashboard <> element <> "wrapper"

dashboardContainer :: String
dashboardContainer = dashboard <> element <> "container"

dashBoardBlocksViewId :: String
dashBoardBlocksViewId = dashboard <> element <> "blocks-view" <> identifier

-- | blocks header

blocksHeader :: String
blocksHeader = "blocks-header"


-- | blocks body

blocksBody :: String
blocksBody = "blocks-body"
  --
blocksBodyWrapper :: String
blocksBodyWrapper = blocksBody <> element <> "wrapper"

blocksBodyCover :: String
blocksBodyCover = blocksBody <> element <> "cover"

blocksBodyCoverLabel :: String
blocksBodyCoverLabel = blocksBody <> element <> "cover-label"

blocksBodyRow :: String
blocksBodyRow = blocksBody <> element <> "row"


-- | blocks columns

blocksColumn :: String
blocksColumn = "blocks-column"

blocksColumnEpoch :: String
blocksColumnEpoch = blocksColumn <> element <> "epoch"

blocksColumnSlot :: String
blocksColumnSlot = blocksColumn <> element <> "slot"

blocksColumnAge :: String
blocksColumnAge = blocksColumn <> element <> "age"

blocksColumnTxs :: String
blocksColumnTxs = blocksColumn <> element <> "txs"

blocksColumnTotalSent :: String
blocksColumnTotalSent = blocksColumn <> element <> "totalSend"

blocksColumnLead :: String
blocksColumnLead = blocksColumn <> element <> "lead"

blocksColumnSize :: String
blocksColumnSize = blocksColumn <> element <> "size"

-- | blocks footer

blocksFooter :: String
blocksFooter = "blocks-footer"

-- | blocks misc.

blocksWaiting :: String
blocksWaiting = "blocks-waiting"

blocksFailed :: String
blocksFailed = "blocks-failed"


-----------------------------------------------------------
-- Header
-----------------------------------------------------------

header :: String
header = "explorer-header"

headerId :: String
headerId = header <> identifier

-----------------------------------------------------------
-- Routes
-----------------------------------------------------------

routePrefix :: String
routePrefix = "explorer-route-"

route :: Route -> String
route Dashboard = routePrefix <> dashboardLit
route (Tx id) = routePrefix <> transactionLit
route (Address address) = routePrefix <>  addressLit
route (EpochSlot epoch slot) = routePrefix <> epochLit <> "-" <> slotLit
route (Epoch epoch) = routePrefix <> epochLit
route Calculator = routePrefix <> calculatorLit
route (Block hash) = routePrefix <> slotLit
route Playground = routePrefix <> playgroundLit
route NotFound = routePrefix <> notFoundLit
