module Explorer.View.CSS
  ( blocksBody
  , blocksBodyRow
  , blocksHeader
  , blocksColumnEpoch
  , blocksColumnSlot
  , blocksColumnAge
  , blocksColumnTxs
  , blocksColumnTotalSent
  , blocksColumnRelayedBy
  , blocksColumnSize
  , blocksFooter
  , blocksWaiting
  , blocksFailed
  , dashboardWrapper
  , dashboardContainer
  )
  where

import Prelude

-----------------------------------------------------------
-- BEM, meaning Block, Element, Modifier
-----------------------------------------------------------

element :: String
element = "__"

modifier :: String
modifier = "--"

-----------------------------------------------------------
-- Dashboard
-----------------------------------------------------------

dashbaord :: String
dashbaord = "explorer-dashboard"

dashboardWrapper :: String
dashboardWrapper = dashbaord <> element <> "wrapper"

dashboardContainer :: String
dashboardContainer = dashbaord <> element <> "container"


-- | blocks header

blocksHeader :: String
blocksHeader = "blocks-header"


-- | blocks body

blocksBody :: String
blocksBody = "blocks-body"

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

blocksColumnRelayedBy :: String
blocksColumnRelayedBy = blocksColumn <> element <> "relayedBy"

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
