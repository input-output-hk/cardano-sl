module Explorer.I18n.EN where

import Explorer.I18n.Lenses (cCalculator)

translation =
    { common:
        { cBack: "Back"
        , cTransaction: "Transaction"
        , cTransactions: "Transactions"
        , cTransactionFeed: "Transactions Feed"
        , cAddress: "Address"
        , cVersion: "version"
        , cCalculator: "Calculator"
        , cSummary: "Summary"
        , cBlock: "Block"
        , cHashes: "Hashes"
        , cHeight: "Height"
        , cAge: "Age"
        , cTotalSent: "Total Sent"
        , cRelayedBy: "Relayed by"
        , cSizeKB: "Size (kB)"
        , cExpand: "Expand"
        }
    , navigation:
        { navHome: "Home"
        , navBlockchain: "Blockchain"
        , navMarket: "Market"
        , navCharts: "Charts"
        , navTools: "Tools"
        }
    , hero:
        { hrTitle: "Cardano Blockchain Explorer"
        , hrSubtitle: "most effective way to reasearch crypto currencies network"
        , hrSearch: "Search for address, block, token"
        }
    , dashboard:
        { dbLastBlocks: "Last blocks"
        , dbExploreBlocks: "Explore blocks"
        }
    , address:
        { addScan: "Scan this QR Code to copy address to clipboard"
        }
    }
