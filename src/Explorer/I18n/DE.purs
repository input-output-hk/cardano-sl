module Explorer.I18n.DE where

import Explorer.I18n.Lenses (cCalculator)

translation =
    { common:
        { cBack: "Zurück"
        , cTransaction: "Transaktion"
        , cTransactions: "Transaktionen"
        , cTransactionFeed: "Transaktionen Feed"
        , cAddress: "Address"
        , cCalculator: "Rechner"
        , cVersion: "Version"
        , cSummary: "Zusammenfassung"
        , cBlock: "Block"
        , cHashes: "Hashes"
        , cHeight: "Höhe"
        , cAge: "Alter"
        , cTotalSent: "Insgesamt gesendet"
        , cRelayedBy: "Weitergegeben durch"
        , cSizeKB: "Größe (kB)"
        , cExpand: "Aufklappen"
        }
    , navigation:
        { navHome: "Home"
        , navBlockchain: "Blockchain"
        , navMarket: "Markt"
        , navCharts: "Charts"
        , navTools: "Tools"
        }
    , hero:
        { hrTitle: "Cardano Blockchain Explorer"
        , hrSubtitle: "Der effektivste Weg um das Kryptowährung Netzwerk zu erforschen."
        , hrSearch: "Suche Addresse, Block, Token"
        }
    , dashboard:
        { dbLastBlocks: "Aktuelle Blöcke"
        , dbExploreBlocks: "Blöcke erkunden"
        }
    , address:
        { addScan: "Scan this QR Code to copy address to clipboard"
        }
    }
