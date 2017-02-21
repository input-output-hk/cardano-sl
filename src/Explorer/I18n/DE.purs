module Explorer.I18n.DE where

import Explorer.I18n.Lenses (cCalculator)

translation =
    { common:
        { cBack: "Zurück"
        , cADA: "ADA"
        , cBCshort: "BC"
        , cBCong: "Bitcoin"
        , cApi: "Api"
        , cTransaction: "Transaktion"
        , cTransactions: "Transaktionen"
        , cTransactionFeed: "Transaktionen Feed"
        , cAddress: "Address"
        , cCalculator: "Rechner"
        , cNetwork: "Netzwerk"
        , cVersion: "Version"
        , cSummary: "Zusammenfassung"
        , cBlock: "Block"
        , cHash: "Hash"
        , cHashes: "Hashes"
        , cHeight: "Höhe"
        , cAge: "Alter"
        , cTotalSent: "Insgesamt gesendet"
        , cRelayedBy: "Weitergegeben durch"
        , cSizeKB: "Größe (kB)"
        , cExpand: "Aufklappen"
        , cCollapse: "Zuklappen"
        , cNoData: "Keine Daten"
        , cCopyright: "Cardano Blockchain Explorer @2017"
        , cUnknown: "Unbekannt"
        , cTotalOutput: "Gesamtausgabe"
        , cOf: "von"
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
        , hrSubtitle: "Der effektivste Weg um das Netzwerk der Kryptowährung zu erforschen."
        , hrSearch: "Suche Addresse, Block, Token"
        }
    , dashboard:
        { dbLastBlocks: "Aktuelle Blöcke"
        , dbLastBlocksDescription: "Am {0} wurden {1} Transakationen generiert."
        , dbNetworkDifficulty: "Netzwerk difficulty"
        , dbNetworkDifficultyDescription: "Difficulty ist eine Maßeinheit die beschreibt, wieviel Aufwand es bedarf, um einen neuen Block unterhalb einer bestimmten Grenze zu finden"
        , dbPriceAverage: "Price (Durchschnitt)"
        , dbPriceForOne: "{0} für 1 {1}"
        , dbPriceSince: "{0} seid gestern."
        , dbTotalSupply: "Gesamtumsatz"
        , dbTotalAmountOf: "Anzahl von {0} im System."
        , dbTotalAmountOfTransactions: "Gesamtanzahl von erfassten Transaktionen im System seit Beginn an."
        , dbExploreBlocks: "Blöcke erkunden"
        , dbExploreTransactions: "Transaktionen erkunden"
        , dbBlockchainOffer: "Was bieten wir mit unserem Block Explorer"
        , dbBlockSearch: "Blockssuche"
        , dbBlockSearchDescription: "Block ist eine Box in der Transaktionen gespeichert werden."
        , dbAddressSearch: "Adresssuche"
        , dbAddressSearchDescription: "Adresssuche"
        , dbTransactionSearch: "Transaktionssuche"
        , dbTransactionSearchDescription: "Transaktion ist der Transfer von Münzem vom Benutzer 'A' zum Benutzer 'B'."
        , dbApiDescription: "Unsere robuste API ist in unterschiedlichen Sprachen und SDKs verfügbar."
        , dbGetAddress: "Addresse abfragen"
        , dbResponse: "Antwort"
        , dbCurl: "Curl"
        , dbNode: "Node"
        , dbJQuery: "jQuery"
        , dbGetApiKey: "API key anfordern"
        , dbMoreExamples: "Mehr Beispiele"
        , dbAboutBlockchain: "Über Blockchain"
        , dbAboutBlockchainDescription: "Mit der Blockchain API ist es einfach Anwendungen für Kryptowährung zu entwickeln. Wir sind bestrebt eine Plattform anzubieten, mit der Entwickler schnell skalierbare und sichere Services umsetzen können.<br/><br/>Diese API ist kostenlos and unbeschränkt nutzbar während der Beta Phase. Wir haben gerade gestartet und werden nach und nach mehr Endpunkte und Funktionen in den kommenden Wochen anbieten. Wir wollen die API anbieten, die Sie wirklich benötigen. Darum senden Sie uns bitte Wünsche und Verbesserungsvorschläge oder sagen Sie einfach nur 'Hallo'."
        }
    , address:
        { addScan: "Scannen Sie hier den QR Code, um die Adresse in die Zwischenablage zu kopieren."
        , addQrCode: "QR-Code"
        , addFinalBalance: "Aktueller Kontostand"
        }
    , tx:
        { txTime: "Eingangszeit"
        , txIncluded: "Bestand im Block"
        , txRelayed: "Weitergabe per IP"
        }
    , block:
        { blFees: "Gebühren"
        , blEstVolume: "Geschätztes Volumen"
        , blPrevBlock: "Vorheriger Block"
        , blNextBlock: "Nächster Block"
        , blRoot: "Oberer Block"
        }
    , footer:
        { fooRessources: "Ressourcen"
        , fooFollow: "Folgen Sie uns"
        , fooLinks: "Links"
        }
    }
