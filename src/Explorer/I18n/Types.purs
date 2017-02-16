module Explorer.I18n.Types where

-- Add all i18n types here to generate lenses from it

type Translation =
    { common :: Common
    , navigation :: Nav
    , hero :: Hero
    , dashboard :: Dashboard
    , address :: Address
    }

-- common translations

type Common =
    { cBack :: String
    , cTransaction :: String
    , cTransactions :: String
    , cTransactionFeed :: String
    , cCalculator :: String
    , cVersion :: String
    , cAddress :: String
    , cSummary :: String
    , cBlock :: String
    , cHashes :: String
    , cHeight :: String
    , cAge :: String
    , cTotalSent :: String
    , cRelayedBy :: String
    , cSizeKB :: String
    , cExpand :: String
    }

-- translations of main navigation

type Nav =
    { navHome :: String
    , navBlockchain :: String
    , navMarket :: String
    , navCharts :: String
    , navTools :: String
    }

-- translations of hero

type Hero =
    { hrTitle :: String
    , hrSubtitle :: String
    , hrSearch :: String
    }

-- translations of dashboard

type Dashboard =
    { dbLastBlocks :: String
    , dbExploreBlocks :: String
    }

-- translations of address detail page

type Address =
    { addScan :: String
    }
