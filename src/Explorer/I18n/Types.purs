module Explorer.I18n.Types where

-- Add all i18n types here to generate lenses from it

type Translation =
    { title :: String
    , subtitle :: String
    , back :: String
    , transaction :: String
    , transactions :: String
    , transactionFeed :: String
    , address :: String
    , calculator :: String
    , version :: String
    , summary :: String
    , block :: String
    , hashes :: String
    , nav :: NavTranslation
    , height :: String
    , age :: String
    , totalSent :: String
    , relayedBy :: String
    , sizeKB :: String
    }

type NavTranslation =
    { home :: String
    , blockchain :: String
    , market :: String
    , charts :: String
    , tools :: String
    }
